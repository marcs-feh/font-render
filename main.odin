package font_render

import "core:slice"
import "core:mem"
import "core:fmt"
import "core:time"
import "core:math"
import "core:unicode/utf8"

import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

FONT_RENDER_SCRATCH_SPACE :: #config(FONT_RENDER_SCRATCH_SPACE, 1 * mem.Megabyte)

FONT_RENDER_DEFAULT_CACHE_CAPACITY :: #config(FONT_RENDER_DEFAULT_CACHE_CAPACITY, 256)

// Container for a specific TTF font
Font :: struct {
	info: ttf.fontinfo,
	sdf_store: map[rune]Distance_Field,
	placeholder: Distance_Field,
	atlas: Glyph_Atlas,

	raster_passes: []Raster_Pass,

	size: f32,
	edge_value: f32,
	dist_scale: f32,

	allocator: mem.Allocator,
}

Raster_Pass :: #type proc(field: Distance_Field) -> Distance_Field

// A signed distance field for a codepoint, the higher the value the "more inside" it is inside the glyph's contour
Distance_Field :: struct {
	width:  i32,
	height: i32,
	offset: [2]i32,
	values: []u8,
	codepoint: rune,
}

// Slot to be put in a glyph atlas. `glyph_offset` is the offset from the glyph's origin.
Glyph_Atlas_Slot :: struct {
	atlas_offset: [2]i32,
	glyph_offset: [2]i32,
	width: i32,
	height: i32,
}

// Rendered version of a glyph, note that `atlas_offset` is only used to pack the glyph
Glyph_Bitmap :: struct {
	codepoint: rune,
	atlas_offset: [2]i32,
	glyph_offset: [2]i32,
	width: i32,
	height: i32,
	pixels: []u8,
}

// A large bitmap that contains a set of glyphs and their offsets into the atlas
Glyph_Atlas :: struct {
	glyphs: map[rune]Glyph_Atlas_Slot,
	pixels: []u8,
	width: i32,
	height: i32,
}

// Rune to be rendered when a codepoint cannot be found
PLACEHOLDER_RUNE :: -1

// Get a codepoint's field present in the font's sdf_cache, if it is not present a new one will be allocated.
get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, err: mem.Allocator_Error) {
	context.allocator = font.allocator
	// Cache hit
	if sdf, cached := font.sdf_store[codepoint]; cached {
		return sdf, nil
	}

	has_glyph := ttf.FindGlyphIndex(&font.info, codepoint) != 0
	if !has_glyph {
		field = font.placeholder
		return
	}

	field = render_sdf(font, codepoint, font.allocator) or_return
	font.sdf_store[codepoint] = field

	return
}

// Allocate a signed distance field for a codepoint of font, this does not do caching.
@(require_results)
render_sdf :: proc(font: ^Font, codepoint: rune, allocator := context.allocator) -> (field: Distance_Field, err: mem.Allocator_Error) {
	context.allocator = allocator
	on_edge    := u8(font.edge_value * 0xff)
	dist_scale := font.dist_scale * 0xff

	scale := ttf.ScaleForPixelHeight(&font.info, font.size)

	width, height, x_off, y_off : i32
	PADDING :: 4

	values := ttf.GetCodepointSDF(
		&font.info, scale, i32(codepoint),
		PADDING, on_edge, dist_scale,
		&width, &height, &x_off, &y_off
	)
	defer ttf.FreeSDF(values, nil)
	if values == nil {
		return {}, .Out_Of_Memory
	}

	field.values = make([]u8, width * height, font.allocator) or_return
	mem.copy_non_overlapping(raw_data(field.values), values, int(width * height))

	field.codepoint = codepoint
	field.width     = i32(width)
	field.height    = i32(height)
	field.offset    = {i32(x_off), i32(y_off)}

	return
}

// Render a new bitmap from an signed distance field
@(require_results)
render_bitmap_sdf :: proc(sdf: Distance_Field, passes: []Raster_Pass, arena: ^mem.Arena, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error){
	region := mem.begin_arena_temp_memory(arena)
	defer mem.end_arena_temp_memory(region)

	context.allocator = allocator
	context.temp_allocator = mem.arena_allocator(arena)

	values_copy := slice.clone(sdf.values, context.temp_allocator) or_return
	field := sdf
	field.values = values_copy

	glyph.pixels       = make([]u8, field.width * field.height) or_return
	glyph.width        = field.width
	glyph.height       = field.height
	glyph.glyph_offset = field.offset
	glyph.codepoint    = field.codepoint

	for pass in passes {
		field = pass(field)
	}

	copy(glyph.pixels, field.values)


	return
}

sigmoid_pass :: proc(field: Distance_Field) -> Distance_Field {
	sharpness :: 3.0

	sigmoid_activate :: proc(v: f32, s: f32) -> f32 {
		return 1.0 / (1 + math.exp(-s * (v - 0.5)))
	}

	for sv, i in field.values {
		v := f32(sv) / 0xff
		px := u8(clamp(0, 0xff * sigmoid_activate(v, sharpness), 255))
		field.values[i] = px
	}

	return field
}

// Render a new bitmap for a codepoint from font
render_bitmap_rune :: proc(font: ^Font, codepoint: rune, arena: ^mem.Arena, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error){
	context.allocator = allocator
	sdf := get_rune_sdf(font, codepoint) or_return
	return render_bitmap_sdf(sdf, font.raster_passes, arena)
}

render_bitmap :: proc {
	render_bitmap_sdf,
	render_bitmap_rune,
}

// Load a font from a TTF data set
font_load :: proc(data: []byte, size: f32, allocator := context.allocator) -> (font: Font, ok: bool) {
	mem_err : mem.Allocator_Error
	bool(ttf.InitFont(&font.info, raw_data(data), 0)) or_return

	ok = true
	font.size = size
	font.edge_value = 0.51
	font.dist_scale = 0.5
	font.allocator = allocator

	context.allocator = allocator
	defer if !ok {
		delete(font.sdf_store)
		delete(font.placeholder.values)
	}

	font.sdf_store, mem_err = make(map[rune]Distance_Field, FONT_RENDER_DEFAULT_CACHE_CAPACITY)
	if mem_err != nil {
		ok = false
	}

	font.placeholder, mem_err = render_sdf(&font, -1, font.allocator)
	if mem_err != nil {
		ok = false
	}

	return
}

// Deallocate a font object
font_destroy :: proc(font: ^Font){
	context.allocator = font.allocator

	// NOTE: Because all placeholder runes share the same values, they are
	//       ignored and the deletion is deferred for later
	placeholder_values := font.placeholder.values
	defer delete(placeholder_values)

	for _, sdf in font.sdf_store {
		if raw_data(sdf.values) == raw_data(placeholder_values) {
			continue
		}
		delete(sdf.values)
	}
	delete(font.sdf_store)
}


@(require_results, private)
pack_atlas_rows :: proc(glyphs: []Glyph_Bitmap, max_width: i32) -> (width, height: i32) {
	cur_x, cur_y, max_height, total_height : i32

	slice.sort_by_cmp(glyphs[:], proc(a, b: Glyph_Bitmap) -> slice.Ordering {
		ord := slice.Ordering(clamp(-1, b.height - a.height, +1))
		if ord == .Equal {
			ord = slice.Ordering(clamp(-1, b.width - a.width, +1))
		}
		return ord
	})

	for rect, i in glyphs {
		if cur_x + rect.width > max_width {
			cur_y += max_height
			total_height += max_height
			cur_x = 0
			max_height = 0
		}

		glyphs[i].atlas_offset = {cur_x, cur_y}
		cur_x += rect.width
		max_height = max(max_height, rect.height)
	}

	total_height += max_height

	return max_width, total_height
}

// Create an empty font atlas with a preallocated capacity
atlas_create :: proc(allocator := context.allocator, glyph_cap := FONT_RENDER_DEFAULT_CACHE_CAPACITY) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error) {
	atlas.glyphs = make(map[rune]Glyph_Atlas_Slot, FONT_RENDER_DEFAULT_CACHE_CAPACITY, allocator) or_return
	return
}

// Destroy a font atlas (uses `context.allocator` for deleting pixels)
atlas_destroy :: proc(atlas: ^Glyph_Atlas){
	delete(atlas.glyphs)
	delete(atlas.pixels)
}

// Generate a font atlas with the glyphs currently present in the font's sdf store. An arena is used as scratch space,
// if it is not provided then the default from the package is used.
font_generate_atlas :: proc(font: ^Font, temp_arena: ^mem.Arena = nil) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error){
	context.allocator = font.allocator
	temp_arena := temp_arena if temp_arena != nil else &DEFAULT_TEMP_ARENA

	arena_restore := mem.begin_arena_temp_memory(temp_arena)
	defer mem.end_arena_temp_memory(arena_restore)

	defer fmt.println("Peak usage:", temp_arena.peak_used / mem.Kilobyte, "KiB")

	when ODIN_DEBUG {
		mem_remaining := len(temp_arena.data) - temp_arena.offset
		ensure(mem_remaining >= FONT_RENDER_SCRATCH_SPACE, "Arena is smaller than the recommended size to generate a font atlas")
	}
	context.temp_allocator = mem.arena_allocator(temp_arena)

	atlas = atlas_create(font.allocator, len(font.sdf_store) + 1) or_return
	// NOTE: The bitmap pixel rendering is deferred to when copying the bitmap to the atlas to avoid excessive memory usage.
	//       We do not need the pixels to pack them as the bounding boxes are defined by the signed distance fields.
	packed_bitmaps := make([dynamic]Glyph_Bitmap, 0, len(font.sdf_store) + 1, context.temp_allocator) or_return

	total_sdf_width, total_sdf_count : i32

	for codepoint, sdf in font.sdf_store {
		if raw_data(sdf.values) == raw_data(font.placeholder.values){
			continue
		}
		bmap := Glyph_Bitmap{
			glyph_offset = sdf.offset,
			width = sdf.width,
			height = sdf.height,
			codepoint = codepoint,
		}
		append(&packed_bitmaps, bmap)
		total_sdf_width += bmap.width
		total_sdf_count += 1
	}

	placeholder_bmap := render_bitmap_sdf(font.placeholder, font.raster_passes, temp_arena, context.temp_allocator) or_return
	append(&packed_bitmaps, placeholder_bmap)

	desired_atlas_width := 1.5*math.sqrt(f64(len(packed_bitmaps))) * f64(total_sdf_width) / f64(total_sdf_count)

	aw, ah := pack_atlas_rows(packed_bitmaps[:], i32(desired_atlas_width))

	atlas.pixels = make([]u8, aw * ah) or_return
	atlas.width  = aw
	atlas.height = ah

	for _, i in packed_bitmaps {
		loop_region := mem.begin_arena_temp_memory(temp_arena)
		defer mem.end_arena_temp_memory(loop_region)

		bmap := render_bitmap_rune(font, packed_bitmaps[i].codepoint, temp_arena, context.temp_allocator) or_return
		bmap.atlas_offset = packed_bitmaps[i].atlas_offset

		slot := Glyph_Atlas_Slot{
			atlas_offset = bmap.atlas_offset,
			glyph_offset = bmap.glyph_offset,
			width = bmap.width,
			height = bmap.height,
		}

		atlas.glyphs[bmap.codepoint] = slot

		// Copy bitmap into apropriate slot
		for y in 0..<slot.height {
			for x in 0..<slot.width {
				apos := bmap.atlas_offset + [2]i32{x, y}
				atlas.pixels[atlas.width * apos.y + apos.x] = bmap.pixels[bmap.width * y + x]
			}
		}
	}

	return
}

// Update the font's atlas, deleting the previous one
font_update_atlas :: proc(font: ^Font) -> (err: mem.Allocator_Error) {
	context.allocator = font.allocator
	atlas := font_generate_atlas(font, nil) or_return
	atlas_destroy(&font.atlas)
	font.atlas = atlas
	return
}

// Rectangle corresponding to an atlas glyph with ajusted offsets to be used in rendering
Glyph_Rect :: struct {
	draw_offset: [2]i32,
	atlas_offset: [2]i32,
	width: i32,
	height: i32,
	codepoint: rune,
}

// Rectangle, used for bounds measuring
Box :: struct {
	using pos: [2]i32,
	width, height: i32,
}

render_text :: proc(font: ^Font, text: string, line_height: i32 = -1) -> (positions: []Glyph_Rect, bounds: Box) {
	runes := utf8.rune_count(text)
	rects := make([dynamic]Glyph_Rect, 0, runes)
	line_height := i32(font.size) if line_height < 0 else line_height

	scale := ttf.ScaleForPixelHeight(&font.info, font.size)

	x_offset, line_offset : i32

	for char, i in text {
		advance, lsb : i32
		ttf.GetCodepointHMetrics(&font.info, char, &advance, &lsb)
		advance = i32(f32(advance) * scale)
		lsb     = i32(f32(lsb) * scale)

		slot, ok := font.atlas.glyphs[char]
		if !ok {
			slot = font.atlas.glyphs[PLACEHOLDER_RUNE]
		}

		if char == '\n' {
			x_offset = 0
			line_offset += line_height
			continue
		}
		else if char == ' ' {
			slot.height = 0
		}

		// Add kerning
		next_i := min(len(text) - 1, i + 1)
		kern_adv := f32(ttf.GetGlyphKernAdvance(&font.info, i32(char), i32(text[next_i]))) * scale
		x_offset += i32(math.round(kern_adv))
		
		rect := Glyph_Rect {
			codepoint = char,
			atlas_offset = slot.atlas_offset,
			draw_offset = [2]i32{slot.glyph_offset.x + x_offset, slot.glyph_offset.y + line_offset},
			width = slot.width,
			height = slot.height,
		}

  		x_offset += advance + lsb

		append(&rects, rect)
	}

	// Bounding box
	x0, x1 := max(i32), min(i32)
	y0, y1 := max(i32), min(i32)
	for rect in rects {
		x0 = min(x0, rect.draw_offset.x)
		x1 = max(x1, rect.draw_offset.x + rect.width)

		y0 = min(y0, rect.draw_offset.y)
		y1 = max(y1, rect.draw_offset.y + rect.height)
	}

	bounds.width  = x1 - x0
	bounds.height = y1 - y0
	bounds.pos    = {x0, y0}

	return rects[:], bounds
}

@private DEFAULT_TEMP_ARENA_MEMORY : [FONT_RENDER_SCRATCH_SPACE]byte

@private DEFAULT_TEMP_ARENA : mem.Arena

@(init)
init_scratch_space :: proc(){
	@static initialized := false
	ensure(!initialized, "Font rendering module has already been i nitialized")
	mem.arena_init(&DEFAULT_TEMP_ARENA, DEFAULT_TEMP_ARENA_MEMORY[:])
	initialized = true
}

FONT :: #load("jetbrains.ttf", []byte)

sdf_texture :: proc(f: Distance_Field) -> rl.Texture {
	img := rl.Image{
		data = raw_data(f.values),
		width = f.width,
		height = f.height,
		mipmaps = 1,
		format = .UNCOMPRESSED_GRAYSCALE,
	}

	return rl.LoadTextureFromImage(img)
}

draw_atlas_grid :: proc(atlas: Glyph_Atlas, pos: [2]i32){
	rl.DrawRectangleLines(i32(pos.x), i32(pos.y), i32(atlas.width), i32(atlas.height), rl.RED)

	for _, glyph in atlas.glyphs {
		rl.DrawRectangleLines(i32(glyph.atlas_offset.x + pos.x), i32(glyph.atlas_offset.y + pos.y), i32(glyph.width), i32(glyph.height),
			{0x30, 0xfe, 0xfe, 0xff})
	}
}

as_texture :: proc(atlas: Glyph_Atlas) -> rl.Texture {
	GSAlpha :: [2]u8

	bitmap := make([]GSAlpha, len(atlas.pixels))
	defer delete(bitmap)

	for val, i in atlas.pixels {
		bitmap[i] = {0xff, val}
	}

	img := rl.Image {
		data = raw_data(bitmap),
		width = i32(atlas.width),
		height = i32(atlas.height),
		mipmaps = 1,
		format = .UNCOMPRESSED_GRAY_ALPHA,
	}

	return rl.LoadTextureFromImage(img)
}

font_refresh_sdf_store :: proc(font: ^Font) -> (err: mem.Allocator_Error){
	context.allocator = font.allocator
	for codepoint, field in font.sdf_store {
		delete(field.values)
		font.sdf_store[codepoint] = render_sdf(font, codepoint) or_return
	}
	return
}

main :: proc(){
	rl.InitWindow(1200, 600, "font render")
    rl.SetWindowState({.WINDOW_RESIZABLE})
	rl.SetTargetFPS(60)

	load_base_chars :: proc(font: ^Font){
		/* Latin1   */ for r in 0x0000..<0x00ff { get_rune_sdf(font, rune(r)) }
		// /* Latin+   */ for r in 0x0180..<0x02af { get_rune_sdf(font, rune(r)) }
		// /* Greek    */ for r in 0x0370..<0x03ff { get_rune_sdf(font, rune(r)) }
		// /* Math     */ for r in 0x2200..<0x22ff { get_rune_sdf(font, rune(r)) }
		// /* Math+    */ for r in 0x2a00..<0x2aff { get_rune_sdf(font, rune(r)) }
	}

	font, ok := font_load(FONT, 16)
	defer font_destroy(&font)
	font.edge_value = 0.51
	font.dist_scale = 0.62
	font.raster_passes = {sigmoid_pass}
	ensure(ok, "Failed to load font")

	load_base_chars(&font)
	font_update_atlas(&font)

	beg := time.now()
	font_update_atlas(&font)
	fmt.println("Atlas generation:", time.since(beg))

	tex := as_texture(font.atlas)
	fmt.println("Glyphs loaded:", len(font.atlas.glyphs))
	fmt.println("Glyphs cap:", cap(font.sdf_store))

	rects, box := render_text(&font,
`// The quick brown fox jumped
// over the lazy dog...
// damn!

#include <stdio.h>

int main(){
    printf("H∈llo, ωorld\n");
    return 0;
}
`
)
	// rects := render_text(&font, "AAA AAA")

	fmt.println(box)
	show_boxes := true

	font_changed := false

	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		
		if rl.IsKeyPressed(.B){
			show_boxes = !show_boxes
		}

		switch {
		case rl.IsKeyPressed(.J): font.edge_value -= 0.05; font_changed = true
		case rl.IsKeyPressed(.K): font.edge_value += 0.05; font_changed = true
		case rl.IsKeyPressed(.N): font.dist_scale -= 0.05; font_changed = true
		case rl.IsKeyPressed(.M): font.dist_scale += 0.05; font_changed = true
		}


		if font_changed {
			font_refresh_sdf_store(&font)
			font_update_atlas(&font)

			rl.UnloadTexture(tex)
			tex = as_texture(font.atlas)
			font_changed = false

			fmt.println("--------------")
			fmt.println("Edge Value:", font.edge_value)
			fmt.println("Dist Scale:", font.dist_scale)
			fmt.println("--------------")
		}

		mouse_pos := rl.GetMousePosition()

		for rect in rects {
			texture_piece := rl.Rectangle{
				x = f32(rect.atlas_offset.x),
				y = f32(rect.atlas_offset.y),
				width = f32(rect.width),
				height = f32(rect.height),
			}

			draw_offset := rl.Vector2{f32(rect.draw_offset.x - box.x), f32(rect.draw_offset.y - box.y)}
			pos := mouse_pos + draw_offset

			rl.DrawTextureRec(tex, texture_piece, pos, {0xff, 0xff, 0xff, 0xff})

			rl_rect := rl.Rectangle{
				x = pos.x,
				y = pos.y,
				width = f32(rect.width),
				height = f32(rect.height),
			}

			if show_boxes {
				rl.DrawRectangleLinesEx(rl_rect, 1, {0xee, 0xee, 0x30, 0x7f})
				rl.DrawCircle(i32(pos.x), i32(pos.y), 1.5, {0xee, 0x30, 0xee, 0xff})
			}
		}
		if show_boxes {
			rl.DrawRectangleLines(i32(mouse_pos.x), i32(mouse_pos.y), box.width, box.height, {0x30, 0xee, 0xee, 0x7f})
			rl.DrawCircle(i32(mouse_pos.x), i32(mouse_pos.y), 2, {0x30, 0xee, 0x30, 0xff})
		}
		// rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xdc, 0xdc, 0xdc, 0xff})
		// draw_atlas_grid(font.atlas, {10, 10})
		rl.EndDrawing()
	}
}
