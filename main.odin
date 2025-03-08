package font_render

import "core:slice"
import "core:mem"
import "core:fmt"
import "core:time"
import "core:math"

import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

// TODO: have font own a copy of the ttf raw data

FONT_RENDER_SCRATCH_SPACE :: #config(FONT_RENDER_SCRATCH_SPACE, 2 * mem.Megabyte)

Font :: struct {
	info: ttf.fontinfo,
	sdf_cache: map[rune]Distance_Field,
	placeholder: Distance_Field,
	atlas: Glyph_Atlas,

	size: f32,
	edge_value: f32,
	dist_scale: f32,
	sharpness: f32,

	allocator: mem.Allocator,
}

Distance_Field :: struct {
	width:  i32,
	height: i32,
	offset: [2]i32,
	values: []u8,
	codepoint: rune,
}

Glyph_Atlas_Slot :: struct {
	width: i32,
	height: i32,
	atlas_offset: [2]i32,
	glyph_offset: [2]i32,
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

Glyph_Atlas :: struct {
	glyphs: map[rune]Glyph_Atlas_Slot,
	pixels: []u8,
	width: i32,
	height: i32,
}

PLACEHOLDER_RUNE :: 0

get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, err: mem.Allocator_Error) {
	context.allocator = font.allocator
	// Cache hit
	if sdf, cached := font.sdf_cache[codepoint]; cached {
		return sdf, nil
	}

	has_glyph := ttf.FindGlyphIndex(&font.info, codepoint) != 0
	if !has_glyph {
		field = font.placeholder
		return
	}

	field = render_sdf(font, codepoint) or_return
	font.sdf_cache[codepoint] = field

	return
}

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
	if values == nil { return }

	field.values = make([]u8, width * height, font.allocator) or_return
	mem.copy_non_overlapping(raw_data(field.values), values, int(width * height))

	field.codepoint = codepoint
	field.width     = i32(width)
	field.height    = i32(height)
	field.offset    = {i32(x_off), i32(y_off)}

	return
}

sigmoid_activate :: proc(v: f32, s: f32) -> f32 {
	return 1.0 / (1 + math.exp(-s * (v - 0.5)))
}

@(require_results)
render_bitmap_sdf :: proc(sdf: Distance_Field, sharpness: f32, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error){
	context.allocator = allocator
	glyph.pixels = make([]u8, sdf.width * sdf.height) or_return
	glyph.width  = sdf.width
	glyph.height = sdf.height
	glyph.glyph_offset = sdf.offset
	glyph.codepoint = sdf.codepoint

	for sv, i in sdf.values {
		v := f32(sv) / 0xff
		px := u8(clamp(0, 0xff * sigmoid_activate(v, sharpness), 255))
		glyph.pixels[i] = px
	}

	return
}

render_bitmap_rune :: proc(font: ^Font, codepoint: rune, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error){
	context.allocator = allocator
	sdf := get_rune_sdf(font, codepoint) or_return
	return render_bitmap_sdf(sdf, font.sharpness)
}

render_bitmap :: proc {
	render_bitmap_sdf,
	render_bitmap_rune,
}

DEFAULT_CACHE_CAPACITY :: 256

font_load :: proc(data: []byte, size: f32, allocator := context.allocator) -> (font: Font, ok: bool) {
	mem_err : mem.Allocator_Error
	bool(ttf.InitFont(&font.info, raw_data(data), 0)) or_return

	ok = true
	font.size = size
	font.edge_value = 0.63
	font.dist_scale = 0.61
	font.sharpness = 9.25
	font.allocator = allocator

	context.allocator = allocator
	defer if !ok {
		delete(font.sdf_cache)
		delete(font.placeholder.values)
	}

	font.sdf_cache, mem_err = make(map[rune]Distance_Field, DEFAULT_CACHE_CAPACITY)
	if mem_err != nil {
		ok = false
	}

	font.placeholder, mem_err = render_sdf(&font, -1, font.allocator)
	if mem_err != nil {
		ok = false
	}

	return
}

font_destroy :: proc(font: ^Font){
	context.allocator = font.allocator

	// NOTE: Because all placeholder runes share the same values, they are
	//       ignored and the deletion is deferred for later
	placeholder_values := font.placeholder.values
	defer delete(placeholder_values)

	for _, sdf in font.sdf_cache {
		if raw_data(sdf.values) == raw_data(placeholder_values) {
			continue
		}
		delete(sdf.values)
	}
	delete(font.sdf_cache)
}

@(require_results, private)
pack_atlas_rows :: proc(glyphs: []Glyph_Bitmap, max_width: i32) -> (i32, i32){
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

	return max_width + 2, total_height + 2
}

atlas_create :: proc(allocator := context.allocator, glyph_cap := DEFAULT_CACHE_CAPACITY) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error) {
	atlas.glyphs = make(map[rune]Glyph_Atlas_Slot, DEFAULT_CACHE_CAPACITY, allocator) or_return
	return
}

atlas_destroy :: proc(atlas: ^Glyph_Atlas){
	delete(atlas.glyphs)
	delete(atlas.pixels)
}

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

	atlas = atlas_create(font.allocator, len(font.sdf_cache) + 1) or_return
	// NOTE: The bitmap pixel rendering is deferred to when copying the bitmap to the atlas to avoid excessive memory usage.
	//       We do not need the pixels to pack them as the bounding boxes are defined by the signed distance fields.
	packed_bitmaps := make([dynamic]Glyph_Bitmap, 0, len(font.sdf_cache) + 1, context.temp_allocator) or_return

	total_sdf_width, total_sdf_count : i32

	for codepoint, sdf in font.sdf_cache {
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

	placeholder_bmap := render_bitmap(font.placeholder, font.sharpness, context.temp_allocator) or_return
	append(&packed_bitmaps, placeholder_bmap)

	desired_atlas_width := 1.5*math.sqrt(f64(len(packed_bitmaps))) * f64(total_sdf_width) / f64(total_sdf_count)

	aw, ah := pack_atlas_rows(packed_bitmaps[:], i32(desired_atlas_width))

	atlas.pixels = make([]u8, aw * ah) or_return
	atlas.width  = aw
	atlas.height = ah

	for _, i in packed_bitmaps {
		restore_point := temp_arena.offset
		defer temp_arena.offset = restore_point

		bmap := render_bitmap(font, packed_bitmaps[i].codepoint, context.temp_allocator) or_return
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

font_update_atlas :: proc(font: ^Font) -> (err: mem.Allocator_Error) {
	context.allocator = font.allocator
	atlas := font_generate_atlas(font, nil) or_return
	atlas_destroy(&font.atlas)
	font.atlas = atlas
	return
}

import "core:unicode/utf8"

Glyph_Rect :: struct {
	draw_offset: [2]i32,
	atlas_offset: [2]i32,
	width: i32,
	height: i32,
	codepoint: rune,
}

Box :: struct {
	using pos: [2]i32,
	width, height: i32,
}

render_text :: proc(font: ^Font, text: string) -> (positions: []Glyph_Rect, bounds: Box) {
	runes := utf8.rune_count(text)
	rects := make([dynamic]Glyph_Rect, 0, runes)

	scale := ttf.ScaleForPixelHeight(&font.info, font.size)

	x_offset, line_offset : i32
	prev_width : i32 = 0

	for char, i in text {
		advance, lsb : i32
		ttf.GetCodepointHMetrics(&font.info, char, &advance, &lsb)
		advance = i32(f32(advance) * scale)
		lsb = i32(f32(lsb) * scale)

		slot, ok := font.atlas.glyphs[char]
		if !ok {
			slot = font.atlas.glyphs[PLACEHOLDER_RUNE]
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
		y_offset := slot.glyph_offset.y + line_offset

		append(&rects, rect)
	}

	x0, x1 := max(i32), min(i32)
	y0, y1 := max(i32), min(i32)
	for rect in rects {
		fmt.println(rect.codepoint, rect.draw_offset)
		x0 = min(x0, rect.draw_offset.x)
		x1 = max(x1, rect.draw_offset.x + rect.width)

		y0 = min(y0, rect.draw_offset.y)
		y1 = max(y1, rect.draw_offset.y + rect.height)
	}

	bounds.width  = x1 - x0
	bounds.height = y1 - y0
	bounds.pos    = {x0, y0}
	assert(len(rects) == cap(rects))
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

FONT :: #load("noto.ttf", []byte)

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


main :: proc(){
	rl.InitWindow(1200, 800, "font render")
    rl.SetWindowState({.WINDOW_RESIZABLE})
	rl.SetTargetFPS(60)

	font, ok := font_load(FONT, 54)
	defer font_destroy(&font)
	font.edge_value = 0.52
	// font.dist_scale = 1.2
	// font.sharpness = 7.0
	// font.sharpness = 12.1
	ensure(ok, "Failed to laod font")

	{
		beg := time.now()
		/* Latin1   */ for r in 0x0000..<0x00ff { get_rune_sdf(&font, rune(r)) }
		// /* Latin+   */ for r in 0x0180..<0x02af { get_rune_sdf(&font, rune(r)) }
		// /* Greek    */ for r in 0x0370..<0x03ff { get_rune_sdf(&font, rune(r)) }
		// /* Cyrillic */ for r in 0x0400..<0x04ff { get_rune_sdf(&font, rune(r)) }
		// /* Armenian */ for r in 0x0530..<0x058f { get_rune_sdf(&font, rune(r)) }
		// /* Math     */ for r in 0x2200..<0x22ff { get_rune_sdf(&font, rune(r)) }
		// /* Math+    */ for r in 0x2a00..<0x2aff { get_rune_sdf(&font, rune(r)) }
		fmt.println("Fresh SDF:", time.since(beg))
	}

	beg := time.now()
	font_update_atlas(&font)
	fmt.println("Atlas generation:", time.since(beg))

	tex := as_texture(font.atlas)
	fmt.println("Glyphs loaded:", len(font.atlas.glyphs))
	fmt.println("Glyphs cap:", cap(font.sdf_cache))

	rects, box := render_text(&font, "The quick brown fox jumped over the lazy dog")
	// rects := render_text(&font, "AAA AAA")

	fmt.println(box)
	show_boxes := true
	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		
		if rl.IsKeyPressed(.B){
			show_boxes = !show_boxes
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
				rl.DrawRectangleLinesEx(rl_rect, 1, rl.YELLOW)
				rl.DrawCircle(i32(pos.x), i32(pos.y), 2, rl.MAGENTA)
			}
		}
		if show_boxes {
			rl.DrawRectangleLines(i32(mouse_pos.x), i32(mouse_pos.y), box.width, box.height, rl.RED)
			rl.DrawCircle(i32(mouse_pos.x), i32(mouse_pos.y), 2, rl.GREEN)
		}
		// rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xdc, 0xdc, 0xdc, 0xff})
		// draw_atlas_grid(atlas, {10, 10})
		rl.EndDrawing()
	}
}
