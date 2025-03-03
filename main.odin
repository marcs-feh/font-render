package font_render

import "core:slice"
import "core:mem"
import "core:container/lru"
import "core:fmt"
import "core:time"
import "core:math"
import "set"

import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

// TODO: have font own a copy of the ttf raw data
// TODO: replace font map with sorted list + binary search?

Font :: struct {
	info: ttf.fontinfo,
	sdf_cache: map[rune]Distance_Field,
	placeholder: Distance_Field,
	allocator: mem.Allocator,

	size: f32,
	edge_value: f32,
	dist_scale: f32,
	sharpness: f32,
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

PLACEHOLDER_RUNE :: -1

get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, err: mem.Allocator_Error) {
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
render_bitmap :: proc(sdf: Distance_Field, sharpness: f32, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error){
	context.allocator = allocator
	glyph.pixels = make([]u8, sdf.width * sdf.height) or_return
	glyph.width  = sdf.width
	glyph.height = sdf.height
	glyph.glyph_offset = {auto_cast sdf.offset.x, auto_cast  sdf.offset.y}
	glyph.codepoint = sdf.codepoint

	for sv, i in sdf.values {
		v := f32(sv) / 0xff
		px := u8(clamp(0, 0xff * sigmoid_activate(v, sharpness), 255))
		glyph.pixels[i] = px
	}

	return
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

	for codepoint, sdf in font.sdf_cache {
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

atlas_create :: proc(glyph_cap := DEFAULT_CACHE_CAPACITY) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error) {
	atlas.glyphs = make(map[rune]Glyph_Atlas_Slot, DEFAULT_CACHE_CAPACITY) or_return
	return
}

atlas_destroy :: proc(atlas: ^Glyph_Atlas){
	delete(atlas.glyphs)
	delete(atlas.pixels)
}

RECOMMENDED_ATLAS_TEMP_SIZE :: 1 * mem.Megabyte

font_generate_atlas :: proc(font: ^Font, temp_arena: ^mem.Arena, allocator := context.allocator) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error){
	arena_restore := mem.begin_arena_temp_memory(temp_arena)
	defer mem.end_arena_temp_memory(arena_restore)
	defer fmt.println("Peak usage:", temp_arena.peak_used / mem.Kilobyte, "KiB")

	mem_remaining := len(temp_arena.data) - temp_arena.offset
	assert(mem_remaining >= RECOMMENDED_ATLAS_TEMP_SIZE, "Arena is smaller than the recommended size to generate a font atlas")
	context.temp_allocator = mem.arena_allocator(temp_arena)

	atlas = atlas_create(len(font.sdf_cache) + 1) or_return
	bitmaps := make([dynamic]Glyph_Bitmap, 0, len(font.sdf_cache) + 1, context.temp_allocator) or_return

	// Add placeholder so the atlas always has a valid bitmap even if it does not have a rune mapped
	placeholder_bmap := render_bitmap(font.placeholder, font.sharpness, context.temp_allocator) or_return
	append(&bitmaps, placeholder_bmap)

	total_sdf_width, total_sdf_count : i32

	for codepoint, sdf in font.sdf_cache {
		if raw_data(sdf.values) == raw_data(font.placeholder.values){
			// Ignore unsupported chars
			continue
		}
		bmap := render_bitmap(sdf, font.sharpness, context.temp_allocator) or_return
		append(&bitmaps, bmap)
		total_sdf_width += bmap.width
		total_sdf_count += 1
	}

	desired_atlas_width := 1.5 * math.sqrt(f64(len(bitmaps))) * f64(total_sdf_width) / f64(total_sdf_count)

	aw, ah := pack_atlas_rows(bitmaps[:], i32(desired_atlas_width))

	atlas.pixels = make([]u8, aw * ah) or_return
	atlas.width  = aw
	atlas.height = ah

	for bmap in bitmaps {
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

draw_atlas_grid :: proc(atlas: Glyph_Atlas, pos: [2]i32){
	rl.DrawRectangleLines(i32(pos.x), i32(pos.y), i32(atlas.width), i32(atlas.height), rl.RED)

	for _, glyph in atlas.glyphs {
		off := glyph.atlas_offset

		rl.DrawRectangleLines(i32(glyph.atlas_offset.x + pos.x), i32(glyph.atlas_offset.y + pos.y), i32(glyph.width), i32(glyph.height),
			{0x30, 0xfe, 0xfe, 0xff})
	}
}

as_texture :: proc(atlas: Glyph_Atlas) -> rl.Texture {
	img := rl.Image {
		data = raw_data(atlas.pixels),
		width = i32(atlas.width),
		height = i32(atlas.height),
		mipmaps = 1,
		format = .UNCOMPRESSED_GRAYSCALE,
	}

	return rl.LoadTextureFromImage(img)
}

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

FONT :: #load("jetbrains.ttf", []byte)

main :: proc(){
	rl.InitWindow(1200, 800, "font render")
    rl.SetWindowState({.WINDOW_RESIZABLE})
	rl.SetTargetFPS(60)

	font, ok := font_load(FONT, 32)
	defer font_destroy(&font)
	font.edge_value = 0.52
	font.sharpness = 12.1
	ensure(ok, "Failed to laod font")


	letter : Distance_Field
	{
		beg := time.now()
		for r in 0..<max(u16) {
			get_rune_sdf(&font, rune(r))
		}
		fmt.println("Fresh SDF:", time.since(beg))
	}
	{
		beg := time.now()
		for r in 0..<max(u16) {
			get_rune_sdf(&font, rune(r))
		}
		fmt.println("Cached SDF:", time.since(beg))
	}
	sdftex := sdf_texture(letter)

	atlas_arena : mem.Arena
	{
		@static atlas_arena_mem : [128 * mem.Megabyte]byte
		mem.arena_init(&atlas_arena, atlas_arena_mem[:])
	}

	atlas, _ := font_generate_atlas(&font, &atlas_arena)
	tex := as_texture(atlas)

	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		// rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xff, 0xd5, 0x2e, 0xff})
		rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xdc, 0xdc, 0xdc, 0xff})
		draw_atlas_grid(atlas, {10, 10})
		rl.EndDrawing()
	}
}
