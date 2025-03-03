package font_render

import "core:slice"
import "core:mem"
import "core:fmt"
import "core:time"
import "core:math"
import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

// TODO: have font own a copy of the ttf raw data
// TODO: replace font map with sorted list + binary search?

Font :: struct {
	info: ttf.fontinfo,
	sdf_cache: map[rune]Distance_Field,
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

AtlasSlot :: struct {
	width: int,
	height: int,
	atlas_offset: [2]int,
	glyph_offset: [2]int,
}

// Used only to pack glyphs
@private
RawGlyph :: struct {
	codepoint: rune,
	atlas_offset: [2]i32,
	glyph_offset: [2]i32,
	width: i32,
	height: i32,
	pixels: []u8,
}

GlyphAtlas :: struct {
	base_glyph: rune,
	glyphs: map[rune]AtlasSlot,
	font: ^Font,

	pixels: []u8,
	width: int,
	height: int,
}

// Get rune signed distance field from font, if the rune is not supported by
// font `has_glyph` will be false and the placeholder SDF will be returned.
get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, has_glyph: bool, err: mem.Allocator_Error) {
	PLACEHOLDER_RUNE :: -1
	has_glyph = ttf.FindGlyphIndex(&font.info, codepoint) > 0

	// Cache hit
	if sdf, cached := font.sdf_cache[codepoint]; cached {
		return sdf, has_glyph, nil
	}

	// Replace unsupported codepoints with the placeholder to avoid a bunch of redundant copies to the same "Not found" character
	if codepoint != PLACEHOLDER_RUNE && !has_glyph {
		field, _ = get_rune_sdf(font, PLACEHOLDER_RUNE) or_return
		field.codepoint = codepoint
		font.sdf_cache[codepoint] = field
		return
	}

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

// Push a temporary render of a distance field to arena
render_temp_bitmap :: proc(sdf: Distance_Field, sharpness: f32, arena: ^mem.Arena) -> (glyph: RawGlyph, err: mem.Allocator_Error){
    context.allocator = mem.arena_allocator(arena)

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

font_load :: proc(data: []byte, size: f32, allocator := context.allocator) -> (font: Font, ok: bool) {
	mem_err : mem.Allocator_Error
	bool(ttf.InitFont(&font.info, raw_data(data), 0)) or_return

	font.size = size
	font.edge_value = 0.63
	font.dist_scale = 0.61
	font.sharpness = 9.25
	font.allocator = allocator

	ok = true
	return
}

font_pack_atlas :: proc(font: ^Font, arena: ^mem.Arena) -> (atlas: GlyphAtlas, err: mem.Allocator_Error){
	arena_restore := mem.begin_arena_temp_memory(arena)
	defer mem.end_arena_temp_memory(arena_restore)
	context.temp_allocator = mem.arena_allocator(arena)

	glyphs := make([dynamic]RawGlyph, 0, len(font.sdf_cache) + 1, context.temp_allocator) or_return
	total_width : i32
	valid_slots : i32

	return
}

font_destroy :: proc(font: ^Font){
	context.allocator = font.allocator

	for codepoint, sdf in font.sdf_cache {
		delete(sdf.values)
	}
	delete(font.sdf_cache)
}

atlas_create :: proc(font: ^Font) -> (atlas: GlyphAtlas, err: mem.Allocator_Error) {
	atlas.glyphs = make(map[rune]AtlasSlot, 256) or_return
	atlas.font = font
	return
}

@(require_results, private)
pack_atlas_rows :: proc(glyphs: []RawGlyph, max_width: i32) -> (int, int){
	cur_x, cur_y, max_height, total_height : i32

	slice.sort_by_cmp(glyphs[:], proc(a, b: RawGlyph) -> slice.Ordering {
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

	return int(max_width + 2), int(total_height + 2)
}

// atlas_create :: proc(font: ^Font, base: rune, glyph_count: int, temp_reserve := 32 * mem.Megabyte) -> (atlas: GlyphAtlas, err: mem.Allocator_Error){
// 	// Setup arena for temporary bitmap allocations
//     arena_mem := make([]byte, temp_reserve) or_return
// 	defer delete(arena_mem)
//     arena : mem.Arena
//     mem.arena_init(&arena, arena_mem)
// 	context.temp_allocator = mem.arena_allocator(&arena)
//
// 	raw_glyphs := make([dynamic]RawGlyph, 0, glyph_count + 1, context.temp_allocator) or_return
//
//     total_width : int
// 	non_zero_slots : int = 1
// 	// Add placeholder rune
// 	{
// 		placeholder_slot := get_rune_bitmap(font, 0, &arena) or_return
// 		placeholder_slot.codepoint = -1
// 		append(&raw_glyphs, placeholder_slot)
// 		total_width += placeholder_slot.width
// 	}
//
// 	start := time.now()
// 	// Render slots bitmaps
// 	for off in 0..<rune(glyph_count) {
// 		slot : RawGlyph
// 		err : mem.Allocator_Error
//
// 		r := base + off
// 		if idx := ttf.FindGlyphIndex(&font.info, r); idx != 0 {
// 			slot, err = get_rune_bitmap(font, base + off, &arena)
// 			if err != nil { continue }
// 			total_width += int(slot.width)
// 			non_zero_slots += 1
// 		}
// 		append(&raw_glyphs, slot)
// 	}
// 	fmt.println("Rendering Peak:", arena.peak_used / mem.Kilobyte, "KiB", "Took:", time.since(start))
//
// 	assert(len(raw_glyphs) == glyph_count + 1, "Did not create all glyphs, maybe reserved space is too small?")
//
// 	// Initialize atlas
// 	{
// 		atlas.glyphs = make([]AtlasSlot, len(raw_glyphs)) or_return
// 		atlas.base_glyph = base
// 		atlas.font = font
//
// 		atlas_width := f64(total_width / non_zero_slots) * math.sqrt(f64(non_zero_slots)) * 1.5
// 		aw, ah := pack_atlas_rows(raw_glyphs[:], int(max(16, atlas_width)))
//
// 		atlas.pixels = make([]u8, aw * ah) or_return
// 		atlas.width = aw
// 		atlas.height = ah
// 	}
//
// 	// Transfer temporary packed slots into final atlas bitmap
// 	placeholder := raw_glyphs[0]
// 	bad_chars := 0
// 	defer fmt.println("Bad characters:", bad_chars)
// 	for &slot, i in atlas.glyphs {
// 		packed_slot := raw_glyphs[i]
//
// 		if packed_slot.width == 0 && packed_slot.height == 0 {
// 			bad_chars += 1
// 			atlas.glyphs[i] = AtlasSlot {
// 				glyph_offset = placeholder.glyph_offset,
// 				atlas_offset = placeholder.atlas_offset,
// 				width = placeholder.width,
// 				height = placeholder.height,
// 				codepoint = packed_slot.codepoint,
// 			}
// 			continue
// 		}
//
// 		slot.width = packed_slot.width
// 		slot.height = packed_slot.height
// 		slot.atlas_offset = packed_slot.atlas_offset
// 		slot.glyph_offset = packed_slot.glyph_offset
// 		slot.codepoint = packed_slot.codepoint
//
// 		// Copy bitmap into apropriate slot
// 		for y in 0..<slot.height {
// 			for x in 0..<slot.width {
// 				ax, ay := int(x + packed_slot.atlas_offset.x), int(y + packed_slot.atlas_offset.y)
// 				atlas.pixels[atlas.width * ay + ax] = packed_slot.pixels[packed_slot.width * y + x]
// 			}
// 		}
// 	}
//
// 	return
// }

// draw_atlas_grid :: proc(atlas: GlyphAtlas, pos: [2]int){
// 	rl.DrawRectangleLines(i32(pos.x), i32(pos.y), i32(atlas.width), i32(atlas.height), rl.RED)
//
// 	for glyph in atlas.glyphs {
// 		off := glyph.atlas_offset
//
// 		rl.DrawRectangleLines(i32(glyph.atlas_offset.x + pos.x), i32(glyph.atlas_offset.y + pos.y), i32(glyph.width), i32(glyph.height),
// 			{0x30, 0xfe, 0xfe, 0xff})
// 	}
// }

as_texture :: proc(atlas: GlyphAtlas) -> rl.Texture {
	img := rl.Image {
		data = raw_data(atlas.pixels),
		width = i32(atlas.width),
		height = i32(atlas.height),
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

	font, ok := font_load(FONT, 24)
	font.edge_value = 0.52
	font.sharpness = 12.1
	ensure(ok, "Failed to laod font")

	// tex := as_texture(atlas)
	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		// rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xff, 0xff, 0xff, 0xff})
		// draw_atlas_grid(atlas, {10, 10})
		rl.EndDrawing()
	}
}
