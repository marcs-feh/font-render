package font_render

import "core:slice"
import "core:mem"
import "core:fmt"
import "core:time"
import "core:math"
import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

Font :: struct {
	info: ttf.fontinfo,

	size: f32,
	edge_value: f32,
	dist_scale: f32,
	sharpness: f32,
}

Distance_Field :: struct {
	values: []u8,
	width: int,
	height: int,
	offset: [2]int,
}

AtlasSlot :: struct {
	width: int,
	height: int,
	atlas_offset: [2]int,
	glyph_offset: [2]int,
	codepoint: rune,
}

// Used only to pack glyphs
@private
RawGlyph :: struct {
	atlas_offset: [2]int,
	glyph_offset: [2]int,
	codepoint: rune,
	width: int,
	height: int,
	pixels: []u8,
}

GlyphAtlas :: struct {
	base_glyph: rune,
	slots: []AtlasSlot,
	font: ^Font,

	pixels: []u8,
	width: int,
	height: int,
}

get_rune_sdf :: proc(font: ^Font, codepoint: rune, arena: ^mem.Arena) -> (field: Distance_Field, err: mem.Allocator_Error) {
    context.allocator = mem.arena_allocator(arena)

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
	if values == nil {
		return
	}
	defer ttf.FreeSDF(values, nil)

	field.values = make([]u8, width * height) or_return
	mem.copy_non_overlapping(raw_data(field.values), values, int(width * height))

	field.width = int(width)
	field.height = int(height)
	field.offset = {int(x_off), int(y_off)}

	return
}

sigmoid_activate :: proc(v: f32, s: f32) -> f32 {
	return 1.0 / (1 + math.exp(-s * (v - 0.5)))
}

get_rune_bitmap :: proc(font: ^Font, codepoint: rune, arena: ^mem.Arena) -> (glyph: RawGlyph, err: mem.Allocator_Error){
    context.allocator = mem.arena_allocator(arena)

	sdf := get_rune_sdf(font, codepoint, arena) or_return
	glyph.pixels = make([]u8, sdf.width * sdf.height) or_return
	glyph.width = sdf.width
	glyph.height = sdf.height
	glyph.glyph_offset = {auto_cast sdf.offset.x, auto_cast  sdf.offset.y}
	glyph.codepoint = codepoint

	for sv, i in sdf.values {
		v := f32(sv) / 0xff
		px := u8(clamp(0, 0xff * sigmoid_activate(v, font.sharpness), 255))
		glyph.pixels[i] = px
	}

	return
}

font_load :: proc(data: []byte, size: f32) -> (font: Font, ok: bool) {
	mem_err : mem.Allocator_Error
	bool(ttf.InitFont(&font.info, raw_data(data), 0)) or_return

	font.size = size

	font.edge_value = 0.63
	font.dist_scale = 0.61
	font.sharpness  = 9.25

	ok = true
	return
}

font_destroy :: proc(font: ^Font){
}

FONT :: #load("noto.ttf", []byte)

@(require_results, private)
pack_atlas_rows :: proc(glyphs: []RawGlyph, max_width: int) -> (int, int){
	// Apply indirection to sort without losing relative codepoint
	rects := make([]^RawGlyph, len(glyphs), context.temp_allocator)
	for _, i in rects {
		rects[i] = &glyphs[i]
	}

	cur_x, cur_y, max_height, total_height : int

	slice.sort_by_cmp(rects[:], proc(a, b: ^RawGlyph) -> slice.Ordering {
		ord := slice.Ordering(clamp(-1, b.height - a.height, +1))
		if ord == .Equal {
			ord = slice.Ordering(clamp(-1, b.width - a.width, +1))
		}
		return ord
	})

	for rect, i in rects {
		if cur_x + rect.width > max_width {
			cur_y += max_height
			total_height += max_height
			cur_x = 0
			max_height = 0
		}

		rects[i].atlas_offset = {cur_x, cur_y}
		cur_x += rect.width
		max_height = max(max_height, rect.height)
	}

	total_height += max_height

	return int(max_width + 2), int(total_height + 2)
}

atlas_create :: proc(font: ^Font, base: rune, glyph_count: int, temp_reserve := 32 * mem.Megabyte) -> (atlas: GlyphAtlas, err: mem.Allocator_Error){
	// Setup arena for temporary bitmap allocations
    arena_mem := make([]byte, temp_reserve) or_return
	defer delete(arena_mem)
    arena : mem.Arena
    mem.arena_init(&arena, arena_mem)
	context.temp_allocator = mem.arena_allocator(&arena)

	raw_glyphs := make([dynamic]RawGlyph, 0, glyph_count + 1, context.temp_allocator) or_return

    total_width : int
	non_zero_slots : int = 1
	// Add placeholder rune
	{
		placeholder_slot := get_rune_bitmap(font, 0, &arena) or_return
		placeholder_slot.codepoint = -1
		append(&raw_glyphs, placeholder_slot)
		total_width += placeholder_slot.width
	}

	start := time.now()
	// Render slots bitmaps
	for off in 0..<rune(glyph_count) {
		slot : RawGlyph
		err : mem.Allocator_Error

		r := base + off
		if idx := ttf.FindGlyphIndex(&font.info, r); idx != 0 {
			slot, err = get_rune_bitmap(font, base + off, &arena)
			if err != nil { continue }
			total_width += int(slot.width)
			non_zero_slots += 1
		}
		append(&raw_glyphs, slot)
	}
	fmt.println("Rendering Peak:", arena.peak_used / mem.Kilobyte, "KiB", "Took:", time.since(start))

	assert(len(raw_glyphs) == glyph_count + 1, "Did not create all glyphs, maybe reserved space is too small?")

	// Initialize atlas
	{
		atlas.slots = make([]AtlasSlot, len(raw_glyphs)) or_return
		atlas.base_glyph = base
		atlas.font = font

		atlas_width := f64(total_width / non_zero_slots) * math.sqrt(f64(non_zero_slots)) * 1.5
		aw, ah := pack_atlas_rows(raw_glyphs[:], int(max(16, atlas_width)))

		atlas.pixels = make([]u8, aw * ah) or_return
		atlas.width = aw
		atlas.height = ah
	}

	// Transfer temporary packed slots into final atlas bitmap
	placeholder := raw_glyphs[0]
	bad_chars := 0
	defer fmt.println("Bad characters:", bad_chars)
	for &slot, i in atlas.slots {
		packed_slot := raw_glyphs[i]

		if packed_slot.width == 0 && packed_slot.height == 0 {
			bad_chars += 1
			atlas.slots[i] = AtlasSlot {
				glyph_offset = placeholder.glyph_offset,
				atlas_offset = placeholder.atlas_offset,
				width = placeholder.width,
				height = placeholder.height,
				codepoint = packed_slot.codepoint,
			}
			continue
		}

		slot.width = packed_slot.width
		slot.height = packed_slot.height
		slot.atlas_offset = packed_slot.atlas_offset
		slot.glyph_offset = packed_slot.glyph_offset
		slot.codepoint = packed_slot.codepoint

		// Copy bitmap into apropriate slot
		for y in 0..<slot.height {
			for x in 0..<slot.width {
				ax, ay := int(x + packed_slot.atlas_offset.x), int(y + packed_slot.atlas_offset.y)
				atlas.pixels[atlas.width * ay + ax] = packed_slot.pixels[packed_slot.width * y + x]
			}
		}
	}

	return
}

draw_atlas_grid :: proc(atlas: GlyphAtlas, pos: [2]int){
	rl.DrawRectangleLines(i32(pos.x), i32(pos.y), i32(atlas.width), i32(atlas.height), rl.RED)

	for glyph in atlas.slots {
		off := glyph.atlas_offset

		rl.DrawRectangleLines(i32(glyph.atlas_offset.x + pos.x), i32(glyph.atlas_offset.y + pos.y), i32(glyph.width), i32(glyph.height),
			{0x30, 0xfe, 0xfe, 0xff})
	}
}

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

main :: proc(){
	rl.InitWindow(1200, 800, "font render")
    rl.SetWindowState({.WINDOW_RESIZABLE})
	rl.SetTargetFPS(60)

	font, ok := font_load(FONT, 24)
	font.edge_value = 0.52
	font.sharpness = 12.1
	ensure(ok, "Failed to laod font")

    now := time.now()
	// atlas, _ := atlas_create(&font, 0x4e00, 0x9fff - 0x4e00)
	atlas, _ := atlas_create(&font, 0, 8192)

	// atlas, _ := atlas_create(&font, 0x2200, 1024)
    elapsed := time.since(now)

    fmt.println("Took", elapsed, "to create atlas")
	// for r in 0..<256 {
	//     get_rune_bitmap(&font, rune(r))
	// }

	// for slot, i in atlas.slots {
	// 	fmt.println(slot.codepoint, ":", slot.atlas_offset, slot.width, slot.height)
	// }
	tex := as_texture(atlas)
	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xff, 0xff, 0xff, 0xff})
		draw_atlas_grid(atlas, {10, 10})
		rl.EndDrawing()
	}
}
