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
}

// Used only to pack glyphs
@private
AtlasUnpackedSlot :: struct {
	atlas_offset: [2]int,
	glyph_offset: [2]int,
	pixels: []u8,
	width: int,
	height: int,
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

get_rune_bitmap :: proc(font: ^Font, codepoint: rune, arena: ^mem.Arena) -> (bitmap: AtlasUnpackedSlot, err: mem.Allocator_Error){
    context.allocator = mem.arena_allocator(arena)

	sdf := get_rune_sdf(font, codepoint, arena) or_return
	bitmap.pixels = make([]u8, sdf.width * sdf.height) or_return
	bitmap.width = sdf.width
	bitmap.height = sdf.height
	bitmap.glyph_offset = sdf.offset

	for sv, i in sdf.values {
		v := f32(sv) / 0xff
		px := u8(clamp(0, 0xff * sigmoid_activate(v, font.sharpness), 255))
		bitmap.pixels[i] = px
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

FONT :: #load("jetbrains.ttf", []byte)

@(require_results)
pack_atlas_rows :: proc(rects: []AtlasUnpackedSlot, max_width: int) -> (int, int){
	cur_x := 0
	cur_y := 0
	max_height := 0
	total_height := 0

	slice.sort_by_cmp(rects[:], proc(a, b: AtlasUnpackedSlot) -> slice.Ordering {
		ord := slice.Ordering(clamp(-1, b.height - a.height, +1))
		if ord == .Equal {
			ord = slice.Ordering(clamp(-1, b.width - a.width, +1))
		}
		return ord
	})

	for &rect in rects {
		if cur_x + rect.width > max_width {
			cur_y += max_height
			total_height += max_height
			cur_x = 0
			max_height = 0
		}

		rect.atlas_offset = {cur_x, cur_y}
		cur_x += rect.width
		max_height = max(max_height, rect.height)
	}

	total_height += max_height

	return max_width + 2, total_height + 2
}

atlas_create :: proc(font: ^Font, base: rune, glyph_count: int, temp_reserve := 32 * mem.Megabyte) -> (atlas: GlyphAtlas, err: mem.Allocator_Error){
	atlas.slots = make([]AtlasSlot, glyph_count) or_return
	atlas.base_glyph = base
	atlas.font = font

	// Setup arena for temporary bitmap allocations
    arena_mem := make([]byte, temp_reserve) or_return
	defer delete(arena_mem)
    arena : mem.Arena
    mem.arena_init(&arena, arena_mem)
	context.temp_allocator = mem.arena_allocator(&arena)

	packing_slots := make([]AtlasUnpackedSlot, glyph_count) or_return

    total_width := 0

	// Render and pack slots
	for &slot, i in packing_slots {
		err : mem.Allocator_Error
		slot, err = get_rune_bitmap(font, base + rune(i), &arena)
        if err != nil { continue }

        total_width += slot.width
	}
	fmt.println("Peak:", arena.peak_used / mem.Kilobyte, "KiB")

    atlas_width := f64(total_width / len(atlas.slots)) * math.sqrt(f64(glyph_count)) * 1.5

	aw, ah := pack_atlas_rows(packing_slots[:], int(max(16, atlas_width)))
	atlas.pixels = make([]u8, aw * ah) or_return
	atlas.width = aw
	atlas.height = ah

	for &slot, i in atlas.slots {
		packed_slot := packing_slots[i]

		slot.width = packed_slot.width
		slot.height = packed_slot.height
		slot.atlas_offset = packed_slot.atlas_offset
		slot.glyph_offset = packed_slot.glyph_offset

		for y in 0..<slot.height {
			for x in 0..<slot.width {
				ax, ay := x + packed_slot.atlas_offset.x, y + packed_slot.atlas_offset.y
				atlas.pixels[atlas.width * ay + ax] = packed_slot.pixels[packed_slot.width * y + x]
			}
		}
	}

	return
}

draw_atlas :: proc(atlas: GlyphAtlas, pos: [2]int){
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

// TODO: de-duplicate reduntant "glyph not found char" with ttf.GetGlyphIndex

main :: proc(){
	rl.InitWindow(1200, 800, "font render")
    rl.SetWindowState({.WINDOW_RESIZABLE})
	rl.SetTargetFPS(60)

	font, ok := font_load(FONT, 24)
	ensure(ok, "Failed to laod font")

    now := time.now()
	atlas, _ := atlas_create(&font, 0x2200, 1024)
    elapsed := time.since(now)

    fmt.println("Took", elapsed, "to create atlas")
	// for r in 0..<256 {
	//     get_rune_bitmap(&font, rune(r))
	// }

	tex := as_texture(atlas)
	for !rl.WindowShouldClose(){
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xff, 0xff, 0xff, 0xff})
		draw_atlas(atlas, {10, 10})
		rl.EndDrawing()
	}
}
