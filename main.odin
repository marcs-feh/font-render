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
    sdf_cache: map[rune]Distance_Field,
    bitmap_cache: map[rune]Bitmap,

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

// RGBA
Color :: [4]u8

Bitmap :: struct {
    pixels: []Color,
    width: int,
    height: int,
    offset: [2]int,
}

get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, err: mem.Allocator_Error) {
    if cached, ok := font.sdf_cache[codepoint]; ok {
        return cached, nil
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
    if values == nil {
        return
    }
    defer ttf.FreeSDF(values, nil)

    field.values = make([]u8, width * height) or_return
    mem.copy_non_overlapping(raw_data(field.values), values, int(width * height))

    field.width = int(width)
    field.height = int(height)
    field.offset = {int(x_off), int(y_off)}


    font.sdf_cache[codepoint] = field
    return
}

sigmoid_activate :: proc(v: f32, s: f32) -> f32 {
    return 1.0 / (1 + math.exp(-s * (v - 0.5)))
}

get_rune_bitmap :: proc(font: ^Font, codepoint: rune) -> (bitmap: Bitmap, err: mem.Allocator_Error){
    if cached, ok := font.bitmap_cache[codepoint]; ok {
        return cached, nil
    }

    sdf := get_rune_sdf(font, codepoint) or_return
    bitmap.pixels = make([]Color, sdf.width * sdf.height) or_return
    bitmap.width = sdf.width
    bitmap.height = sdf.height
    bitmap.offset = sdf.offset

    for sv, i in sdf.values {
        v := f32(sv) / 0xff
        px := u8(clamp(0, 0xff * sigmoid_activate(v, font.sharpness), 255))
        bitmap.pixels[i] = px
    }

    font.bitmap_cache[codepoint] = bitmap
    return
}

font_load :: proc(data: []byte, size: f32) -> (font: Font, ok: bool) {
    mem_err : mem.Allocator_Error
    bool(ttf.InitFont(&font.info, raw_data(data), 0)) or_return

    font.size = size
    font.sdf_cache, mem_err = make(map[rune]Distance_Field, 256)
    if mem_err != nil {
        return {}, false
    }

    font.bitmap_cache, mem_err = make(map[rune]Bitmap, 256)
    if mem_err != nil {
        return {}, false
    }

    font.edge_value = 0.63
    font.dist_scale = 0.61
    font.sharpness  = 9.25

    ok = true
    return
}

font_destroy :: proc(font: ^Font){
    for _, field in font.sdf_cache {
        delete(field.values)
    }
    for _, bmap in font.bitmap_cache {
        delete(bmap.pixels)
    }
    delete(font.sdf_cache)
    delete(font.bitmap_cache)
}

as_texture :: proc(bmap: Bitmap) -> rl.Texture {
    img := rl.Image {
        data = raw_data(bmap.pixels),
        width = i32(bmap.width),
        height = i32(bmap.height),
        mipmaps = 1,
        format = .UNCOMPRESSED_R8G8B8A8,
    }

    return rl.LoadTextureFromImage(img)
}

FONT :: #load("jetbrains.ttf", []byte)

AtlasSlot :: struct {
    width: int,
    height: int,
    atlas_offset: [2]int,
    glyph_offset: [2]int,
}

GlyphAtlas :: struct {
    base_glyph: rune,
    slots: []AtlasSlot,
    pixels: Bitmap,
    font: ^Font,
}

@(require_results)
pack_bitmap_rows :: proc(rects: []AtlasSlot, max_width: int) -> (int, int){
    cur_x := 0
    cur_y := 0
    max_height := 0
    total_height := 0

    slice.sort_by_cmp(rects[:], proc(a, b: AtlasSlot) -> slice.Ordering {
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

atlas_create :: proc(font: ^Font, base: rune, glyph_count: int) -> (atlas: GlyphAtlas, err: mem.Allocator_Error){
    atlas.slots = make([]AtlasSlot, glyph_count) or_return
    atlas.base_glyph = base
    atlas.font = font

    for &slot, i in atlas.slots {
        bmap, _ := get_rune_bitmap(font, base + rune(i))
        slot.glyph_offset = bmap.offset
        slot.width = bmap.width
        slot.height = bmap.height
    }
    
    aw, ah := pack_bitmap_rows(atlas.slots, 500)
    // atlas.pixels = make([]Color, aw * ah) or_return
    atlas.pixels.width = aw
    atlas.pixels.height = ah

    return
}

import "core:hash"
draw_atlas :: proc(atlas: GlyphAtlas){
    rl.DrawRectangleLines(0, 0, i32(atlas.pixels.width), i32(atlas.pixels.height), rl.RED)

    for glyph in atlas.slots {
        off := glyph.atlas_offset
        bytes := transmute([size_of(off)]byte)off

        r := u8(hash.fnv32(bytes[:]) % 120) + 100
        rl.DrawRectangle(i32(glyph.atlas_offset.x), i32(glyph.atlas_offset.y), i32(glyph.width), i32(glyph.height),
            {r, 0xfe, 0xfe, 127})
    }
}

main :: proc(){
    rl.InitWindow(1200, 800, "font render")
    rl.SetTargetFPS(60)

    font, ok := font_load(FONT, 129)
    ensure(ok, "Failed to laod font")

    atlas, _ := atlas_create(&font, 0, 64)


    // for r in 0..<256 {
    //     get_rune_bitmap(&font, rune(r))
    // }

    // tex := as_texture(font.bitmap_cache['G'])
    for !rl.WindowShouldClose(){
        rl.BeginDrawing()
        rl.ClearBackground(rl.BLACK)
        // rl.DrawTextureEx(tex, {10, 10}, 0, 1, {0xff, 0xff, 0xff, 0xff})
        draw_atlas(atlas)
        rl.EndDrawing()
    }
}