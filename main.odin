package font_render

import "core:mem"
import "core:fmt"
import "core:time"
import "core:math"
import rl "vendor:raylib"
import ttf "vendor:stb/truetype"

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
    ensure(values != nil, "Fatal error within stb truetype")
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
    delete(font.sdf_cache)
}

Font :: struct {
    info: ttf.fontinfo,
    sdf_cache: map[rune]Distance_Field,
    bitmap_cache: map[rune]Bitmap,

    size: f32,
    edge_value: f32,
	dist_scale: f32,
    sharpness: f32,
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

main :: proc(){
    rl.InitWindow(1200, 800, "font render")
    rl.SetTargetFPS(60)

    font, ok := font_load(FONT, 22)
    ensure(ok, "Failed to laod font")

    bmap, _ := get_rune_bitmap(&font, 'G')
    tex := as_texture(bmap)
    for !rl.WindowShouldClose(){
        rl.BeginDrawing()
        rl.ClearBackground(rl.BLACK)
        rl.DrawTextureEx(tex, {10, 10}, 0, 8, {0xff, 0xff, 0xff, 0xff})
        rl.EndDrawing()
    }
}