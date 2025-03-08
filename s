package font_render
	constants
		FONT :: #load("jetbrains.ttf", []byte)
		FONT_RENDER_DEFAULT_CACHE_CAPACITY :: #config(FONT_RENDER_DEFAULT_CACHE_CAPACITY, 256)
		FONT_RENDER_SCRATCH_SPACE :: #config(FONT_RENDER_SCRATCH_SPACE, 1 * mem.Megabyte)
		PLACEHOLDER_RUNE :: -1
			Rune to be rendered when a codepoint cannot be found


	procedures
		as_texture :: proc(atlas: Glyph_Atlas) -> rl.Texture {...}
		atlas_create :: proc(allocator := context.allocator, glyph_cap := FONT_RENDER_DEFAULT_CACHE_CAPACITY) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error) {...}
			Create an empty font atlas with a preallocated capacity

		atlas_destroy :: proc(atlas: ^Glyph_Atlas) {...}
			Destroy a font atlas (uses `context.allocator` for deleting pixels)

		draw_atlas_grid :: proc(atlas: Glyph_Atlas, pos: [2]i32) {...}
		font_destroy :: proc(font: ^Font) {...}
			Deallocate a font object

		font_generate_atlas :: proc(font: ^Font, temp_arena: ^mem.Arena = nil) -> (atlas: Glyph_Atlas, err: mem.Allocator_Error) {...}
				Generate a font atlas with the glyphs currently present in the font's sdf store. An arena is used as scratch space,
				if it is not provided then the default from the package is used.

		font_load :: proc(data: []byte, size: f32, allocator := context.allocator) -> (font: Font, ok: bool) {...}
			Load a font from a TTF data set

		font_update_atlas :: proc(font: ^Font) -> (err: mem.Allocator_Error) {...}
			Update the font's atlas, deleting the previous one

		get_rune_sdf :: proc(font: ^Font, codepoint: rune) -> (field: Distance_Field, err: mem.Allocator_Error) {...}
			Get a codepoint's field present in the font's sdf_cache, if it is not present a new one will be allocated.

		init_scratch_space :: proc() {...}
		main :: proc() {...}
		render_bitmap_rune :: proc(font: ^Font, codepoint: rune, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error) {...}
			Render a new bitmap for a codepoint from font

		render_bitmap_sdf :: proc(sdf: Distance_Field, sharpness: f32, allocator := context.allocator) -> (glyph: Glyph_Bitmap, err: mem.Allocator_Error) {...}
			Render a new bitmap from an signed distance field

		render_sdf :: proc(font: ^Font, codepoint: rune, allocator := context.allocator) -> (field: Distance_Field, err: mem.Allocator_Error) {...}
			Allocate a signed distance field for a codepoint of font, this does not do caching.

		render_text :: proc(font: ^Font, text: string, line_height: i32 = -1) -> (positions: []Glyph_Rect, bounds: Box) {...}
		sdf_texture :: proc(f: Distance_Field) -> rl.Texture {...}
		sigmoid_activate :: proc(v: f32, s: f32) -> f32 {...}

	proc_group
		render_bitmap :: proc{render_bitmap_sdf, render_bitmap_rune}

	types
		Box :: struct {using pos: [2]i32, width, height: i32}
			Rectangle, used for bounds measuring

		Distance_Field :: struct {width: i32, height: i32, offset: [2]i32, values: []u8, codepoint: rune}
			A signed distance field for a codepoint, the higher the value the "more inside" it is inside the glyph's contour

		Font :: struct {info: ttf.fontinfo, sdf_store: map[rune]Distance_Field, placeholder: Distance_Field, atlas: Glyph_Atlas, size: f32, edge_value: f32, dist_scale: f32, sharpness: f32, allocator: mem.Allocator}
			Container for a specific TTF font

		Glyph_Atlas :: struct {glyphs: map[rune]Glyph_Atlas_Slot, pixels: []u8, width: i32, height: i32}
			A large bitmap that contains a set of glyphs and their offsets into the atlas

		Glyph_Atlas_Slot :: struct {atlas_offset: [2]i32, glyph_offset: [2]i32, width: i32, height: i32}
			Slot to be put in a glyph atlas. `glyph_offset` is the offset from the glyph's origin.

		Glyph_Bitmap :: struct {codepoint: rune, atlas_offset: [2]i32, glyph_offset: [2]i32, width: i32, height: i32, pixels: []u8}
			Rendered version of a glyph, note that `atlas_offset` is only used to pack the glyph

		Glyph_Rect :: struct {draw_offset: [2]i32, atlas_offset: [2]i32, width: i32, height: i32, codepoint: rune}
			Rectangle corresponding to an atlas glyph with ajusted offsets to be used in rendering



	fullpath:
		/home/marcos/projects/font-render
	files:
		main.odin
