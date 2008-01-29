package GLOBE_3D.Textures is

  -- Check if texture loaded and load if needed
  procedure Check_2D_texture(id: Image_id; blending_hint: out Boolean);
  -- variant for situations where the blending information doesn't matter:
  procedure Check_2D_texture(id: Image_id);

  -- Check, then bind as current GL texture
  procedure Bind_2D_texture(id: Image_id; blending_hint: out Boolean);

  function Valid_texture_ID(id: Image_id) return Boolean;

  Textures_not_reserved: exception;
  Texture_out_of_range: exception;

  -----------------------------------------------------------------------
  -- a) Automatic Texture name association through an enumarated type: --
  -----------------------------------------------------------------------
  -- Easy, reliable, but static, monolithic and diallows a
  -- directory structure

  generic
    type Texture_enum is (<>);
  procedure Associate_textures;

  -----------------------------------------
  -- b) Manual Texture name association: --
  -----------------------------------------
  -- Allows subdirectories in resource ('/' or '\' in names)
  -- and a flexible management

  -- - Reserve place for a texture name table:
  procedure Reserve_textures( last_id: Image_id );

  -- - Associate a name to a texture id:
  procedure Name_texture( id: Image_id; name: String );

  -- - Recall a texture's name
  function Texture_name( id: Image_id; trim: Boolean ) return Ident;

  -- - Recall a texture's ID
  function Texture_ID( name: Ident ) return Image_ID;
  Texture_name_not_found: exception;

end GLOBE_3D.Textures;

