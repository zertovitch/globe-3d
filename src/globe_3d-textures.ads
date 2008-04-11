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

  ----------------------------------------------------------------------
  -- a) Automatic Texture name association through an enumarated type --
  ----------------------------------------------------------------------
  --
  -- For test or demo programs.
  -- Easy, reliable, but: static, hard-coded and diallowing a
  -- directory structure.

  generic
    type Texture_enum is (<>);
  procedure Associate_textures;

  ----------------------------------------------------------------------
  -- b) Automatic Texture name association by searching the .zip data --
  --    resource files for images                                     --
  ----------------------------------------------------------------------
  --
  -- For "real life" programs which don't know of the data. 
  -- Allows subdirectories in resource ('/' or '\' in names)
  -- and a flexible management. 
  -- The texture name list is obtained by traversing the directory of
  -- both .zip data resource files, searching for images (anyway, the
  -- textures are read from there!).

  -- TBD! (idea: 11-apr-2008)

  ----------------------------------------
  -- c) Manual Texture name association --
  ----------------------------------------
  --
  -- Free way of associating names. E.g., the texture name list could be
  -- simply stored on a text file, or obtained by a "dir"-like operation.

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

