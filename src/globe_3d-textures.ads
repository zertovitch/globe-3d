package GLOBE_3D.Textures is

  -- The textures are stored by GL in that way:
  --
  --   an image <-> a number (Image_ID)
  --
  -- To complete this and facilitate things, GLOBE_3D adds:
  --
  --   a number (Image_ID) ->
  --      a record (Texture_info) with an image name
  --      and whether the image was already stored into GL
  --      and other infos
  --   an image name -> a number (Image_ID)

  ---------------------------------------------------------------------
  -- Here are three ways of register the texture names that GLOBE_3D --
  -- will find on the resource files.                                --
  ---------------------------------------------------------------------

  --------------------------------------------
  -- a) Texture-by-texture name association --
  --------------------------------------------
  --
  -- Free way of associating names. E.g., the texture name list could be
  -- simply stored on a text file, or obtained by a "dir"-like operation.
  -- The system associates a name to a texture id that it finds itself.

  procedure Add_texture_name( name: String; id: out Image_id );

  ----------------------------------------------------------------------
  -- b) Texture name association by searching the .zip data --
  --    resource files for images                                     --
  ----------------------------------------------------------------------
  --
  -- For "real life" programs which don't know of the data.
  -- Allows subdirectories in resource ('/' or '\' in names)
  -- and a flexible management.
  -- The texture name list is obtained by traversing the directory of
  -- both .zip data resource files, searching for images (anyway, the
  -- textures are read from there!).

  procedure Register_textures_from_resources;

  ----------------------------------------------------------------------
  -- c) Texture name association through an enumarated type --
  ----------------------------------------------------------------------
  --
  -- For test or demo programs.
  -- Easy, reliable, but: static, hard-coded and diallowing a
  -- directory structure.

  generic
    type Texture_enum is (<>);
  procedure Associate_textures;

  --------------------------------------------------------------------------
  -- Once that texture names are registered, you have the following tools --
  --------------------------------------------------------------------------

  -- Check if the texture is loaded and load it if needed
  procedure Check_2D_texture(id: Image_id; blending_hint: out Boolean);
  -- variant for situations where the blending information doesn't matter:
  procedure Check_2D_texture(id: Image_id);

  -- Check, then bind as current GL texture
  procedure Bind_2D_texture(id: Image_id; blending_hint: out Boolean);

  function Valid_texture_ID(id: Image_id) return Boolean;

  -- >= 16-apr-2008: no more need to reserve anything; unbounded collection
  --
  --  Textures_not_reserved: exception;
  --  Texture_out_of_range: exception;

  Undefined_texture_ID  : exception;
  Undefined_texture_name: exception;

  -- - Erase the present texture collection
  --   (names, GL ID's, evenutally loaded images)
  procedure Reset_textures;

  -- - Recall a texture's name
  function Texture_name( id: Image_id; trim: Boolean ) return Ident;

  -- - Recall a texture's ID
  function Texture_ID( name: String ) return Image_ID;
  Texture_name_not_found: exception;

end GLOBE_3D.Textures;
