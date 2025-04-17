--  Central (to the whole application) texture register

package GLOBE_3D.Textures is

  --  The textures are stored by GL in that way:
  --
  --   an image <-> a number (Image_ID)
  --
  --  To complete this, and facilitate things, GLOBE_3D adds the
  --  following associations:
  --
  --  *  a number (Image_ID) ->
  --        a record (Texture_info) with an image name
  --        and whether the image was already stored into GL
  --        (images can be loaded on first use, or preloaded)
  --        and other infos
  --
  --  *  an image name -> a number (Image_ID)
  --
  --  Names are case insensitive!

  --------------------------------------------------------------------------
  --  Here are three ways of registering the texture names that GLOBE_3D  --
  --  will find in the resource files. You can use any mix of these       --
  --  methods at your convenience.                                        --
  --------------------------------------------------------------------------

  ----------------------------------------------
  --  a) Texture-by-texture name association  --
  ----------------------------------------------
  --
  --  Free way of associating names. E.g., the texture name list could be
  --  simply stored on a text file, or obtained by a "dir"-like operation.
  --  The system associates a name to a texture id that it finds itself.

  procedure Add_Texture_Name (name : String; id : out Image_ID);

  --------------------------------------------------------------
  --  b) Texture name association by searching the .zip data  --
  --     resource files for images                            --
  --------------------------------------------------------------
  --
  --  For "real life" programs which don't know of the data.
  --  Allows subdirectories in resource ('/' or '\' in names)
  --  and a flexible management.
  --
  --  The texture name list is obtained by traversing the directory of
  --  both .zip data resource files, searching for images (anyway, the
  --  textures are read from there!).
  --
  --  The zip name(s) must be set first with
  --  GLOBE_3D.Set_level_data_name, GLOBE_3D.Set_global_data_name

  procedure Register_Textures_From_Resources;

  --------------------------------------------------------------
  --  c) Texture name association through an enumarated type  --
  --------------------------------------------------------------
  --
  --  For test or demo programs.
  --  Easy, reliable, but: static, hard-coded and disallowing a
  --  directory structure.

  generic
    type Texture_Enum is (<>);
  procedure Associate_Textures;

  -----------------------------------------------------------------------
  --  When texture names are registered, you have the following tools  --
  -----------------------------------------------------------------------

  --  - Recall a texture's ID by its name - you may need it to define objects' faces.
  function Texture_ID (name : String) return Image_ID;
  Texture_name_not_found : exception;

  --  - Recall a texture's name
  function Texture_Name (id : Image_ID; trim_spaces : Boolean) return Ident;

  --  Check if the texture image has been loaded and load it if needed.
  --  This is done automatically, but you may want to force the loading
  --  of the images before beginning to display.
  procedure Check_2D_Texture (id : Image_ID; blending_hint : out Boolean);

  --  Variant for situations where the blending information doesn't matter:
  procedure Check_2D_Texture (id : Image_ID);

  --  Same, but for all textures:
  procedure Check_All_Textures;

  Undefined_texture_ID  : exception;  --  raised by Texture_name or Check_2D_texture

  function Valid_Texture_ID (id : Image_ID) return Boolean;

  --  - Erase the present texture collection
  --    (names, GL ID's, possibly loaded images)
  procedure Reset_Textures;

end GLOBE_3D.Textures;
