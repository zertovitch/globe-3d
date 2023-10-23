with GL, GL.IO, UnZip.Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded.Hash;

package body GLOBE_3D.Textures is

  --------------------------------------------------------------------
  --  1) Fast access though the number (Image_ID -> Texture_info):  --
  --------------------------------------------------------------------
  type Texture_Info is record
    loaded        : Boolean := False;
    blending_hint : Boolean := False;
    name          : Ident   := empty_ident;
  end record;

  type Texture_Info_Array is array (Image_ID range <>) of Texture_Info;
  type p_Texture_info_array is access Texture_Info_Array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation (Texture_Info_Array, p_Texture_info_array);

  -------------------------------------
  --  2) Fast access through a name  --
  -------------------------------------

  package Texture_Name_Mapping is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Image_ID,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

  type Texture_2D_Infos_Type is record
    tex               : p_Texture_info_array;
    map               : Texture_Name_Mapping.Map;
    last_entry_in_use : Image_ID;
  end record;

  empty_texture_2d_infos : constant Texture_2D_Infos_Type :=
    (null,
     Texture_Name_Mapping.Empty_Map,
     null_image);

  texture_2d_infos : Texture_2D_Infos_Type := empty_texture_2d_infos;

  -------------------------------
  --  Load_texture (internal)  --
  -------------------------------

  procedure Load_texture_2D (id : Image_ID; blending_hint : out Boolean) is
    tex_name : constant String := Trim (texture_2d_infos.tex (id).name, Right);
    found : Boolean := False;

    procedure Try_archive (zif : in out Zip.Zip_info; name : String) is
      use UnZip.Streams;
      ftex : Zipped_File_Type;
      procedure Try_image_type (tex_name_ext : String) is
      begin
        Open (ftex, zif, tex_name_ext);
        GL.IO.Load (
          Ada.Streams.Stream_IO.Stream_Access (Stream (ftex)),
          Image_ID'Pos (id) + 1, blending_hint
        );
        Close (ftex);
        found := True;
      exception
        when Zip.Entry_name_not_found =>
          null;  --  Nothing bad, item just not found.
        when e : others =>
          Raise_Exception (
            Exception_Identity (e),
            Exception_Message (e) & " on texture: " & tex_name_ext
          );
      end Try_image_type;
    begin -- Try_archive
      Load_if_needed (zif, name);
      Try_image_type (tex_name & ".TGA");
      if found then
        return;
      end if;
      Try_image_type (tex_name & ".BMP");
      if found then
        return;
      end if;
      Try_image_type (tex_name & ".JPG");
      if found then
        return;
      end if;
      --  Tip: use rather the TGA format: single compression, better with LZMA in the Zip archive.
      Try_image_type (tex_name & ".PNG");
      if found then
        return;
      end if;
      --  Tip: use rather the TGA format: single, better compression in the Zip archive.
      Try_image_type (tex_name & ".GIF");
    exception
      when Zip.Archive_open_error =>
        null;
    end Try_archive;
  begin
    Try_archive (zif_level, S (level_data_name));
    if found then
      return;
    end if;
    Try_archive (zif_global, S (global_data_name));
    if found then
      return;
    end if;
    --  Never found - neither in level, nor in global pack
    raise Missing_texture with "texture: " & tex_name;
  end Load_texture_2D;

  function Valid_Texture_ID (id : Image_ID) return Boolean is
  begin
    return id in null_image + 1 .. texture_2d_infos.last_entry_in_use;
  end Valid_Texture_ID;

  procedure Check_2D_Texture (id : Image_ID; blending_hint : out Boolean) is
  begin
    if not Valid_Texture_ID (id) then
      raise Undefined_texture_ID with "ID =" & Image_ID'Image (id);
    end if;
    if texture_2d_infos.tex (id).loaded then
      --  Already loaded. We recall the information we had at loading time.
      blending_hint := texture_2d_infos.tex (id).blending_hint;
    else
      Load_texture_2D (id, blending_hint);
      texture_2d_infos.tex (id).loaded := True;
      texture_2d_infos.tex (id).blending_hint := blending_hint;
    end if;
  end Check_2D_Texture;

  procedure Check_2D_Texture (id : Image_ID) is
    junk_blending_hint : Boolean;
    pragma Warnings (off, junk_blending_hint);
  begin
    Check_2D_Texture (id, junk_blending_hint);
  end Check_2D_Texture;

  procedure Check_All_Textures is
  begin
    for i in null_image + 1 .. texture_2d_infos.last_entry_in_use loop
      Check_2D_Texture (i);
    end loop;
  end Check_All_Textures;

  procedure Reset_Textures is
  begin
    if texture_2d_infos.tex /= null then
      Dispose (texture_2d_infos.tex);
    end if;
    texture_2d_infos := empty_texture_2d_infos;
  end Reset_Textures;

  procedure Add_Texture_Name (name : String; id : out Image_ID) is
    new_tab : p_Texture_info_array;
    up_name : constant String := To_Upper (name);
    --  Convention: UPPER_CASE for identifiers
    n_id : Ident := empty_ident;
    pos : Texture_Name_Mapping.Cursor;
    success : Boolean;
  begin
    if texture_2d_infos.tex = null then
      texture_2d_infos.tex := new Texture_Info_Array (0 .. 100);
    end if;
    if texture_2d_infos.last_entry_in_use >= texture_2d_infos.tex'Last then
      --  We need to enlarge the table: we double it...
      new_tab := new Texture_Info_Array (0 .. texture_2d_infos.tex'Last * 2);
      new_tab (texture_2d_infos.tex'Range) := texture_2d_infos.tex.all;
      Dispose (texture_2d_infos.tex);
      texture_2d_infos.tex := new_tab;
    end if;
    id := texture_2d_infos.last_entry_in_use + 1;
    for i in up_name'Range loop
      n_id (n_id'First + i - up_name'First) := up_name (i);
    end loop;
    texture_2d_infos.tex (id).name := n_id;
    texture_2d_infos.last_entry_in_use :=
      Image_ID'Max (texture_2d_infos.last_entry_in_use, id);
    --  Feed the name dictionary with the new name:
    Texture_Name_Mapping.Insert (
      texture_2d_infos.map,
      U (up_name),
      id,
      pos,
      success
    );
    if not success then -- A.18.4. 45/2
      raise Duplicate_name with name & ", already stored as " & up_name;
    end if;
  end Add_Texture_Name;

  procedure Register_Textures_From_Resources is

    procedure Register (zif : in out Zip.Zip_info; name : String) is
      --
      procedure Action (tex_name : String) is
        dummy : Image_ID;
        ext : constant String := To_Upper (tex_name (tex_name'Last - 3 .. tex_name'Last));
      begin
        if ext = ".BMP" or else ext = ".TGA"
          or else ext = ".JPG" or else ext = "JPEG"
          or else ext = ".GIF" or else ext = ".PNG"
        then
          Add_Texture_Name (tex_name (tex_name'First .. tex_name'Last - 4), dummy);
        end if;
      end Action;
      --
      procedure Traverse is new Zip.Traverse (Action);
    begin
      Load_if_needed (zif, name);
      Traverse (zif);
      --  That's it!
    exception
      when Zip.Archive_open_error =>
        null;
    end Register;

  begin
    Register (zif_level,  S (level_data_name));
    Register (zif_global, S (global_data_name));
  end Register_Textures_From_Resources;

  procedure Associate_Textures is
    dummy : Image_ID;
  begin
    Reset_Textures;
    for t in Texture_Enum loop
      Add_Texture_Name (Texture_Enum'Image (t), dummy);
    end loop;
  end Associate_Textures;

  function Texture_Name (id : Image_ID; trim_spaces : Boolean) return Ident is
    tn : Ident;
  begin
    if not Valid_Texture_ID (id) then
      raise Undefined_texture_ID with "ID =" & Image_ID'Image (id);
    end if;
    tn := texture_2d_infos.tex (id).name;
    if trim_spaces then
      return Ada.Strings.Fixed.Trim (tn, Right);
    else
      return tn;
    end if;
  end Texture_Name;

  function Texture_ID (name : String) return Image_ID is
    trimmed : constant String := Trim (name, Both);
    up_name : constant String := To_Upper (trimmed);
    use Texture_Name_Mapping;
    c : constant Cursor := texture_2d_infos.map.Find (U (up_name));
  begin
    if c = No_Element then
      --  Key not found
      raise Texture_name_not_found with
        "Texture: " & trimmed & ", searched as " & up_name & "." &
        ASCII.CR & ASCII.LF &
        "Check data files:" &
        ASCII.CR & ASCII.LF &
        "  - " & S (global_data_name) &
        ASCII.CR & ASCII.LF &
        "  - " & S (level_data_name) & '.' &
        ASCII.CR & ASCII.LF &
        "Or, check calls to Add_texture_name or Associate_textures." &
        ASCII.CR & ASCII.LF &
        "Or look if Register_textures_from_resources has browsed the right file extensions.";
    else
      return Element (c);
    end if;
  end Texture_ID;

end GLOBE_3D.Textures;
