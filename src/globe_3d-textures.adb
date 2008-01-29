with GL, GL.IO, Unzip.Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body GLOBE_3D.Textures is

  type Texture_info is record
    loaded       : Boolean;
    blending_hint: Boolean;
    name         : Ident;
  end record;

  type Texture_info_array is array( Image_id range <>) of Texture_info;
  type p_Texture_info_array is access Texture_info_array;

  procedure Dispose is new Ada.Unchecked_Deallocation(Texture_info_array, p_Texture_info_array);

  texture_2d_infos: p_Texture_info_array:= null;

  -----------------------------
  -- Load_texture (internal) --
  -----------------------------

  procedure Load_texture_2D (id: Image_id; blending_hint: out Boolean) is
    tex_name: constant String:= Trim(texture_2D_infos(id).name,Right);

    procedure Try( zif: in out Zip.Zip_info; name: String ) is
      use Unzip.Streams;
      ftex: Zipped_File_Type;
      procedure Try_a_type(tex_name_ext: String; format: GL.IO.Supported_format) is
      begin
        Open( ftex, zif, tex_name_ext );
        GL.IO.Load( Stream(ftex), format, Image_id'Pos(id)+1, blending_hint );
        Close( ftex );
      exception
        when Zip.File_name_not_found =>
          raise;
        when e:others =>
          raise_exception(
            Exception_Identity(e),
            Exception_Message(e) & " on texture: " & tex_name_ext
          );
      end Try_a_type;
    begin -- Try
      Load_if_needed( zif, name );
      Try_a_type(tex_name & ".TGA", GL.IO.TGA);
    exception
      when Zip.File_name_not_found =>
        Try_a_type(tex_name & ".BMP", GL.IO.BMP);
    end Try;
  begin
    begin
      Try( zif_level, To_String(level_data_name) );
    exception
      when Zip.File_name_not_found |
           Zip.Zip_file_open_error =>
        -- Not found in level-specific pack
        Try( zif_global, To_String(global_data_name) );
    end;
  exception
    when Zip.File_name_not_found |
         Zip.Zip_file_open_error =>
      -- Never found - neither in level, nor in global pack
      raise_exception(Missing_texture'Identity, "texture: " & tex_name);
  end Load_texture_2D;

  function Valid_texture_ID(id: Image_id) return Boolean is
  begin
    return id in texture_2d_infos'Range;
  end Valid_texture_ID;

  procedure Check_2D_texture(id: Image_id; blending_hint: out Boolean) is
  begin
    if texture_2D_infos = null then
      raise Textures_not_reserved;
    end if;
    if not Valid_texture_ID(id) then raise
      Texture_out_of_range;
    end if;
    if texture_2D_infos(id).loaded then
      blending_hint:= texture_2D_infos(id).blending_hint;
    else
      Load_texture_2D(id, blending_hint);
      texture_2D_infos(id).loaded:= True;
      texture_2D_infos(id).blending_hint:= blending_hint;
    end if;
  end Check_2D_texture;

  procedure Check_2D_texture(id: Image_id) is
    junk: Boolean;
  begin
    Check_2D_texture(id,junk);
  end Check_2D_texture;

  ---------------------
  -- Bind_2D_texture --
  ---------------------

  procedure Bind_2D_texture (id: Image_id; blending_hint: out Boolean) is
  begin
    Check_2D_texture(id, blending_hint);
    GL.BindTexture( GL.TEXTURE_2D, GL.Uint(Image_id'Pos(id)+1) );
  end Bind_2D_texture;

  procedure Reserve_textures( last_id: Image_id ) is
  begin
    if texture_2d_infos /= null then
      Dispose(texture_2d_infos);
    end if;
    texture_2d_infos:= new Texture_info_array(0..last_id);
    texture_2d_infos.all:= (others => (False,False,empty));
  end Reserve_textures;

  procedure Name_texture( id: Image_id; name: String ) is
    n_id: Ident:= empty;
  begin
    for i in name'Range loop
      n_id(n_id'First + i - name'First):= name(i);
    end loop;
    if texture_2D_infos = null then
      raise Textures_not_reserved;
    end if;
    if id not in texture_2d_infos'Range then raise
      Texture_out_of_range;
    end if;
    texture_2d_infos(id).name:= n_id;
  end Name_texture;

  procedure Associate_textures is
  begin
    Reserve_textures( Texture_enum'Pos(Texture_enum'Last) );
    for t in Texture_enum loop
      Name_Texture( Texture_enum'Pos(t), Texture_enum'Image(t) );
    end loop;
  end Associate_textures;

  function Texture_name( id: Image_id; trim: Boolean ) return Ident is
    tn: Ident;
  begin
    if id not in texture_2d_infos'Range then raise
      Texture_out_of_range;
    end if;
    tn:= texture_2d_infos(id).name;
    if trim then
      return Ada.Strings.Fixed.Trim(tn,Right);
    else
      return tn;
    end if;
  end Texture_name;

  function Texture_ID( name: Ident ) return Image_ID is
    up_name: constant Ident:= To_Upper(name);
  begin
    for id in texture_2d_infos'Range loop
      if up_name = To_Upper(texture_2d_infos(id).name) then
        return id;
      end if;
    end loop;
    raise_exception(
      Texture_name_not_found'Identity,
      " Texture: [" & Trim(name,both) & ']'
    );
    raise Constraint_Error;
    -- ^ fake, just some compilers warn because they don't know that
    --   raise_exception will raise an exception.
  end Texture_ID;

end GLOBE_3D.Textures;

