with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
--  with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with UnZip.Streams;
with Float_portable_binary_transfer;
pragma Elaborate_All (Float_portable_binary_transfer);

with GLOBE_3D.Textures;
with GL.IO;

package body GLOBE_3D.IO is

  ------------------------------------------------
  -- Common, internal definitions, routines,... --
  ------------------------------------------------

  stop_type : constant Character :=
    Character'Val (26); -- Ctrl-Z to stop displaying a binary file as a text

  signature_obj_root : constant String :=
    "GLOBE_3D 3D Binary Object File (" & object_extension & "). ";

  signature_obj_v2008 : constant String := signature_obj_root &
    "Format version: 2-Apr-2008." & stop_type;

  signature_obj_v2016a : constant String := signature_obj_root &
    "Format version: 09-Jun-2016" & stop_type;

  signature_obj : constant String := signature_obj_root &
    "Format version: 15-Jun-2016" & stop_type;

  signature_bsp : constant String :=
    "GLOBE_3D Binary Space Partition File (" & BSP_extension & "). " &
    "Format version: 2-Apr-2008." & stop_type;

  subtype U8 is GL.Ubyte;
  type U16 is mod 2 ** 16;  for U16'Size use 16;
  type U32 is mod 2 ** 32;  for U32'Size use 32;

  type I16 is range -2 ** 15 .. 2 ** 15 - 1; for I16'Size use 16;
  type I32 is range -2 ** 31 .. 2 ** 31 - 1; for I32'Size use 32;

  f_scaling : constant := 2.0**24;
  package FFBT is
    new Float_portable_binary_transfer (GL.Float, I32, I16, True, f_scaling);
  use FFBT;
  d_scaling : constant := 2.0**27; -- 53/2=26.5
  package DFBT is
    new Float_portable_binary_transfer (GL.Double, I32, I16, True, d_scaling);
  use DFBT;

  function Cvt is new Ada.Unchecked_Conversion (I16, U16);
  function Cvt is new Ada.Unchecked_Conversion (I32, U32);
  function Cvt is new Ada.Unchecked_Conversion (U16, I16);
  function Cvt is new Ada.Unchecked_Conversion (U32, I32);

  generic
    type Number is mod <>;
  procedure Read_Intel_x86_number (gsb : in out GL.IO.Input_buffer; n : out Number);

  procedure Read_Intel_x86_number (gsb : in out GL.IO.Input_buffer; n : out Number) is
    b : U8;
    m : Number := 1;
    bytes : constant Integer := Number'Size / 8;
  begin
    n := 0;
    for i in 1 .. bytes loop
      GL.IO.Get_Byte (gsb, b);
      n := n + m * Number (b);
      m := m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Read_Double
    (sb : in out GL.IO.Input_buffer;
     n  :    out GL.Double)
  is
    procedure Read_Intel is new Read_Intel_x86_number (U16);
    procedure Read_Intel is new Read_Intel_x86_number (U32);
    m1, m2 : U32;
    e : U16;
  begin
    Read_Intel (sb, m1);
    Read_Intel (sb, m2);
    Read_Intel (sb, e);
    Merge (Cvt (m1), Cvt (m2), Cvt (e), n);
    --  Double is stored in two parts due to the absence of
    --  64-bit integers on certain compilers.
  end Read_Double;

  generic
    s : Ada.Streams.Stream_IO.Stream_Access;
    type Number is mod <>;
  procedure Write_Intel_x86_number (n : in Number);

  procedure Write_Intel_x86_number (n : in Number) is
    m : Number := n;
    bytes : constant Integer := Number'Size / 8;
  begin
    for i in 1 .. bytes loop
      U8'Write (s, U8 (m mod 256));
      m := m / 256;
    end loop;
  end Write_Intel_x86_number;

  procedure Write_Double
    (s :    Ada.Streams.Stream_IO.Stream_Access;
     n : in GL.Double)
  is
    procedure Write_Intel is new Write_Intel_x86_number (s, U16);
    procedure Write_Intel is new Write_Intel_x86_number (s, U32);
    m1, m2 : I32;
    e : I16;
  begin
    Split (n, m1, m2, e);
    --  Double is stored in two parts due to the absence of
    --  64-bit integers on certain compilers.
    Write_Intel (Cvt (m1));
    Write_Intel (Cvt (m2));
    Write_Intel (Cvt (e));
  end Write_Double;

  procedure Write_String
    (s  : in  Ada.Streams.Stream_IO.Stream_Access;
     str : in  String)
  is
    tstr : constant String := Trim (str, Right);
  begin
    U8'Write (s, tstr'Length);
    String'Write (s, tstr);
  end Write_String;

  procedure Read_String
    (sb  : in out GL.IO.Input_buffer;
     str :    out String)
  is
    l8 : U8;
    l : Natural;
  begin
    GL.IO.Get_Byte (sb, l8);
    l := Natural (l8);
    if l > str'Length then
      raise Constraint_Error with
        "String length in data is" & l'Image &
        ", exceeds target string length" & str'Length'Image;
    end if;
    for i in str'First .. str'First + l - 1 loop
      GL.IO.Get_Byte (sb, l8);
      str (i) := Character'Val (l8);
    end loop;
    str (str'First + l .. str'Last) := (others => ' ');
  end Read_String;

  -------------------
  -- Object_3D I/O --
  -------------------

  procedure Read (
    s : in  Ada.Streams.Stream_IO.Stream_Access;
    o : out p_Object_3D
  )
  is

    buf : GL.IO.Input_buffer;

    procedure Read_Intel is new Read_Intel_x86_number (U16);
    procedure Read_Intel is new Read_Intel_x86_number (U32);

    procedure Read_Float (n : out GL.Float) is
      m : U32; e : U16;
    begin
      Read_Intel (buf, m);
      Read_Intel (buf, e);
      Merge (Cvt (m), Cvt (e), n);
    end Read_Float;

    procedure Read_Material (mfv : out GL.Material_Float_vector) is
    begin
      for i in mfv'Range loop
        Read_Float (mfv (i));
      end loop;
    end Read_Material;

    procedure Read_Point_3D (p : out Point_3D) is
    begin
      for i in p'Range loop
        Read_Double (buf, p (i));
      end loop;
    end Read_Point_3D;

    procedure Read_Pair_Array (m : out Map_Idx_Pair_Array) is
    begin
      for i in m'Range loop
        Read_Double (buf, m (i).U);
        Read_Double (buf, m (i).V);
      end loop;
    end Read_Pair_Array;

    v8 : U8;
    v32, mp32, mf32 : U32;
    with_specular : Boolean := True;
    with_sub_objects : Boolean := True;

    procedure Read_Face (face : out Face_Type; face_invar : in out Face_Internal_Type) is
    begin
      --  1/ Points
      for i in face.P'Range loop
        Read_Intel (buf, v32);
        face.P (i) := Integer (v32);
      end loop;
      --  2/ Portal connection: object name is stored;
      --    access must be found later
      Read_String (buf, face_invar.connect_name);
      --  3/ Skin
      GL.IO.Get_Byte (buf, v8);
      face.skin := Skin_Type'Val (v8);
      --  4/ Mirror
      GL.IO.Get_Byte (buf, v8);
      face.mirror := Boolean'Val (v8);
      --  5/ Alpha
      Read_Double (buf, face.alpha);
      --  6/ Colour
      case face.skin is
        when colour_only | coloured_texture =>
          Read_Double (buf, face.colour.red);
          Read_Double (buf, face.colour.green);
          Read_Double (buf, face.colour.blue);
        when others =>
          null;
      end case;
      --  7/ Material
      case face.skin is
        when material_only | material_texture =>
          Read_Material (face.material.ambient);
          Read_Material (face.material.diffuse);
          Read_Material (face.material.specular);
          Read_Material (face.material.emission);
          Read_Float (face.material.shininess);
        when others =>
          null;
      end case;
      --  8/ Texture: texture name is stored; id must be found later
      Read_String (buf, face_invar.texture_name);
      if with_specular then
        Read_String (buf, face_invar.specular_name);
      end if;
      GL.IO.Get_Byte (buf, v8);
      face.whole_texture := Boolean'Val (v8);
      GL.IO.Get_Byte (buf, v8);
      face.repeat_U := Positive'Val (v8);
      GL.IO.Get_Byte (buf, v8);
      face.repeat_V := Positive'Val (v8);
      if not face.whole_texture then
        Read_Pair_Array (face.texture_edge_map);
      end if;
    end Read_Face;
    --
    test_signature : String (signature_obj'Range);
    ID : Ident;
  begin
    String'Read (s, test_signature);
    if test_signature = signature_obj then
      null;
    elsif test_signature = signature_obj_v2016a then
      with_sub_objects := False;
    elsif test_signature = signature_obj_v2008 then
      with_sub_objects := False;
      with_specular    := False;
    else
      raise Bad_data_format with "Signature not found: " & signature_obj_root;
    end if;
    GL.IO.Attach_Stream (b => buf, stm => s);
    Read_String (buf, ID);
    --  Read the object's dimensions, create object, read its contents
    Read_Intel (buf, mp32);
    Read_Intel (buf, mf32);
    o := new Object_3D (Integer (mp32), Integer (mf32));
    o.ID := ID;
    for p in o.point'Range loop
      Read_Point_3D (o.point (p));
    end loop;
    for f in o.face'Range loop
      Read_Face (o.face (f), o.face_internal (f));
    end loop;
    Read_Point_3D (o.centre);
    for i in Matrix_33'Range (1) loop
      for j in Matrix_33'Range (2) loop
        Read_Double (buf, o.rotation (i, j));
      end loop;
    end loop;
    if with_sub_objects then
      loop
        GL.IO.Get_Byte (buf, v8);
        exit when Boolean'Val (v8);
        Read_String (buf, ID);
        o.sub_obj_ids.Append (ID);
      end loop;
    end if;
    --  Main operation done!
  end Read;

  procedure Write
    (s : in  Ada.Streams.Stream_IO.Stream_Access;
     o : in  Object_3D)
  is

    procedure Write_Intel is new Write_Intel_x86_number (s, U16);
    procedure Write_Intel is new Write_Intel_x86_number (s, U32);

    procedure Write_Float (n : in GL.Float) is
      m : I32; e : I16;
    begin
      Split (n, m, e);
      Write_Intel (Cvt (m));
      Write_Intel (Cvt (e));
    end Write_Float;

    procedure Write_Material (mfv : in GL.Material_Float_vector) is
    begin
      for i in mfv'Range loop
        Write_Float (mfv (i));
      end loop;
    end Write_Material;

    procedure Write_Point_3D (p : in Point_3D) is
    begin
      for i in p'Range loop
        Write_Double (s, p (i));
      end loop;
    end Write_Point_3D;

    procedure Write_Map_idx_pair_array (m : in Map_Idx_Pair_Array) is
    begin
      for i in m'Range loop
        Write_Double (s, m (i).U);
        Write_Double (s, m (i).V);
      end loop;
    end Write_Map_idx_pair_array;

    procedure Write_Face (face : Face_Type; face_invar : Face_Internal_Type) is
    begin
      --  1/ Points
      for i in face.P'Range loop
        Write_Intel (U32 (face.P (i)));
      end loop;
      --  2/ Portal connection: object name is stored
      if face.connecting = null then
        Write_String (s, empty_ident);
      else
        Write_String (s, face.connecting.ID);
      end if;
      --  3/ Skin
      U8'Write (s, Skin_Type'Pos (face.skin));
      --  4/ Mirror
      U8'Write (s, Boolean'Pos (face.mirror));
      --  5/ Alpha
      Write_Double (s, face.alpha);
      --  6/ Colour
      case face.skin is
        when colour_only | coloured_texture =>
          Write_Double (s, face.colour.red);
          Write_Double (s, face.colour.green);
          Write_Double (s, face.colour.blue);
        when others =>
          null;
      end case;
      --  7/ Material
      case face.skin is
        when material_only | material_texture =>
          Write_Material (face.material.ambient);
          Write_Material (face.material.diffuse);
          Write_Material (face.material.specular);
          Write_Material (face.material.emission);
          Write_Float (face.material.shininess);
        when others =>
          null;
      end case;
      --  8/ Texture: texture name is stored
      --  First, the main (diffuse) bitmap.
      if face.texture = null_image then
        --  Maybe a texture name has been given with Texture_name_hint,
        --  but was not yet attached to a GL ID number through Rebuild_Links (e.g., the d3g tool).
        --  In doubt, we give the hinted name (better than losing that information).
        Write_String (s, face_invar.texture_name);
      else
        --  Usual way: We can get the texture name associated to the
        --  GL ID number; name is stored by GLOBE_3D.Textures.
        Write_String (s, Textures.Texture_Name (face.texture, trim_spaces => False));
      end if;
      --  Next, the specular map.
      if face.specular_map = null_image then
        Write_String (s, face_invar.specular_name);
      else
        Write_String (s, Textures.Texture_Name (face.specular_map, trim_spaces => False));
      end if;
      U8'Write (s, Boolean'Pos (face.whole_texture));
      U8'Write (s, Positive'Pos (face.repeat_U));
      U8'Write (s, Positive'Pos (face.repeat_V));
      if not face.whole_texture then
        Write_Map_idx_pair_array (face.texture_edge_map);
      end if;
    end Write_Face;

    sub_obj_lst : p_Object_3D_List := o.sub_objects;
    last : Boolean;

  begin
    String'Write (s, signature_obj);
    Write_String (s, o.ID);
    Write_Intel (U32 (o.Max_points));
    Write_Intel (U32 (o.Max_faces));
    for p in o.point'Range loop
      Write_Point_3D (o.point (p));
    end loop;
    for f in o.face'Range loop
      Write_Face (o.face (f), o.face_internal (f));
    end loop;
    Write_Point_3D (o.centre);
    for i in Matrix_33'Range (1) loop
      for j in Matrix_33'Range (2) loop
        Write_Double (s, o.rotation (i, j));
      end loop;
    end loop;
    loop
      last := sub_obj_lst = null;
      U8'Write (s, Boolean'Pos (last));
      exit when last;
      Write_String (s, sub_obj_lst.objc.ID);
      sub_obj_lst := sub_obj_lst.next;
    end loop;
    --  Main operation done!
  end Write;

  generic
    type Anything is private;
    extension : String;
    description : String;
    with procedure Reader
      (s : in    Ada.Streams.Stream_IO.Stream_Access;
       a :   out Anything);
  procedure Load_Generic (name_in_resource_g : String; a : out Anything);

  procedure Load_Generic (name_in_resource_g : String; a : out Anything) is
    name_ext : constant String := Trim (name_in_resource_g, Both) & extension;

    procedure Try (zif : in out Zip.Zip_info; name : String) is
      use UnZip.Streams;
      fobj : Zipped_File_Type;
    begin
      Load_if_needed (zif, name);
      Open (fobj, zif, name_ext);
      Reader (Ada.Streams.Stream_IO.Stream_Access (Stream (fobj)), a);
      Close (fobj);
    exception
      when Zip.Entry_name_not_found =>
        raise;
      when e : others =>
        Raise_Exception
          (Exception_Identity (e),
           Exception_Message (e) & " on reading " & description & ": " & name_ext);
    end Try;
  begin
    begin
      Try (zif_level, S (level_data_name));
    exception
      when Zip.Entry_name_not_found |
           Zip.Archive_open_error =>
        --  Not found in level-specific pack
        Try (zif_global, S (global_data_name));
    end;
  exception
    when Zip.Entry_name_not_found |
         Zip.Archive_open_error =>
      --  Never found - neither in level, nor in global pack
      raise Missing_object with
        description & " not found in any data resource pack: " & name_in_resource_g;
  end Load_Generic;

  procedure Load_Internal is
    new Load_Generic
      (Anything    => p_Object_3D,
       extension   => object_extension,
       description => "object",
       Reader      => Read
    );

  procedure Load (name_in_resource : String; o : out p_Object_3D)
  renames Load_Internal;

  procedure Load_File (file_name : String; o : out p_Object_3D) is
    use Ada.Streams.Stream_IO;
    f : File_Type;
  begin
    Open (f, In_File, file_name);
    Read (Stream (f), o);
    Close (f);
  end Load_File;

  procedure Save_File (file_name : String; o : in Object_3D'Class) is
    use Ada.Streams.Stream_IO;
    f : File_Type;
  begin
    Create (f, Out_File, file_name);
    Write (Stream (f), Object_3D (o));
    --  ^ endian-proof and floating-point hardware neutral;
    --    using stream attribute would be machine-specific.
    Close (f);
  end Save_File;

  procedure Save_File (o : in Object_3D'Class) is
  begin
    Save_File (Trim (o.ID, Right) & object_extension, o);
  end Save_File;

  ---------------
  --  BSP I/O  --
  ---------------

  --  Write a BSP tree to a stream

  procedure Write
    (s    : in  Ada.Streams.Stream_IO.Stream_Access;
     tree : in BSP.p_BSP_Node)
  is
    procedure Write_Intel is new Write_Intel_x86_number (s, U32);
    use BSP;

    n : Natural := 0;

    procedure Numbering (node : p_BSP_Node) is
    begin
      if node /= null then
        n := n + 1;
        node.node_id := n;
        Numbering (node.front_child);
        Numbering (node.back_child);
      end if;
    end Numbering;

    procedure Save_Node (node : p_BSP_Node) is
    begin
      if node /= null then
        Write_Intel (U32 (node.node_id));
        if node.front_child = null then
          Write_Intel (U32'(0)); --  Leaf nodes have index 0.
          if node.front_leaf = null then
            Write_String (s, empty_ident);
          else
            Write_String (s, node.front_leaf.ID);
          end if;
        else
          Write_Intel (U32 (node.front_child.node_id));
        end if;
        if node.back_child = null then
          Write_Intel (U32'(0)); --  Leaf nodes have index 0.
          if node.back_leaf = null then
            Write_String (s, empty_ident);
          else
            Write_String (s, node.back_leaf.ID);
          end if;
        else
          Write_Intel (U32 (node.back_child.node_id));
        end if;
        for i in node.normal'Range loop
          Write_Double (s, node.normal (i));
        end loop;
        Write_Double (s, node.distance);
        --
        Save_Node (node.front_child);
        Save_Node (node.back_child);
      end if;
    end Save_Node;

  begin
    Numbering (tree);                 -- fill the node_id's
    String'Write (s, signature_bsp);  -- header
    Write_Intel (U32 (n));            -- give the number of nodes first
    Save_Node (tree);
  end Write;

  --  Write a BSP tree to a file

  procedure Save_File (file_name : String; tree : in BSP.p_BSP_Node) is
    use Ada.Streams.Stream_IO;
    f : File_Type;
  begin
    if Index (file_name, ".") = 0 then
      Create (f, Out_File, file_name & BSP_extension);
    else
      Create (f, Out_File, file_name);
    end if;
    Write (Stream (f), tree);
    Close (f);
  end Save_File;

  procedure Load
    (name_in_resource : in  String;
     referred         : in  Map_of_Visuals;
     tree             : out BSP.p_BSP_Node)
  is

    function Find_Object (ID : Ident; tolerant : Boolean) return p_Object_3D is
      use Visuals_Mapping;
      c : Cursor;
    begin
      if ID = empty_ident then
        return null;
      end if;
      c := referred.Find (U (ID));
      if c = No_Element then
        --  Key not found
        if tolerant then
          return null;
        else
          raise Missing_object_in_BSP with "Object not found: [" & Trim (ID, Right) & ']';
        end if;
      else
        return p_Object_3D (Element (c));
      end if;
    end Find_Object;

    procedure Read_BSP
      (s        : in    Ada.Streams.Stream_IO.Stream_Access;
       bsp_tree :   out BSP.p_BSP_Node)
    is
      use BSP;
      buf : GL.IO.Input_buffer;
      procedure Read_Intel is new Read_Intel_x86_number (U32);

      test_signature : String (signature_bsp'Range);
      n, j, k : U32;
      ID : Ident;
      tol : constant Boolean := False;
    begin
      String'Read (s, test_signature);
      if test_signature /= signature_bsp then
        raise Bad_data_format;
      end if;
      GL.IO.Attach_Stream (b => buf, stm => s);
      Read_Intel (buf, n);
      if n < 1 then
        bsp_tree := null;
        return;
      end if;
      declare
        --  We put all the new-born nodes into a farm with numbered boxes,
        --  because only the numbers are stored in the BSP file.
        --  Once the nodes are linked together through accesses (pointers),
        --  we can forget the farm and let the tree float...
        farm : array (0 .. n) of p_BSP_Node;
      begin
        farm (0) := null;  --  Leaf nodes have index 0.
        for i in 1 .. n loop
          farm (i) := new BSP_Node;
        end loop;
        for i in 1 .. n loop
          Read_Intel (buf, j);  --  node_id
          farm (j).node_id := Integer (j);
          --  Front child:
          Read_Intel (buf, k);
          farm (j).front_child := farm (k);  --  Connect with an access.
          if k = 0 then  -- it is a leaf (front side) -> associate object
            Read_String (buf, ID);
            farm (j).front_leaf := Find_Object (ID, tol);
          end if;
          --  Back child:
          Read_Intel (buf, k);
          farm (j).back_child := farm (k);  --  Connect with an access.
          if k = 0 then  --  it is a leaf (back side) -> associate object
            Read_String (buf, ID);
            farm (j).back_leaf := Find_Object (ID, tol);
          end if;
          --  The node's geometric information (a plane):
          for ii in farm (j).normal'Range loop
            Read_Double (buf, farm (j).normal (ii));
          end loop;
          Read_Double (buf, farm (j).distance);
        end loop;
        bsp_tree := farm (1);
      end;
    end Read_BSP;

    procedure Load_Internal is
      new Load_Generic
        (Anything    => BSP.p_BSP_Node,
         extension   => BSP_extension,
         description => "BSP tree",
         Reader      => Read_BSP);

  begin
    Load_Internal (name_in_resource, tree);
  end Load;

end GLOBE_3D.IO;
