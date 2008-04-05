with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
-- with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with UnZip.Streams;
with Float_portable_binary_transfer;
pragma Elaborate_All(Float_portable_binary_transfer);

with GLOBE_3D.Textures;

package body GLOBE_3D.IO is

  ------------------------------------------------
  -- Common, internal definitions, routines,... --
  ------------------------------------------------

  stop_type: constant Character:=
    Character'Val(26); -- Ctrl-Z to stop typing a binary file

  signature_obj: constant String:=
    "GLOBE_3D 3D Binary Object File (" & object_extension & "). " &
    "Format version: 2-Apr-2008." & stop_type;

  signature_bsp: constant String:=
    "GLOBE_3D Binary Space Partition File (" & BSP_extension & "). " &
    "Format version: 2-Apr-2008." & stop_type;

  type U8  is mod 2 ** 8;   for U8'Size  use 8;
  type U16 is mod 2 ** 16;  for U16'Size use 16;
  type U32 is mod 2 ** 32;  for U32'Size use 32;

  type I16 is range -2 ** 15 .. 2 ** 15 - 1; for I16'Size use 16;
  type I32 is range -2 ** 31 .. 2 ** 31 - 1; for I32'Size use 32;

  f_scaling: constant:= 2.0**24;
  package FFBT is
    new Float_portable_binary_transfer(GL.Float,I32,I16,True,f_scaling);
  use FFBT;
  d_scaling: constant:= 2.0**27; -- 53/2=26.5
  package DFBT is
    new Float_portable_binary_transfer(GL.Double,I32,I16,True,d_scaling);
  use DFBT;

  function Cvt is new Ada.Unchecked_Conversion( I16, U16 );
  function Cvt is new Ada.Unchecked_Conversion( I32, U32 );
  function Cvt is new Ada.Unchecked_Conversion( U16, I16 );
  function Cvt is new Ada.Unchecked_Conversion( U32, I32 );

  generic
    s: Ada.Streams.Stream_IO.Stream_Access;
    type Number is mod <>;
  procedure Read_Intel_x86_number(n: out Number);

  procedure Read_Intel_x86_number(n: out Number) is
    b: U8;
    m: Number:= 1;
    bytes: constant Integer:= Number'Size/8;
  begin
    n:= 0;
    for i in 1..bytes loop
      U8'Read(s,b);
      n:= n + m * Number(b);
      m:= m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Read_Double(
    s: Ada.Streams.Stream_IO.Stream_Access;
    n: out GL.Double
  ) is
    procedure Read_Intel is new Read_Intel_x86_number( s, U16 );
    procedure Read_Intel is new Read_Intel_x86_number( s, U32 );
    m1,m2: U32; e: U16;
  begin
    Read_Intel(m1);
    Read_Intel(m2);
    Read_Intel(e);
    Merge(Cvt(m1),Cvt(m2),Cvt(e),n);
    -- Double is stored in two parts due to the absence of
    -- 64-bit integers on certain compilers (e.g. OA 8.2)
  end Read_Double;

  generic
    s: Ada.Streams.Stream_IO.Stream_Access;
    type Number is mod <>;
  procedure Write_Intel_x86_number(n: in Number);

  procedure Write_Intel_x86_number(n: in Number) is
    m: Number:= n;
    bytes: constant Integer:= Number'Size/8;
  begin
    for i in 1..bytes loop
      U8'Write(s, U8(m mod 256));
      m:= m / 256;
    end loop;
  end Write_Intel_x86_number;

  procedure Write_Double(
    s: Ada.Streams.Stream_IO.Stream_Access;
    n: in GL.Double)
  is
    procedure Write_Intel is new Write_Intel_x86_number( s, U16 );
    procedure Write_Intel is new Write_Intel_x86_number( s, U32 );
    m1,m2: I32; e: I16;
  begin
    Split(n,m1,m2,e);
    -- Double is stored in two parts due to the absence of
    -- 64-bit integers on certain compilers (e.g. OA 8.2)
    Write_Intel(Cvt(m1));
    Write_Intel(Cvt(m2));
    Write_Intel(Cvt(e));
  end Write_Double;

  procedure Write_String(
    s  : in  Ada.Streams.Stream_IO.Stream_Access;
    str: in  String
  )
  is
    tstr: constant String:= Trim(str,right);
  begin
    U8'Write(s,tstr'Length);
    String'Write(s,tstr);
  end Write_String;

  procedure Read_String(
    s  : in  Ada.Streams.Stream_IO.Stream_Access;
    str: out String
  )
  is
    l8: U8;
    l: Natural;
  begin
    U8'Read(s,l8);
    l:= Natural(l8);
    if l > str'Length then
      raise Constraint_Error;
    end if;
    String'Read(s,str(str'First..str'First+l-1));
    str(str'First+l..str'Last):= (others => ' ');
  end Read_String;

  -------------------
  -- Object_3D I/O --
  -------------------

  procedure Read(
    s: in  Ada.Streams.Stream_IO.Stream_Access;
    o: out p_Object_3D
  )
  is

    procedure Read_Intel is new Read_Intel_x86_number( s, U16 );
    procedure Read_Intel is new Read_Intel_x86_number( s, U32 );

    procedure Read_Float(n: out GL.Float) is
      m: U32; e: U16;
    begin
      Read_Intel(m);
      Read_Intel(e);
      Merge(Cvt(m),Cvt(e),n);
    end Read_Float;

    procedure Read_Material_Float_vector(mfv: out GL.Material_Float_vector) is
    begin
      for i in mfv'Range loop
        Read_Float(mfv(i));
      end loop;
    end Read_Material_Float_vector;

    procedure Read_Point_3D(p: out Point_3D) is
    begin
      for i in p'Range loop
        Read_Double(s, p(i));
      end loop;
    end Read_Point_3D;

    procedure Read_Map_idx_pair_array(m: out Map_idx_pair_array) is
    begin
      for i in m'Range loop
        Read_Double(s, m(i).U);
        Read_Double(s, m(i).V);
      end loop;
    end Read_Map_idx_pair_array;

    v8: U8;
    v32, mp32, mf32: U32;

    procedure Read_face(face: out Face_type; face_invar: in out Face_invariant_type) is
    begin
      -- 1/ Points
      for i in face.p'Range loop
        Read_Intel(v32);
        face.p(i):= Integer(v32);
      end loop;
      -- 2/ Portal connection: object name is stored;
      --    access must be found later
      Read_String(s,face_invar.connect_name);
      -- 3/ Skin
      U8'Read(s,v8);
      face.skin:= skin_type'Val(v8);
      -- 4/ Mirror
      U8'Read(s,v8);
      face.mirror:= Boolean'Val(v8);
      -- 5/ Alpha
      Read_Double(s, face.alpha);
      -- 6/ Colour
      case face.skin is
        when colour_only | coloured_texture =>
          Read_Double(s, face.colour.red);
          Read_Double(s, face.colour.green);
          Read_Double(s, face.colour.blue);
        when others =>
          null;
      end case;
      -- 7/ Material
      case face.skin is
        when material_only | material_texture =>
          Read_Material_Float_vector(face.material.ambient);
          Read_Material_Float_vector(face.material.diffuse);
          Read_Material_Float_vector(face.material.specular);
          Read_Material_Float_vector(face.material.emission);
          Read_Float(face.material.shininess);
        when others =>
          null;
      end case;
      -- 8/ Texture: texture name is stored;
      --    id must be found later
      Read_String(s, face_invar.texture_name);
      U8'Read(s,v8);
      face.whole_texture:= Boolean'Val(v8);
      U8'Read(s,v8);
      face.repeat_U:= Positive'Val(v8);
      U8'Read(s,v8);
      face.repeat_V:= Positive'Val(v8);
      if not face.whole_texture then
        Read_Map_idx_pair_array(face.texture_edge_map);
      end if;
    end Read_face;
    test_signature: String(signature_obj'Range);
    ID: Ident;
  begin
    String'Read(s,test_signature);
    if test_signature /= signature_obj then
      raise Bad_data_format;
    end if;
    Read_String(s,ID);
    -- Read the object's dimensions, create object, read its contents
    Read_Intel(mp32);
    Read_Intel(mf32);
    o:= new Object_3D(Integer(mp32), Integer(mf32));
    o.ID:= ID;
    for p in o.point'Range loop
      Read_Point_3D(o.point(p));
    end loop;
    for f in o.face'Range loop
      Read_face(o.face(f),o.face_invariant(f));
    end loop;
    Read_Point_3D(o.centre);
    for i in Matrix_33'Range(1) loop
      for j in Matrix_33'Range(2) loop
        Read_Double(s, o.rotation(i,j));
      end loop;
    end loop;
    -- !! sub-objects: skipped !!
    -- Main operation done!
  end Read;

  procedure Write(
    s: in  Ada.Streams.Stream_IO.Stream_Access;
    o: in  Object_3D
  )
  is

    procedure Write_Intel is new Write_Intel_x86_number( s, U16 );
    procedure Write_Intel is new Write_Intel_x86_number( s, U32 );

    procedure Write_Float(n: in GL.Float) is
      m: I32; e: I16;
    begin
      Split(n,m,e);
      Write_Intel(Cvt(m));
      Write_Intel(Cvt(e));
    end Write_Float;

    procedure Write_Material_Float_vector(mfv: in GL.Material_Float_vector) is
    begin
      for i in mfv'Range loop
        Write_Float(mfv(i));
      end loop;
    end Write_Material_Float_vector;

    procedure Write_Point_3D(p: in Point_3D) is
    begin
      for i in p'Range loop
        Write_Double(s, p(i));
      end loop;
    end Write_Point_3D;

    procedure Write_Map_idx_pair_array(m: in Map_idx_pair_array) is
    begin
      for i in m'Range loop
        Write_Double(s, m(i).U);
        Write_Double(s, m(i).V);
      end loop;
    end Write_Map_idx_pair_array;

    procedure Write_face(face: Face_type) is
    begin
      -- 1/ Points
      for i in face.p'Range loop
        Write_Intel(U32(face.p(i)));
      end loop;
      -- 2/ Portal connection: object name is stored
      if face.connecting = null then
        Write_String(s, empty);
      else
        Write_String(s, face.connecting.ID);
      end if;
      -- 3/ Skin
      U8'Write(s,skin_type'Pos(face.skin));
      -- 4/ Mirror
      U8'Write(s,Boolean'Pos(face.mirror));
      -- 5/ Alpha
      Write_Double(s, face.alpha);
      -- 6/ Colour
      case face.skin is
        when colour_only | coloured_texture =>
          Write_Double(s, face.colour.red);
          Write_Double(s, face.colour.green);
          Write_Double(s, face.colour.blue);
        when others =>
          null;
      end case;
      -- 7/ Material
      case face.skin is
        when material_only | material_texture =>
          Write_Material_Float_vector(face.material.ambient);
          Write_Material_Float_vector(face.material.diffuse);
          Write_Material_Float_vector(face.material.specular);
          Write_Material_Float_vector(face.material.emission);
          Write_Float(face.material.shininess);
        when others =>
          null;
      end case;
      -- 8/ Texture: texture name is stored
      if face.texture = null_image then
        Write_String(s, empty);
      else
        Write_String(s, Textures.Texture_name(face.texture,False));
      end if;
      U8'Write(s,Boolean'Pos(face.whole_texture));
      U8'Write(s,Positive'Pos(face.repeat_U));
      U8'Write(s,Positive'Pos(face.repeat_V));
      if not face.whole_texture then
        Write_Map_idx_pair_array(face.texture_edge_map);
      end if;
    end Write_face;

  begin
    String'Write(s, signature_obj);
    Write_String(s, o.ID);
    Write_Intel(U32(o.Max_points));
    Write_Intel(U32(o.Max_faces));
    for p in o.point'Range loop
      Write_Point_3D(o.point(p));
    end loop;
    for f in o.face'Range loop
      Write_face(o.face(f));
    end loop;
    Write_Point_3D(o.centre);
    for i in Matrix_33'Range(1) loop
      for j in Matrix_33'Range(2) loop
        Write_Double(s, o.rotation(i,j));
      end loop;
    end loop;
    -- !! sub-objects: skipped !!
    -- Main operation done!
  end Write;

  generic
    type Anything is private;
    extension: String;
    animal: String;
    with procedure Read(
      s: in  Ada.Streams.Stream_IO.Stream_Access;
      a: out Anything
    );
  procedure Load_generic(name_in_resource: String; a: out Anything);

  procedure Load_generic(name_in_resource: String; a: out Anything) is
    name_ext: constant String:= name_in_resource & extension;

    procedure Try( zif: in out Zip.Zip_info; name: String ) is
      use Unzip.Streams;
      fobj: Zipped_File_Type;
    begin -- Try
      Load_if_needed( zif, name );
      Open( fobj, zif, name_ext );
      Read( Stream(fobj), a );
      Close( fobj );
    exception
      when Zip.File_name_not_found =>
        raise;
      when e:others =>
        raise_exception(
          Exception_Identity(e),
          Exception_Message(e) & " on " & animal & ": " & name_ext
        );
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
      raise_exception(
        Missing_object'Identity,
        animal & " not found in any data resource pack: " & name_in_resource
      );
  end Load_generic;

  procedure Load_Internal is
    new Load_generic(
      Anything  => p_Object_3D,
      extension => object_extension,
      animal    => "object",
      Read      => Read
    );

  procedure Load(name_in_resource: String; o: out p_Object_3D)
  renames Load_Internal;

  procedure Load_file(file_name: String; o: out p_Object_3D) is
    use Ada.Streams.Stream_IO;
    f: File_Type;
  begin
    Open(f, in_file, file_name);
    Read(Stream(f),o);
    Close(f);
  end Load_file;

  procedure Save_file(file_name: String; o: in Object_3D'class) is
    use Ada.Streams.Stream_IO;
    f: File_Type;
  begin
    Create(f, out_file, file_name);

--      if o in Object_3D'class then
      Write(Stream(f), Object_3D (o));         -- endian-proof
--      else
--        Object_base'class'output (Stream(f), o); -- !! not yet endian-proof (tbd)
--      end if;

    Close(f);
  end Save_file;

  procedure Save_file(o: in Object_3D'class) is
  begin
    Save_file(Trim(o.ID,right) & object_extension, o);
  end Save_file;

  -------------
  -- BSP I/O --
  -------------

  -- Write a BSP tree to a stream

  procedure Write(
    s: in  Ada.Streams.Stream_IO.Stream_Access;
    tree: in BSP.p_BSP_node
  )
  is
    procedure Write_Intel is new Write_Intel_x86_number( s, U32 );
    use BSP;

    n: Natural:= 0;

    procedure Numbering(node: p_BSP_node) is
    begin
      if node /= null then
        n:= n + 1;
        node.node_id:= n;
        Numbering(node.front_child);
        Numbering(node.back_child);
      end if;
    end Numbering;

    procedure Save_node(node: p_BSP_node) is
    begin
      if node /= null then
        Write_Intel(U32(node.node_id));
        if node.front_child = null then
          Write_Intel(U32'(0));
          if node.front_leaf = null then
            Write_String(s, empty);
          else
            Write_String(s, node.front_leaf.ID);
          end if;
        else
          Write_Intel(U32(node.front_child.node_id));
        end if;
        if node.back_child = null then
          Write_Intel(U32'(0));
          if node.back_leaf = null then
            Write_String(s, empty);
          else
            Write_String(s, node.back_leaf.ID);
          end if;
        else
          Write_Intel(U32(node.back_child.node_id));
        end if;
        for i in node.normal'Range loop
          Write_Double(s, node.normal(i));
        end loop;
        Write_Double(s, node.distance);
        --
        Save_node(node.front_child);
        Save_node(node.back_child);
      end if;
    end Save_node;

  begin
    Numbering(tree);                -- fill the node_id's
    String'Write(s, signature_bsp); -- header
    Write_Intel(U32(n));           -- give the number of nodes first
    Save_node(tree);
  end Write;

  -- Write a BSP tree to a file

  procedure Save_file(file_name: String; tree: in BSP.p_BSP_node) is
    use Ada.Streams.Stream_IO;
    f: File_Type;
  begin
    if Index(file_name, ".")=0 then
      Create(f, out_file, file_name & BSP_extension);
    else
      Create(f, out_file, file_name);
    end if;
    Write(Stream(f),tree);
    Close(f);
  end Save_file;

  procedure Load(
    name_in_resource: in  String;
    referred        : in  Map_of_Visuals;
    tree            : out BSP.p_BSP_node
  )
  is

    function Find_object(ID: Ident; tolerant: Boolean) return p_Object_3D is
    begin
      if ID = empty then
        return null;
      else
        -- XX old linear search:
        --  for i in referred_objects'Range loop
        --    if referred_objects(i).ID = ID then
        --      return referred_objects(i);
        --    end if;
        --  end loop;
        return p_Object_3D(Visuals_Mapping.Element(
          Visuals_Mapping.Map(referred),
          Ada.Strings.Unbounded.To_Unbounded_String(ID))
        );
      end if;
    exception
      when Constraint_Error =>
        -- GNAT gives also the message:
        -- no element available because key not in map
        if tolerant then
          return null;
        else
          raise_exception(
            Missing_object_in_BSP'Identity,
            "Object not found: [" & Trim(ID,right) & ']'
          );
        end if;
    end Find_object;

    procedure Read(
      s           : in  Ada.Streams.Stream_IO.Stream_Access;
      tree        : out BSP.p_BSP_node
    )
    is
      use BSP;
      procedure Read_Intel is new Read_Intel_x86_number( s, U32 );

      test_signature: String(signature_bsp'Range);
      n, j, k: U32;
      ID: Ident;
      tol: constant Boolean:= False;
    begin
      String'Read(s,test_signature);
      if test_signature /= signature_bsp then
        raise Bad_data_format;
      end if;
      Read_Intel(n);
      if n < 1 then
        tree:= null;
        return;
      end if;
      declare
        farm: array (0..n) of p_BSP_Node;
      begin
        farm(0):= null;
        for i in 1..n loop
          farm(i):= new BSP_Node;
        end loop;
        for i in 1..n loop
          Read_Intel(j); -- node_id
          farm(j).node_id:= Integer(j);
          Read_Intel(k);
          farm(j).front_child:= farm(k);
          if k = 0 then
            Read_String(s, ID);
            farm(j).front_leaf:= Find_object(ID, tol);
          end if;
          Read_Intel(k);
          farm(j).back_child := farm(k);
          if k = 0 then
            Read_String(s, ID);
            farm(j).back_leaf := Find_object(ID, tol);
          end if;
          for ii in farm(j).normal'Range loop
            Read_Double(s, farm(j).normal(ii));
          end loop;
          Read_Double(s, farm(j).distance);
        end loop;
        tree:= farm(1);
      end;
    end Read;

    procedure Load_Internal is
      new Load_generic(
        Anything  => BSP.p_BSP_node,
        extension => BSP_extension,
        animal    => "BSP tree",
        Read      => Read
      );


  begin
    Load_Internal(name_in_resource, tree);
  end Load;

end GLOBE_3D.IO;
