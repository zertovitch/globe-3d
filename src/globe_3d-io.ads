----------------------------------------------------
--  Input and Output of 3D objects and BSP trees  --
----------------------------------------------------

--  NB: after reading/loading an object or a group of objects, don't
--      forget to use Rebuild_Links for linking them together and
--      finding texture id's

with GLOBE_3D.BSP;

with Ada.Streams.Stream_IO;

package GLOBE_3D.IO is

  ------------------------------
  -- I/O from/to data streams --
  ------------------------------

  --  Allocate and read an object from a data stream

  procedure Read
    (s : in  Ada.Streams.Stream_IO.Stream_Access;
     o : out p_Object_3D);

  --  Write an object to a data stream

  procedure Write
    (s : in  Ada.Streams.Stream_IO.Stream_Access;
     o : in  Object_3D);

  -----------------------
  -- I/O from/to files --
  -----------------------

  object_extension : constant String := ".g3d";

  --  Allocate and read an object from a file

  procedure Load_File (file_name : String; o : out p_Object_3D);

  --  Write an object to a file

  procedure Save_File (file_name : String; o : in Object_3D'class);

  --  Write an object to a file, using the object's ID as file name

  procedure Save_File (o : in Object_3D'class);

  BSP_extension : constant String := ".bsp";

  --  Write a BSP tree to a file

  procedure Save_File (file_name : String; tree : in BSP.p_BSP_Node);

  ---------------------------------------------------------------
  --  Input from files archived into a GLOBE_3D resource file  --
  ---------------------------------------------------------------

  --  Allocate and read an object from the Level or,
  --  when not there, from the Global data resource

  procedure Load (name_in_resource : String; o : out p_Object_3D);

  --  Allocate and read an BSP tree from a file.
  --
  --  The procedure uses the dictionary of objects in 'referred' to
  --  put the right accesses to 3D visuals on the tree's leaves.
  --  From the file, we know only the visuals' names.
  --
  --  So before loading the BSP tree, you need first to load all
  --  visuals that are referred to in the BSP tree, and build a
  --  dictionary (map). You can use the GLOBE_3D.Add procedure
  --  to do that.

  procedure Load
    (name_in_resource : in  String;
     referred         : in  Map_of_Visuals;
     tree             : out BSP.p_BSP_Node);

  Bad_data_format : exception;

  Missing_object_in_BSP : exception;

end GLOBE_3D.IO;
