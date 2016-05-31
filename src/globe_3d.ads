-------------------------------------------------------------------------
--  GLOBE_3D - GL-based, real-time, 3D engine
--
--  Copyright (c) Gautier de Montmollin 2001..2012
--  SWITZERLAND
--  Copyright (c) Rod Kay 2006..2008
--  AUSTRALIA
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

-------------------------------------------------------------------------

--
-- Added "List_status" and "List_Id" to the Object_3D.
-- by default the Display_One routine will now generate a GL command list
-- instead of sending the command each time explicitely.
-- To disable this feature, set Object_3D.List_Status to "No_List".
-- If memory is not sufficient to hold a list, the Display_One routine will
-- automatically default back to "No_List".
--
-- Uwe R. Zimmer, July 2011
--
--
-- Added an alternative
-- display face routine which is optimized to produce a shorter list
-- of GL commands. Runs slower than the original Display face routine
-- yet needs to be executed only once.
--
-- Uwe R. Zimmer, July 2011
--

with GL,
     GL.Geometry,
     GL.Frustums,
     GL.Skinned_Geometry,
     GL.Materials;

with Zip;

with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package GLOBE_3D is

  subtype Ident is String(1..40);
  -- Identifiers for naming things (textures, objects, ...)
  -- Identifiers are case insensitive and stored as UPPER_CASE

  empty: constant Ident:= (others=> ' ');

  -- Set the name of Zip archives containing the data.
  --
  -- If an item is not found in the level (local) data, it is
  -- searched in the global data. The idea is to set the global
  -- data once in the execution of the program, and change the local data
  -- upon context change (e.g., in a game, a change of level).
  procedure Set_local_data_name(s: String);
  procedure Set_level_data_name(s: String) renames Set_local_data_name;
  procedure Set_global_data_name(s: String);

  data_file_not_found: exception;

  -- List of textures ID's, correspond to files in
  -- the archives and to GL's "names"
  type Image_ID is new Integer range -1..Integer'Last;
  null_image: constant Image_ID:= -1;

  subtype Real is GL.Double;
  package REF is new Ada.Numerics.Generic_Elementary_functions(Real);
  package RIO is new Ada.Text_IO.Float_IO(Real);

  subtype Vector_3D is GL.Double_Vector_3D;
  type  p_Vector_3D is access all Vector_3D;

  type Vector_4D is array (0..3) of Real;

  subtype Point_3D is Vector_3D;

  type Matrix    is array (Positive range <>, Positive range <>) of aliased Real;
  type Matrix_33 is new Matrix(1..3,1..3);
  type Matrix_44 is new Matrix(1..4,1..4);

  Id_33: constant Matrix_33:=
   ( (1.0, 0.0, 0.0),
     (0.0, 1.0, 0.0),
     (0.0, 0.0, 1.0) );

  type Point_3D_array is array(Positive range <>) of aliased Point_3D;
  type p_Point_3D_array is access Point_3D_array;
  type Vector_3D_array is array(Natural range <>) of Vector_3D;

  type Index_array is array(Natural range <>) of aliased Natural;   -- tbd: make gl.unsigned_Int (or unsigned_Short)?

  ----------------------------------------------------------------
  -- Portal rendering definitions (methods in GLOBE_3D.Portals) --
  ----------------------------------------------------------------

  type Rectangle is record X1,Y1,X2,Y2: Integer; end record;

  subtype Clipping_area is Rectangle;

  -- ^ Cheap but fast portal culling & clipping method with rectangles.
  --   Usually, a bit too much is displayed.
  --   With graphics cards as of 2005+, it doesn't matter at all
  --   The important aspect is the culling of the objects when the
  --   intersection is empty.

  type Clipping_data is record
    eye_position    : aliased Point_3D;
    view_direction  : Vector_3D;
    max_dot_product : Real;         -- depends on the field of view
    main_clipping   : Clipping_area;
  end record;

  -- Camera
  --
   use type Real;

   fairly_Far                  : constant := 50_000.0;
   default_field_of_view_Angle : constant :=     55.0;

   type Camera is tagged
      record
         clipper             : Clipping_data:= (eye_position    => (0.0,  0.0,  5.0),
                                                view_direction  => (0.0,  0.0, -1.0),
                                                max_dot_product => 0.0,
                                                main_clipping   => (0, 0, 0, 0));
         world_rotation      : Matrix_33 := Id_33;
         speed               : Vector_3D := (0.0, 0.0, 0.0);
         rotation_speed      : Vector_3D := (0.0, 0.0, 0.0);
         compose_rotations   : Boolean:= True;
         -- True: apply successive rotations from rotation_Speed directly
         --       to world_Rotation. Good for totally free 3D movement, no gravity.
         --       Drawback: rotations around x axis, then y, then x,... induce a
         --       rotation around z (the nose) which is x rotated around y.
         -- False: world_Rotation is set as XYZ_rotation of the rotation vector below;
         --        x,y,z keep separate.
         -- Cf implementation in the package Actors
         rotation            : Vector_3D := (0.0, 0.0, 0.0);
         -- ^ this vector is updated, whatever the state of 'compose_rotations'

         FoVy                : Real  := default_field_of_view_Angle;  -- field of view angle (deg) in the y direction
         Aspect              : Real;                                  -- x/y aspect ratio

         near_plane_Distance : Real  := 1.0;                          -- distance to the near clipping plane
         near_plane_Width    : Real;
         near_plane_Height   : Real;

         far_plane_Distance  : Real  := fairly_Far;                   -- distance to the far clipping plane
         far_plane_Width     : Real;
         far_plane_Height    : Real;

         projection_matrix   : Matrix_44;

         frustum_planes      : GL.Frustums.plane_Array;
      end record;

   type p_Camera is access all Camera'Class;

   -- 'Visual' class hierarchy
   --

   type Visual is abstract tagged
      record
         ID                     : Ident:= "-Nameless-                              ";
         --                                1234567890123456789012345678901234567890

         centre                 : Point_3D  := (0.0, 0.0, 0.0); -- vertex coords are relative to the centre.
         centre_camera_space    : Point_3D;                     -- the visuals 'centre' in camera space.
         rotation               : Matrix_33 := Id_33;

         is_Terrain             : Boolean   := False;
      end record;

   type p_Visual is access all Visual'Class;
   type Visual_array is array (Positive range <>) of p_Visual;

   procedure destroy        (o     : in out Visual) is abstract;
   procedure free           (o     : in out p_Visual);

   procedure Pre_calculate  (o     : in out Visual)     is abstract;

   procedure set_Alpha      (o     : in out Visual;
                             Alpha : in     GL.Double)   is abstract;

   function  is_Transparent (o     : in     Visual) return Boolean   is abstract;
   --
   -- returns 'True' if any part of the 'visual' is potentially transparent.

   function face_Count (o : in Visual) return Natural                   is abstract;
   function Bounds     (o : in Visual) return GL.Geometry.Bounds_record is abstract;

   function skinned_Geometrys (o : in Visual) return GL.Skinned_Geometry.skinned_Geometrys;

   procedure Display (o          : in out Visual;
                      clip       : in     Clipping_data
   ) is abstract;

   procedure Set_name (o: in out Visual'class; new_name: String);
   -- Give a new name (no need of space-filling) to the object

   function Get_name (o: in Visual'class) return String;

   function Width  (o: in Visual'class) return Real;
   function Height (o: in Visual'class) return Real;
   function Depth  (o: in Visual'class) return Real;

   null_Visuals : constant Visual_array (1 .. 0) := (others => null);

   procedure render (the_Visuals : in Visual_array;   the_Camera : in Camera);
   --
   -- clears the color buffer and renders each of the visuals.

   -- Map_of_Visuals
   --
   -- We define here a way of finding quickly a Visual's access
   -- through its identifier.
   --
   type Map_of_Visuals is private;
   -- One can begin with empty_map, then Add Visuals one per one:
   function empty_map return Map_of_Visuals;
   procedure Add( to_map: in out Map_of_Visuals; what: p_Visual );
   Duplicate_name: exception;
   -- One can also get a map of an array of visuals in one go:
   function Map_of( va: Visual_array ) return Map_of_Visuals;

   -- original G3D Object class
   --

  type Object_3D;
  type p_Object_3D is access all Object_3D'Class;

  -------------------
  -- Define a face --
  -------------------

  type Skin_type is (
    texture_only,
    colour_only,
    coloured_texture,
    material_only,
    material_texture,
    invisible
  );

  type Set_of_Skin is array(Skin_type) of Boolean;

  is_textured: constant Set_of_Skin:=
    ( texture_only | coloured_texture | material_texture => True,
      others => False
    );

  null_colour: constant GL.Material_Float_vector:= (0.0,0.0,0.0,0.0);

  subtype Idx_3_array is Index_array(1..3);

  subtype Idx_4_array is Index_array(1..4);
  type Idx_4_array_array is array(Positive range <>) of Idx_4_array;

  type Map_idx_pair is record U,V: aliased GL.Double; end record;
  type Map_idx_pair_array is array(Natural range <>) of Map_idx_pair;
  subtype Map_idx_pair_4_array is Map_idx_pair_array(1..4);

  type Face_type is record
     P            : Idx_4_array;  -- indices of the edges (anticlockwise)
                  -- one of them can be 0 (triangle); then the
                  -- "missing" edge indicates how to put texture
     -- *** Portals :
     connecting   : p_Object_3D:= null; -- object behind - if there is one

     -- *** Surface
     skin         : Skin_type;
     mirror       : Boolean:= False;  -- mirror just behind the skin ?
     alpha        : GL.Double:= 1.0;
       -- alpha in [0;1] for blending colours and textures.
       -- NB: when this value (or all of material colours) is equal to
       --     one, the blending for transparency is switched off to gain
       --     speed; GLOBE_3D can switch on the blending again when loading
       --     a texture that has an alpha layer
     -- *** > colour part (data ignored when irrelevant):
     colour       : GL.RGB_Color;
     -- *** > material part (data ignored when irrelevant):
     material     : GL.Materials.Material_type:=
                      GL.Materials.neutral_material;
     -- *** > texture-mapping part (data ignored when irrelevant):
     texture      : Image_ID:= null_image;
     --  Alternative to setting an Image_id, if it is not known at
     --  time of building the object: use Texture_name_hint, then
     --  Rebuild_links
     --
     --    Whole texture or part of one ?
     whole_texture: Boolean:= True;
     --    - in case of a whole texture, automatic mapping, we just need
     --      to know how many times is it tiled:
     repeat_U,
     repeat_V     : Positive:= 1;
     --    - in case of a partial texture (e.g. for a texture spread
     --      across several faces), we need a deterministic mapping:
     texture_edge_map :
                    Map_idx_pair_4_array;
  end record;

  type Face_array is array(Natural range <>) of aliased Face_type;
  type p_Face_array is access Face_array;

  subtype Edge_count is Positive range 3..4;

  -- Invariants: things that don't change during the object's life

  type Face_invariant_type is private; -- GLOBE_3D-internal, nothing for users

  type Face_invariant_array is array(Natural range <>) of Face_invariant_type;

  type Object_3D_list;
  type p_Object_3D_list is access Object_3D_list;
  type Object_3D_list is record
    objc: p_Object_3D;
    next: p_Object_3D_list;
  end record;

  type Object_3D_array is array(Positive range <>) of p_Object_3D;
  type p_Object_3D_array is access Object_3D_array;

  -----------------------------------
  -- Now: the Object_3D definition --
  -----------------------------------

  type List_Cases  is (No_List, Generate_List, Is_List);
  subtype List_Ids is Positive;

   --
   -- Added "List_status" and "List_Id" to the Object_3D.
   -- by default the Display_One routine will now generate a GL command list
   -- instead of sending the command each time explicitely.
   -- To disable this feature, set Object_3D.List_Status to "No_List".
   -- If memory is not sufficient to hold a list, the Display_One routine will
   -- automatically default back to "No_List".
   --
   -- Uwe R. Zimmer, July 2011
   --
  type Object_3D (Max_points, Max_faces: Integer) is new Visual with record
    point          : Point_3D_array  (1..Max_points);  -- vertices
    edge_vector    : Vector_3D_array (1..Max_points);  -- normals for lighting
    face           : Face_array(1..Max_faces);
    sub_objects    : p_Object_3D_list:= null;
    -- List of objects to be drawn AFTER the
    -- object itself e.g., things inside a room
    pre_calculated : Boolean:= False;
    List_Status    : List_Cases := Generate_List;
    -- private:
    List_Id        : List_Ids;
    face_invariant : Face_invariant_array(1..Max_faces);
    bounds         : GL.Geometry.Bounds_record;
    transparent    : Boolean:= False;
  end record; -- Object_3D

  procedure destroy        (o : in out Object_3D);
  procedure set_Alpha      (o : in out Object_3D;   Alpha : in GL.Double);
  function  is_Transparent (o : in Object_3D) return Boolean;
  function  face_Count     (o : in Object_3D) return Natural;
  function  Bounds         (o : in Object_3D) return GL.Geometry.Bounds_record;

  procedure Check_object(o: Object_3D);
  -- Check object for invalid or duplicate vertices

  procedure Texture_name_hint(
    o   : in out Object_3D;
    face:        Positive;
    name:        String
  );
  -- Indicate a texture's name that can be resolved later by Rebuild_links

  procedure Portal_name_hint(
    o   : in out Object_3D;
    face:        Positive;
    name:        String
  );
  -- Indicate a portal's name that can be resolved later by Rebuild_links

  procedure Rebuild_links(
    o           : in out Object_3D'Class; -- object to be relinked
    neighbouring: in     Map_of_Visuals;  -- neighbourhood
    tolerant_obj: in     Boolean;         -- tolerant on missing objects
    tolerant_tex: in     Boolean          -- tolerant on missing textures
  );
  -- Does nothing when texture or object name is empty
  Portal_connection_failed: exception;

  bad_vertex_number, duplicated_vertex,
    duplicated_vertex_location: exception;
  point_unmatched, too_many_adjacences: exception;
  bad_edge_number: exception;

  procedure Pre_calculate(o: in out Object_3D);
  -- Done automatically at first display, but sometimes
  -- it's better to do it before: operation can be long!

  ------------------------------------------------------------
  -- Display of a whole scene, viewed from a certain object --
  ------------------------------------------------------------

  procedure Display(
    o          : in out Object_3D;
    clip       : in     Clipping_data
  );
  -- - "out" for o because object might be pre_calculated if not yet
  -- - clip:
  --     allows to cull rendering of neighbouring objects that are not
  --     visible from current point of view; also avoids infinite
  --     recursion in case of mutually connected objects.
  -- - neighbouring objects being drawn more than once, e.g. two parts
  --     visible through two portals, is admissible with adequate clipping.

  --------------------------------
  -- Display of a single object --
  --------------------------------

  procedure Display_one(o: in out Object_3D);
  -- Display only this object and not connected objects
  -- "out" for o because object might be pre_calculated if not yet

  -- Abstract windowing management
  --

  type Window is abstract tagged
    record
         Camera : aliased GLOBE_3D.Camera;
    end record;

   type p_Window is access all Window'Class;

  procedure enable  (Self      : in out Window) is abstract;
  procedure freshen (Self      : in out Window;
                     time_Step : in     GLOBE_3D.Real;
                     Extras    : in     GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals) is abstract;

  -- Exceptions
  --

  Missing_level_data : exception;
  Missing_global_data: exception;

  Missing_texture: exception;
  Missing_object : exception;

  zero_normal: exception;
  zero_summed_normal: exception;
  zero_averaged_normal: exception;

  --------------
  -- Lighting --
  --------------

  subtype Light_count is Natural range 0..8;
  -- GL supports up to 8 sources.
  subtype Light_ident is Light_count range 1..Light_count'Last;

  type Light_definition is record
    position, ambient, diffuse, specular: GL.Light_Float_Vector;
  end record;

  procedure Define(which: Light_ident; as: Light_definition);

  procedure Switch_lights(on: Boolean);
  procedure Switch_light(which: Light_ident; on: Boolean);

  procedure Reverse_light_switch(which: Light_ident);

  function Is_light_switched(which: Light_ident) return Boolean;

  ----------
  -- Misc --
  ----------

  function Image( r: Real ) return String;

  function Coords( p: Point_3D ) return String;

  procedure Angles_modulo_360( v: in out Vector_3D );

  --------------------------------
  -- Free heap-allocated memory --
  --------------------------------

  procedure Dispose is
    new Ada.Unchecked_Deallocation( Point_3D_array, p_Point_3D_array );

  procedure Dispose is
    new Ada.Unchecked_Deallocation( Face_array, p_Face_array );

  ---------------------------------------------------------------
  -- Trash: provisory variables for some development checkings --
  ---------------------------------------------------------------

  -- info_?_... : ?= letter; change letter to clean-up debug infos

  info_b_real1,
  info_b_real2: Real:= 123.0;
  info_b_vect : Vector_3D:= (others => 123.0);
  info_b_bool1,
  info_b_bool2: Boolean:= False;
  info_b_clip : Clipping_area:= (0,0,0,0);
  info_b_pnt  : array(0..4) of Point_3D:= (others => (others => 123.0));
  info_b_ntl1,
  info_b_ntl2,
  info_b_ntl3 : Natural:= 0;
  info_b_str1 : Ada.Strings.Unbounded.Unbounded_String:=
                  Ada.Strings.Unbounded.Null_Unbounded_String;

private

  type p_String is access String;

  type Face_invariant_type is record
     P_compact   : Idx_4_array;
                     -- indices of the edges (anticlockwise),
                     -- in compact range : 1..3 for triangle
     last_edge   : Edge_count;
     UV_extrema  : Map_idx_pair_4_array;
                     -- mapping of texture edges according to an eventual
                     -- 0 in P (triangle). Compact range : 1..3 for triangle
     normal      : Vector_3D;
     blending    : Boolean; -- is any alpha < 1 ?
     connect_name: Ident:= empty;
     -- ^ Used for loading connected objects.
     --   When the object group has been loaded, that name is set;
     --   the face(f).connecting accesses can be resolved using
     --   the face_invariant(f).connect_name .
     texture_name: Ident:= empty;
     -- ^ face(f).texture must be resolved using
     --   face_invariant(f).texture_name .
     portal_seen : Boolean:= False;
     -- ^ always False, except during Display to avoid possible infinite
     --   recursion; reset to False at the end of Display.
  end record;

  -- A few global variables - shocking! Don't look, it's private here :-)

  -- Name of Zip archives containing the Level / Global data
  -- If an item is not found in the level data, it is
  -- searched in the global one
  level_data_name  : Ada.Strings.Unbounded.Unbounded_String:=
    Ada.Strings.Unbounded.To_Unbounded_String("*undefined_level_data*");
  global_data_name : Ada.Strings.Unbounded.Unbounded_String:=
    Ada.Strings.Unbounded.To_Unbounded_String("*undefined_global_data*");

  -- Corresponding zip file infos for quick search
  zif_level, zif_global: Zip.Zip_info;

  procedure Load_if_needed( zif: in out Zip.Zip_info; name: String);

  -- General support functions available to child packages ...
  --

  -- blending support
  --
  function Is_to_blend (m: GL.Double)                  return Boolean;
  function Is_to_blend (m: GL.Float)                   return Boolean;
  function Is_to_blend (m: GL.Material_Float_vector)   return Boolean;
  function Is_to_blend (m: GL.Materials.Material_type) return Boolean;

  -- material support
  --
  procedure Set_Material (m: GL.Materials.Material_type);

  -- Maps of Visuals - quick dictionary search
  --
  package Visuals_Mapping is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => p_Visual,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

  type Map_of_Visuals is new Visuals_Mapping.Map with null record;

end GLOBE_3D;
