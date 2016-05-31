-- Max (GMAX or 3D Studio Max) to Ada (GLOBE_3D) exporter.
-- This script writes an Ada package with a procedure Create
-- which creates a 3D object with the geometry and materials
-- of the Max scene. All geometric data is contained in the
-- Ada source produced by this script.
-- Once the script is run, start GMaxSLGRAB.exe, save the
-- text somewhere and the split it into Ada sources (gnatchop x.txt)
--
-- Copyright (c) Gautier de Montmollin 2008 .. 2016
-- Same license as GLOBE_3D
--
-- Quote from the help (about strings): "MAX Script, is-dead-funky"  :-)

  ---------------------
  global pretty = false
  ---------------------

  function filename_to_ident s = (
    local i1, i2, s1
    i1 = 0
    i2 = s.count+1
    for i=1 to s.count do
    ( if s[i]=="\\" then i1=i
      if s[i]=="."  then i2=i
      if s[i]==" "  then s[i]="_"
      if s[i]=="-"  then s[i]="_"
    )
    substring s (i1+1) (i2-i1-1)
  )

  global bitmap_names = ""
  global first_bitmap = true

  function lb_matos ma n level assoc decl =
  (
  if classof ma==Multimaterial then
    (
    for i=1 to ma.count do lb_matos ma[i] i (level + 1) assoc decl
    )
  else
    if classof ma.diffuseMap==Bitmaptexture then
      ( local bitmap_name = filename_to_ident ma.diffusemap.bitmap.filename
        local j = findString bitmap_names bitmap_name
        if (j == undefined) then
        ( if first_bitmap then (if decl then format ";\n") else format ",\n"
          format "    %" bitmap_name
          if assoc then format " => Texture_enum'Pos(%)" bitmap_name
          bitmap_names = bitmap_names + ";" + bitmap_name
          first_bitmap = false
        )
      )
  )
    
  function list_bitmaps assoc decl =(
    for c in rootNode.children do
      ( -- format "  node % \n" c.name
        try (
        lb_matos c.material 1 0 assoc decl
        ) catch()
      )
    if decl and not first_bitmap then format ": GLOBE_3D.Image_ID"
    format "\n"
  )

  function proc_decl =(
    format "  procedure Create(\n" 
    format "    object: in out GLOBE_3D.p_Object_3D;\n"
    format "    scale :        GLOBE_3D.Real;\n" 
    format "    centre:        GLOBE_3D.Point_3D" 
    bitmap_names = ""
    first_bitmap = true
    list_bitmaps false true
    format "  )" 
  )

  global gmax_scene = ""

  function outputHeader =( 
    format "-- * Output of max2ada.ms, a GMax / 3D Studio Max script for exporting to GLOBE_3D\n" 
    format "--\n" 
    format "-- * Copy and paste these lines from the Listener into a\n" 
    format "--   text editor, and save the package as an .ada file.\n" 
    format "-- * Alternatively, use the GMaxSLGRAB.exe tool.\n" 
    format "-- * For GNAT, you must save the specification as an .ads file\n" 
    format "--   and the body as an .adb file, or run gnatchop on the whole .ada file.\n" 
    format "\n"
    -- Get Title, subject and author: 
    for i = 1 to (fileProperties.getNumProperties #summary) do
      format "-- %: %\n" (fileProperties.getPropertyName #summary i) (fileProperties.getPropertyValue #summary i)
    format "\n" 
	format "-- File name: %\n" (maxFileName)
	format "-- File path: %\n" (maxFilePath)
    format "\n" 
    format "with GLOBE_3D;\n" 
    format "\n"
    try
      gmax_scene = (fileProperties.getPropertyValue #summary 1)
    catch
      (  messageBox "You need to give at least the scene title in the file properties\nUsing default name: GMAX_scene"
         gmax_scene = "GMAX_scene"
      )
    format "package % is\n" gmax_scene
    format "\n"
    proc_decl()
    format ";\n"
    format "end %;\n" gmax_scene
    format "with GL, GL.Materials, GLOBE_3D.Math;\n"
    format "\n"
    format "package body % is\n" gmax_scene
    format "  -- Pretty output: %\n" pretty
    format "  use GL, GLOBE_3D, GLOBE_3D.Math;\n"
    format "\n" 
  ) 

  global global_vertex_count = 0 
  global global_face_count = 0 
  global global_mat_count = 0 
  global node_mat_count = 0 
  global some_without_texture = 0

  function integer_from_float f = ( 
    local i = f as integer 
    return i 
  ) 

  function output_vertices node number total_nodes = ( 
    try( -- some nodes have no mesh
    local m = snapshotAsMesh node 
    format "  -- Vertices for node #%/%: % (% points, offset %)\n" number total_nodes node.name m.numVerts global_vertex_count
    local grouping = 1
    if not pretty then grouping = 5
    for v = 1 to m.numVerts do ( 
      local vert = m.verts[v] 
      global_vertex_count  = global_vertex_count + 1 
      if (grouping==1) or (1 == (mod v grouping)) then format "        "
      if pretty then      
        (format "(%, %, %)" vert.pos.x vert.pos.y vert.pos.z)
      else
        (format "(%,%,%)" vert.pos.x vert.pos.y vert.pos.z)
      if (grouping==1) or (0 == (mod v grouping)) then
        ( if (v == m.numVerts) and (number == total_nodes) then
            format "\n"
          else
            format ","
          if pretty then format " -- % (global), % (node)" global_vertex_count v
          format "\n"
        )
      else
        if (v == m.numVerts) and (number == total_nodes) then
         (if pretty then format " -- % (global), % (node)" global_vertex_count v
          format "\n"
         )
        else
          format ","
    ) 
    format "  -- End of vertices for node #%/%: %\n" number total_nodes node.name 
    ) catch ()
  ) 
  -- end output_vertices

  function output_faces node number total_nodes = ( 
    try( -- some nodes have no mesh
    local m = snapshotAsMesh node 
    format "  -- Faces for node #%/%: % (% faces, offset %)\n" number total_nodes node.name m.numFaces global_face_count
    local grouping = 3
    if not pretty then grouping = 8
    for f =1 to m.numFaces do ( 
      local vface =getface m f 
      local p1 = global_vertex_count + integer_from_float(vface.x) 
      local p2 = global_vertex_count + integer_from_float(vface.y) 
      local p3 = global_vertex_count + integer_from_float(vface.z) 

      local n = global_face_count + f 

      if 1 == (mod f grouping) then format "     "
      if pretty then
        (format "(%, %, %, 0)" p1 p2 p3)
      else
        (format "(%,%,%,0)" p1 p2 p3)
      if 0 == (mod f grouping) then
        ( if (f == m.numFaces) and (number == total_nodes) then
            format "\n"
          else
            format ","
          if pretty then format " -- % (global), % (node)" n f
          format "\n"
        )
      else
        if (f == m.numFaces) and (number == total_nodes) then
         (if pretty then format " -- % (global), % (node)" n f
          format "\n"
         )
        else
          format ","
    ) -- end loop f
    global_vertex_count  = global_vertex_count + m.numVerts 
    global_face_count    = global_face_count   + m.numFaces 
    format "  -- End of faces for node #%/%: %\n" number total_nodes node.name 
    ) catch ()
  ) 
  -- end output_faces

  function output_material_ids node number total_nodes = ( 
    try( -- some nodes have no mesh
    local m = snapshotAsMesh node 
    format "  -- Material IDs of node #%/%: % (% faces, offset %, mat offset %)\n" number total_nodes node.name m.numFaces global_face_count global_mat_count
    local max_face_id = 1
    local grouping = 20
    if not pretty then grouping = 50
    for f =1 to m.numFaces do ( 
      local facemat = getFaceMatID m f
      if facemat > max_face_id then max_face_id = facemat 
      if 1 == (mod f grouping) then format "        "
      format "%" (facemat + global_mat_count)
      if 0 == (mod f grouping) then
        ( local n = global_face_count + f 
          if not((f == m.numFaces) and (number == total_nodes)) then format ","
          if pretty then format " -- % (global), % (node)" n f
          format "\n"
        )
      else
        ( if (f == m.numFaces) and (number == total_nodes) then
           (
            if pretty then format " -- % (global), % (node)" n f
            format "\n"
           )
          else
            format ","
        )
    )
    global_face_count    = global_face_count    + m.numFaces 
    global_mat_count     = global_mat_count     + max_face_id -- can be node.material.count

    format "  -- End of material IDs for node #%/%: %\n" number total_nodes node.name 
    ) catch ()
  ) 
  -- end output_material_ids

  function output_texture_maps node number total_nodes = ( 
    try( -- some nodes have no mesh
    local m = snapshotAsMesh node 
    format "  -- Texture maps of node #%/%: % (% faces, offset %)\n" number total_nodes node.name m.numFaces global_face_count
    local grouping = 1
    if not pretty then grouping = 2
    if m.numtverts == 0 then
      some_without_texture = 1
    else
      for f = 1 to m.numFaces do
      ( 
        local tvface = getTVface m f 
        local p1 = integer_from_float(tvface.x)
        local p2 = integer_from_float(tvface.y)
        local p3 = integer_from_float(tvface.z)
        local tvert_1 = getTVert m p1
        local tvert_2 = getTVert m p2
        local tvert_3 = getTVert m p3
        local n = global_face_count + f 
        if (grouping==1) or (1 == (mod f grouping)) then format "   "
        if pretty then
          (format "% =>    ((%, %), (%, %), (%, %), (0.0,0.0))" n tvert_1.x tvert_1.y tvert_2.x tvert_2.y tvert_3.x tvert_3.y)
        else
          (format "%=>((%,%),(%,%),(%,%),(0.0,0.0))" n tvert_1.x tvert_1.y tvert_2.x tvert_2.y tvert_3.x tvert_3.y)
        if (grouping==1) or (0 == (mod f grouping)) then
          ( if (f == m.numFaces) and (number == total_nodes) and not (some_without_texture == 1) then
              format "\n"
            else
              format ","
            if pretty then format " -- % (global), % (node)" n f
            format "\n"
          )
        else
          if (f == m.numFaces) and (number == total_nodes) and not (some_without_texture == 1) then
           (if pretty then format " -- % (global), % (node)" n f
            format "\n"
           )
          else
            format ","
      ) -- end loop f
    global_vertex_count  = global_vertex_count  + m.numVerts 
    global_face_count    = global_face_count    + m.numFaces 
    format "  -- End of texture maps for node #%/%: %\n" number total_nodes node.name 
    ) catch ()
  ) 
  -- end output_texture_maps

  function indent l=(for i=0 to l do format "  ")

  function show_mat ma level = 
    (  local has_texture = False
       (Try(if classof ma.diffuseMap==Bitmaptexture then
       (
        has_texture = True
        format "(material_texture, %," (filename_to_ident ma.diffusemap.bitmap.filename)
        format "  (%, %),"   ma.diffusemap.coordinates.U_Offset ma.diffusemap.coordinates.V_Offset
        -- not exactly mirroring, but +/-...
        if ma.diffusemap.coordinates.U_Mirror then
          format "  (%,"  (ma.diffusemap.coordinates.U_Tiling * 2.0)
        else
          format "  (%,"  (ma.diffusemap.coordinates.U_Tiling)
        if ma.diffusemap.coordinates.V_Mirror then
          format " %),\n" (ma.diffusemap.coordinates.V_Tiling * 2.0)
        else
          format " %),\n" (ma.diffusemap.coordinates.V_Tiling)
	)) Catch())
      if not has_texture then
        (format "(material_only, null_image, (0.0,0.0), (1.0,1.0), -- no texture\n")
      indent (level+5)
      format "("
      format " -- name = %, shader_type = %\n" ma.name ma.Shader_Type
      --  Shader_Type 
      --  0- Anisotropic 
      --  1- Blinn 
      --  2- Metal 
      --  3- Multi-Layer 
      --  4- Oren-Nayar-Blinn 
      --  5- Phong 
      --  6- Strauss 
      indent (level+6)
      format "ambient   => (%, %, %, 1.0),\n" (ma.ambient.r/255.0) (ma.ambient.g/255.0) (ma.ambient.b/255.0)
      indent (level+6)
      format "diffuse   => (%, %, %, 1.0),\n" (ma.diffuse.r/255.0) (ma.diffuse.g/255.0) (ma.diffuse.b/255.0) 
      indent (level+6)
      local sl = ma.specular_level * 0.01 -- the curve indicates: 100 = touches the max.
      if ma.Shader_Type == 1 then -- value 1 but displayed as metal in material editor...
        sl = sl * 0.25 -- the curve indicates: 400 = touches the max.
      format "specular  => (%, %, %, 1.0)," (sl*ma.specular.r/255.0) (sl*ma.specular.g/255.0) (sl*ma.specular.b/255.0)
      if pretty then (format " -- level = %" sl)
      format "\n"
      indent (level+6)
      sl = ma.selfIllumAmount * 0.01;
      if ma.useSelfIllumColor then
        (format "emission  => (%, %, %, 1.0),\n" (sl*ma.selfIllumColor.r/255.0) (sl*ma.selfIllumColor.g/255.0) (sl*ma.selfIllumColor.b/255.0))
      else
        (sl = 0.0 -- the value given is wrong (always 100 !)
         format "emission  => (%, %, %, 1.0),\n" (sl*ma.diffuse.r/255.0) (sl*ma.diffuse.g/255.0) (sl*ma.diffuse.b/255.0))
      indent (level+6)
      format "shininess => %\n"               (ma.glossiness*1.28)
      indent (level+5)
      format ") \n"
      indent (level+4)
      format ")"
    )

  global last_mat

  function matos ma n level =
  ( indent level
    format "    -- sub-%-material, %, % \n" level n ma.name
    if classof ma==Multimaterial then
      (for i=1 to ma.count do
         ( matos ma[i] i (level + 1)
           if i < ma.count then
           format ",\n"
         )
      )
    else if (hasProperty ma "ambient") then 
      ( last_mat = ma
        indent (level+4)
        node_mat_count = node_mat_count + 1
        format "% => " (node_mat_count + global_mat_count)
        show_mat ma level
      )
  )

  function output_materials node number total_nodes = ( 
    try( -- some nodes have no mesh
    local m = snapshotAsMesh node 
    format "  -- Materials of node #%/%: % (% faces, offset %, mat offset %)\n" number total_nodes node.name m.numFaces global_face_count global_mat_count
    local max_face_id = 1
    for f =1 to m.numFaces do ( 
      local facemat = getFaceMatID m f
      if facemat > max_face_id then max_face_id = facemat 
    )
    node_mat_count = 0
    matos node.material 1 0

    if node_mat_count > max_face_id then
      messageBox "node_mat_count > max_face_id"
    else if node_mat_count < max_face_id then
     -- All lost IDs fall back to the last (and only for that node) material
     -- - seem the correct way...
     (
      format ",\n"
      format "  -- Fall-back material\n"
      format "     %..% => " (node_mat_count + global_mat_count + 1) (max_face_id + global_mat_count)
      show_mat last_mat 0
     )
    if number == total_nodes then
      format "\n"
    else
      format ",\n"

    global_mat_count     = global_mat_count     + max_face_id -- can be node.material.count

    format "  -- End of materials for node #%/%: %\n" number total_nodes node.name 
    ) catch ()
  ) 
  -- end output_materials

  function PrintObj = ( 
    outputHeader()
    local total_nodes = 0
    -- * Summary with the nodes:
    format "  -- List of nodes for % :\n" rootNode.name
    (
      for c in rootNode.children do
      ( total_nodes = total_nodes + 1
        format "  -- node % : % \n" total_nodes c.name
      )
    )
    format "\n" 
    -- * Array with all vertices:
    format "  vertices: constant Point_3D_array:= (\n"
    (
      local n = 0
      for c in rootNode.children do
      ( n = n + 1
        output_vertices c n total_nodes
      )
    )
    format "  ); -- End of all vertices\n"
    -- * Array with all faces:
    format "  face_indices: constant Idx_4_array_array:= (\n"
    global_vertex_count = 0
    (
      local n = 0
      for c in rootNode.children do
      ( n = n + 1
        output_faces c n total_nodes
      )
    )
    format "  ); -- End of all face indices\n"
    -- * Array with all material IDs:
    format "  type Positive_Index_array is array(Positive range <>) of Natural;\n"
    format "  face_material_id: constant Positive_Index_array:= (\n"
    global_face_count = 0
    (
      local n = 0
      for c in rootNode.children do
      ( n = n + 1
        output_material_ids c n total_nodes
      )
    )
    format "  ); -- End of all material IDs\n"
    
    -- * Array with all textures coordinates:
    format "  type Texture_info_array is array(Positive range <>) of Map_idx_pair_4_array;\n"
    format "  face_texture_info: constant Texture_info_array(face_indices'Range):= (\n"

    global_vertex_count = 0
    global_face_count = 0
    (
      local n = 0
      for c in rootNode.children do
      ( n = n + 1
        output_texture_maps c n total_nodes
      )
    )
    if some_without_texture == 1 then
      format "   others =>    ((0.0,0.0), (0.0,0.0), (0.0,0.0), (0.0,0.0))"
    format "  ); -- End of all texture maps\n"

    -- * Array with all materials:
    format "  type Material_info is record\n"
    format "    skin    : Skin_type;    -- here: material_only or material_texture\n"
    format "    image   : Image_ID;\n"
    format "    offset  : Map_idx_pair;\n"
    format "    tiling  : Map_idx_pair;\n"
    format "    material: GL.Materials.Material_type;\n"
    format "  end record;\n"
    format "  type Material_info_array is array(Positive range <>) of Material_info;\n"
    proc_decl()
    format "\n  is\n"
    format "    face_material_info: constant Material_info_array(1..%):= (\n" global_mat_count
    global_mat_count = 0
    (
      local n = 0
      for c in rootNode.children do
      ( n = n + 1
        try(
        output_materials c n total_nodes
        ) catch()
      )
    )
    -- format "      others=> (material_only, null_image, (0.0,0.0), (0.0,0.0), neutral_material) -- some materials have IDs but seem lost...\n"
    format "    ); -- End of all materials\n"
    
    format "\n"
    format "    face_0 : Face_type; -- takes defaults values\n"
    format "    mat_id : Positive; \n"
    format "  begin\n"
    format "    object:=\n"
    format "      new Object_3D( Max_points => %, Max_faces => % );\n" global_vertex_count global_face_count
    format "    Set_name(object.all, \"Scene exported from GMax\");\n"
    format "    object.centre:= centre;\n"
    format "    if Almost_zero(scale - 1.0) then\n"
    format "      object.point:= vertices;\n"
    format "    else\n"
    format "      for p in vertices'Range loop\n"
    format "        object.point( p ):= scale * vertices(p);\n"
    format "      end loop;\n"
    format "    end if;\n"
    format "    face_0.whole_texture:= False; -- always mapped from model, or no texture\n"
    format "    for f in face_indices'Range loop\n"
    format "      face_0.P:= face_indices(f);\n"
    format "      mat_id:= face_material_id(f);\n"
    format "      face_0.skin:=     face_material_info(mat_id).skin;\n"
    format "      face_0.texture:=  face_material_info(mat_id).image;\n"
    format "      face_0.material:= face_material_info(mat_id).material;\n"
    format "      if is_textured(face_0.skin) then\n"
    format "        for i in face_0.texture_edge_map'Range loop\n"
    format "          face_0.texture_edge_map(i):=\n"
    format "            (0.5+(face_texture_info(f)(i).u-0.5+face_material_info(mat_id).offset.U)*\n"
    format "                 face_material_info(mat_id).tiling.U,\n"
    format "             0.5+(face_texture_info(f)(i).v-0.5+face_material_info(mat_id).offset.V)*\n"
    format "                 face_material_info(mat_id).tiling.V);\n"
    format "        end loop;\n"
    format "      end if;\n"
    format "      object.face( f ):= face_0;\n"
    format "    end loop;\n"
    format "  end Create;\n" 
    format "\n"
    format "end %;\n" gmax_scene
    format "-- Test procedure, which creates the model from the package's Ada code\n"
    format "-- and saves it as a .g3d object file (more practical).\n"
    format "--\n"
    format "-- Automatically written by max2ada.ms .\n"
    format "--\n"
    format "with GLOBE_3D.IO, GLOBE_3D.Textures; use GLOBE_3D, GLOBE_3D.IO, GLOBE_3D.Textures;\n"
    format "with %; -- The package containing the GMAX model's data\n" gmax_scene
    format "procedure example is\n" 
    format "  type Texture_enum is (\n" 
    bitmap_names = ""
    first_bitmap = true
    list_bitmaps false false
    format "  );\n  gmax: GLOBE_3D.p_Object_3D;\nbegin\n  -- code chunk:\n" 
    format "  Set_global_data_name(\"%.zip\"); -- You put in that .zip file all texture bitmaps, in .bmp or .tga format\n" gmax_scene
    format "  Textures.Register_textures_from_resources;\n"
    format "  --\n"
    format "  %.Create(\n" gmax_scene
    format "    object => gmax,\n"
    format "    scale  => 1.0,\n"
    format "    centre => (0.0,0.0,0.0),\n"
    bitmap_names = ""
    first_bitmap = true
    list_bitmaps true false
    format "  );\n" 
    format "  --\n" 
    format "  Set_name(gmax.all, \"%\");\n" gmax_scene
    format "  Save_file(gmax.all);\n"
    format "  -- ^ Save the \"%.g3d\" object, might be more practical than the Ada source.\n" gmax_scene
    format "  --   For instance (to give an idea), if you include it into the \"%.zip\" file,\n" gmax_scene
    format "  --   you can also view the textured object with \"GLOBE_3D_Demo.exe -load=%\".\n" gmax_scene
    format "end example;\n" 
    format "\n" 
    flushStringStream 
  ) 

ClearListener();
-- pretty = queryBox "Pretty source mode ?" 
PrintObj()
