-- Quote from the help (about strings): "MAX Script, is-dead-funky"  :-)

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

function indent l=(for i=0 to l do format "  ")

function matos ma n level=
( indent level
  format "Sub-%-material, %, '%' \n" level n ma.name
  if classof ma==Multimaterial then
    (
    indent level
    format "  Multimaterial: ID list %\n" ma.materialIDList 
    -- ArrayParameter
    -- default: #(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) -- int array; Index 
    for i=1 to ma.count do matos ma[i] i (level + 1)
    )
  else if (hasProperty ma "ambient") then
    (	 
      (Try(if classof ma.diffuseMap==Bitmaptexture then
      (
        -- indent level
        -- format "  bitmap %  \n" ma.diffusemap.bitmap.filename
        indent level
        format "  Bitmap '%'  \n" (filename_to_ident ma.diffusemap.bitmap.filename)
        -- See UVGenClass in MaxScript reference
        indent level
        format "  Offset (%, %) \n" ma.diffusemap.coordinates.U_Offset ma.diffusemap.coordinates.V_Offset
        indent level
        format "  Tiling (%, %) \n" ma.diffusemap.coordinates.U_Tiling ma.diffusemap.coordinates.V_Tiling
        indent level
        format "  Mirror (%, %) \n" ma.diffusemap.coordinates.U_Mirror ma.diffusemap.coordinates.V_Mirror
        indent level
        format "  Tile (%, %) \n" ma.diffusemap.coordinates.U_Tile ma.diffusemap.coordinates.V_Tile
	)) Catch())
      indent (level+5)
      format "%   : constant Material_type:= (" ma.name      
      format "-- shader type = %\n" ma.Shader_Type
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
      local sl = ma.specular_level * 0.01
      if ma.Shader_Type == 1 then -- value 1 but displayed as metal in material editor...
        sl = sl * 0.25
      format "specular  => (%, %, %, 1.0)," (sl*ma.specular.r/255.0) (sl*ma.specular.g/255.0) (sl*ma.specular.b/255.0)
      format " -- level = %\n" sl
      indent (level+6)
      format "emission  => (%, %, %, 1.0),\n" (ma.selfIllumColor.r/255.0) (ma.selfIllumColor.g/255.0) (ma.selfIllumColor.b/255.0)
      indent (level+6)
      format "shininess => %\n"               (ma.glossiness*1.28)
      indent (level+5)
      format "); \n"
  )
)

  ClearListener();
  
  -- Title, Subject, Author, Keywords, Comments 

  format "%\n" (fileProperties.getNumProperties #summary)
  for i = 1 to (fileProperties.getNumProperties #summary) do
    format "-- %: %\n" (fileProperties.getPropertyName #summary i) (fileProperties.getPropertyValue #summary i)

  format "Scene materials:\n"
  for m in sceneMaterials do ( matos m 1 0 )

  --format "medit materials:\n"
  --for m in meditMaterials do ( matos m 1 0 )

  --format "currentMaterialLibrary\n"
  --for m in currentMaterialLibrary do ( matos m 1 0 )

  (
  local n = 0
  for c in rootNode.children do
    ( format "Node % \n" c.name
      ( n = n + 1
        -- try (
        matos c.material n 0
        -- ) catch()
      )
    )
  )
