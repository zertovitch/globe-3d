-- Doom3.y generated from Domm3.pry by the gnatprep tool.
--
-- DO NOT MODIFY the Doom3.Y file, but only the Doom3.PRY one !

--------------------------------------------------------------
-- AYACC grammar file for parsing id Software's .proc files --
-- i.e. files that the level editor Radiant outputs         --
-- with the geometry of areas, the portal connections       --
-- and the binary space partition (BSP) tree. The           --
-- .proc files are loaded by games like Doom 3 or Quake 4   --
-- to display levels' scenes and locate players or monsters --
-- among the scenes' areas                                  --
--------------------------------------------------------------

-- From a FAQ (completed):
-- what are .map, .proc and .cm files?
--  .map is the editor file with entity placement;
--  .proc is the precompiled geometry, portal and bsp;
--  .cm is the collision map.

%token NUMBER
%token FLOAT_t
%token D3String

%token
 mapProcFile003_t mapProcFile_t model_t
 shadowModel_t interAreaPortals_t nodes_t

%start doom3

{

  type const_type is (
    intval,
    floatval,
    doubleval,
    stringval,
    any_type
  );

  type YYSType is record
     text    : String(1..255);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Long_Float;
  end record;

}

%%

doom3: mapProcFile {Doom3_Help.YY_ACCEPT;} -- .proc file
     | error       {Doom3_Help.YY_ABORT;}
     ;

------------------------
-- *** .proc file *** --
------------------------

  mapProcFile: mapProcFile_header mapProcFile_components ;

  mapProcFile_header :
           mapProcFile003 -- Doom 3
         | mapProcFile004 -- Quake 4
         ;

  mapProcFile003 :
         mapProcFile003_t -- "mapProcFile003"
         { Put_Line( Standard_Error, "Scanning Proc 3 file (Doom 3)" ); }
         ;


  mapProcFile004 :
         mapProcFile_t D3String -- e.g.: 'PROC "4"'
         NUMBER -- checksum ? e.g. 1194436445
         { Put_Line( Standard_Error, "Scanning Proc 4+ file (Quake 4)" ); }
         ;

  mapProcFile_components: 
        mapProcFile_component 
        mapProcFile_components 
    | 
        { Put_Line( Standard_Error, "End of Proc file. **" ); }
    ;

  mapProcFile_component:
                  Model
                | shadowModel
                | interAreaPortals_block
                | BSP_nodes_block
                ;

  Model         :
               model_t
               '{'
               D3String   -- Name of model, like "_area0"
               { Add_Model(YYText);
                 Put( Standard_Error, "- model: " & YYText );
                 if consider_current_model then

                   Ada_Build_Model_Header;



                 else
                   Put(Standard_Error, " (ignored) ");
                 end if;
               }
               NUMBER   -- # of surfaces
               { vertex_offset:= 1; -- includes numbering shift 0->1
                 Reset_surfaces;
                 total_points:= 0;
                 total_faces:= 0;
                 d3_total_points:= 0;
                 d3_total_faces:= 0;
               }
               Sky
               Surfaces -- the surfaces themselves
               { if consider_current_model then

                   Ada_Build_Model_Footer; -- generates Ada code to build model



                 end if;
                 Put_Line(
                   Standard_Error,
                   Integer'Image(total_Faces) & " triangles"
                 );
               }
               '}'
               ;

  Sky :   NUMBER --          : Quake 4
        |        -- (nothing): Doom 3
        ;

  --------------
  -- Surfaces --
  --------------

  Surfaces : Surface Surfaces | ;

  Surface       :
               '{'
               D3String
               { Set_current_texture(YYTExt);

                 if consider_current_model then
                   Ada_Comment("Texture: " &
                     Get_current_texture
                   ); -- check
                 end if;

                 -- Put_Line( Standard_Error, "  - Texture " & Get_current_texture);
               }
               -- Name of common texture, like "textures/base_wall/lfwall13f3"
               NUMBER -- numVerts
               { num_vertices:= yylval.intval;
                 total_points:= total_points + num_vertices;
                 d3_total_points:= d3_total_points + num_vertices;
                }
               NUMBER -- numIndexes
               {
                 num_indices := yylval.intval;
                 total_faces:= total_faces + num_indices / 3;
                 d3_total_faces:= d3_total_faces + num_indices / 3;
                 if consider_current_model then
                   Add_surface(
                     Get_current_texture,
                     num_vertices,
                     num_indices / 3
                   );

                   Ada_Put_Line(
                     "Surface_" &
                     Image(surface_count) &
                     "_vertices: constant " &
                     "Surface_vertex_array:= ("
                   );

                 end if;
                 -- Put_Line( Standard_Error, "  - Vertices " & Image(num_vertices));
               }
               Surface_Vertices

               {
                 if consider_current_model then
                   Ada_Put_Line(
                     "Surface_" &
                     Image(surface_count) &
                     "_triangles: constant " &
                     "Triangle_array:= ("
                   );
                   triangle_count:= 0;
                 end if;
                 -- Put_Line( Standard_Error, "  - Indices " & Image(num_vertices));
               }

               Surface_Indices_or_nothing
               '}'
               {
                 if consider_current_model then
                   vertex_offset:= vertex_offset + num_vertices;
                   surface_count:= surface_count + 1;
                 end if;
               }
               ;

  Surface_Vertices :
                 Surface_Vertex

                 {
                   if consider_current_model then Ada_Put_Line(","); end if;
                 }

                 Surface_Vertices
               |
                 Surface_Vertex

                 {
                   if consider_current_model then Ada_Put_Line(");"); end if;
                 }

                 ;

  Surface_Vertex :
                '('

                {
                  if consider_current_model then Ada_Put("("); end if;
                }

                D3Float_triple                   -- point in space
                D3Float { last_U:= Doom3_Help.Real(yylval.floatval); }       -- u
                D3Float { last_V:= Doom3_Help.Real(1.0 - yylval.floatval); } -- v
                D3Float --  nx
                D3Float --  ny
                D3Float --  nz
                {
                  if consider_current_model then

                    Ada_Put(Coords(last_pt));
                    Ada_Put(",(" & Image(last_U) & ',' & Image(last_V) & ')');
                    Ada_Put(")");




                  end if;
                }
                Surface_Vertex_extra
                ')'
                ;


  Surface_Vertex_extra :
               NUMBER NUMBER NUMBER NUMBER --          : Quake 4 - purpose ?
             |                             -- (nothing): Doom 3
             ;

  Surface_Indices_or_nothing :
              Surface_Indices
            | -- No triangle at all (eotl.proc)! 
            ;

  Surface_Indices :
                  -- List of triangles.

                  -- The indices are those of the array of Vertices above.
                  --
                  -- Numbering on triangles looks weirdo: e.g. 0 1 2 3 1 0
                  -- A bit of guess work: 2,3 appear once, must be opposite.
                  --    0-2
                  --    |\|
                  --    3-1
                  -- Then it can be 0d12 31d0 (d: dead edge); anyway textures
                  -- vertices are deterministic -> dead edges don't matter.

                  Surface_Index
                  {
                    if consider_current_model then

                      triangle_count:= triangle_count + 1;
                      Ada_Put_Triangle;
                      if triangle_count mod 4 = 0 then
                        Ada_Put_Line(",");
                      else
                        Ada_Put(",");
                      end if;



                    end if;
                  }
                  Surface_Indices
                |
                  Surface_Index -- last or only index
                  { if consider_current_model then

                      triangle_count:= triangle_count + 1;
                      if triangle_count = 1 then
                        Ada_Put("1=>");
                      end if;
                      Ada_Put_Triangle;
                      Ada_Put_Line(");");



                    end if;
                  }
                ;

    Surface_Index :
                 NUMBER
                 { v1:= yylval.intval; }
                 NUMBER
                 { v2:= yylval.intval; }
                 NUMBER
                 { v3:= yylval.intval; }

    -- NB: no direct output, we have to buffer because of cases
    -- with only 1 triangle (occurs...) needing "1=>" before
               ;

  -------------------
  -- Shadow models --
  -------------------

  shadowModel   :
                shadowModel_t
                '{'
                D3String -- Name, like "_prelight_light_2"
                { Put_Line( Standard_Error, "- shadow model (not used yet): " & YYText ); }
                NUMBER -- numVerts
                NUMBER -- noCaps
                NUMBER -- noFrontCaps
                NUMBER -- numIndexes
                NUMBER -- planeBits
                shadowVerts
                shadowIndexes
                '}'
                ;

  shadowVerts   : shadowVert shadowVerts | ;

  shadowVert          :
                '('
                D3Float_triple
                {
                  -- Ada_Comment("done with 1 shadowvertex");
                  null;
                }
                ')'
                ;

  shadowIndexes : shadowIndex shadowIndexes | ;

  shadowIndex  :  NUMBER
               ;
  ----------------------
  -- interAreaPortals --
  ----------------------

  interAreaPortals_block  :
                   interAreaPortals_t
                   { Put_Line( Standard_Error, "- Portals connecting areas (interAreaPortals)" ); }
                   '{'
                   NUMBER -- numAreas
                   NUMBER -- numIAP

                   {
                     Ada_Put_Line("procedure Define_IAPs is");
                     Ada_Put_Line("begin");
                     Ada_Put_Line("IAP:= new IAP_array'((");
                   }

                   interAreaPortals

                   {
                     Ada_Put_Line("));");
                     Ada_Put_Line("end Define_IAPs;");
                   }

                   '}'
                   ;

  interAreaPortals:
                 interAreaPortal

                 { Ada_Put_Line(","); }

                 interAreaPortals
                |
                 -- can be empty (deimos.proc)
                ;

  interAreaPortal :
                    NUMBER -- numPoints
                    {
                      iap_points:= yylval.intval;
                      if iap_points /= 4 then
                        -- ^ Supposed to be 4 (usual),
                        -- if not, some modification is needed

                        Ada_Comment("CAUTION ! Portal with /= 4 points not supported");



                      end if;



                    }
                    NUMBER -- positiveSideArea
                    {
                      iap_pos:= yylval.intval;

                      if pretty then
                        Ada_Put("( positive_side => ");
                      else
                        Ada_Put("(");
                      end if;
                      Ada_Put(iap_pos);

                    }
                    NUMBER -- negativeSideArea
                    {
                      iap_neg:= yylval.intval;

                      if pretty then
                        Ada_Put(", negative_side => ");
                      else
                        Ada_Put(",");
                      end if;
                      Ada_Put(iap_neg);
                      if pretty then
                        Ada_Put_Line(", point => (");
                      else
                        Ada_Put(",(");
                      end if;

                    }
                    IAP_Verts -- ( point ) ...
                    {
                      Add_IAP;

                      Ada_Put(")");

                    }
                    ;

  IAP_Verts    : IAP_Vert

                 { Ada_Put_Line(","); }

                 IAP_Verts
                |
                 IAP_Vert

                 { Ada_Put_Line(")"); }

                ;

  IAP_Vert      :
                  '('
                  D3Float_triple
                  {

                    Ada_Put(Coords(last_pt));



                  }
                  ')'
                ;

  ---------------
  -- BSP nodes --
  ---------------

  BSP_nodes_block  :
                   nodes_t
                   { Put( Standard_Error, "- Binary Space Partition tree" ); }
                   '{'
                   NUMBER -- numNodes
                   { Put_Line( Standard_Error, ", " & yytext & " nodes." );

                     Ada_Put_Line("D3_BSP_node: D3_BSP_node_array:= (");










                   }
                   bsp_nodes

                   { Ada_Put_Line(");"); }

                   { Put_Line( Standard_Error, "  BSP reading done." ); }
                   '}'
                   ;

  bsp_nodes         :
                  bsp_node

                  { Ada_Put_Line(","); }

                  bsp_nodes
                |
                  bsp_node
                ;

-- In .proc files:
--
--  /* node format is: ( planeVector ) positiveChild negativeChild */
--  /* a child number of 0 is an opaque, solid area */
--  /* negative child numbers are areas: (-1-child) */
--  /* node 0 */ ( 1 0 0 -1024 ) 1 1684
--  /* node 1 */ ( 1 0 0 -2048 ) 2 576
--  /* node 2 */ ( 0 1 0 -2048 ) 3 16
--
-- NB: node 0 cannot be a child, then...

  bsp_node :
                    {

                      Ada_Put("(");




                    }
                    bsp_plane_vector
                    NUMBER -- positiveChild
                    {

                      if pretty then Ada_Put(" positive_child => "); end if;
                      Ada_Put(yylval.intval);



                    }
                    NUMBER -- negativeChild
                    {

                      Ada_Put(",");
                      if pretty then Ada_Put(" negative_child => "); end if;
                      Ada_Put(yylval.intval);
                      Ada_Put(")");




                    }
                    ;

  bsp_plane_vector :
                '('
                D3Float_triple -- outer normal n
                D3Float -- d: distance to origin O, >0 if O on the n side
                {
                  last_d:= Doom3_Help.Real(yylval.floatval);

                  if pretty then Ada_Put(" normal => "); end if;
                  Ada_Put(Coords(last_pt));
                  Ada_Put(",");
                  if pretty then Ada_Put(" distance => "); end if;
                  Ada_Put(Image(last_d));
                  Ada_Put(",");

                }
                ')'
                ;

  ------------------------
  -- Common definitions --
  ------------------------

  D3Float_triple :
                -- We make some orthogonal transformation towards
                -- the GLOBE_3D system
                D3Float { last_pt(2):= Doom3_Help.Real(yylval.floatval); }
                D3Float { last_pt(0):= Doom3_Help.Real(yylval.floatval); }
                D3Float { last_pt(1):= Doom3_Help.Real(yylval.floatval); }
              ;

  D3Float     : FLOAT_t
              | NUMBER
                {$$ := ($1); -- Float
                }
		;

%%

-- D3a

with Doom3_Tokens, Doom3_Shift_Reduce, Doom3_Goto, Doom3_Help, Doom3_IO;
use  Doom3_Tokens, Doom3_Shift_Reduce, Doom3_Goto, Doom3_Help, Doom3_IO;

with Doom3_DFA, YYroutines, YYerror;
use  Doom3_DFA, YYroutines;

with Ada.Text_IO;                       use Ada.Text_IO;
with Text_IO; -- yyparse has this Ada 83 syntax

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Interfaces;                        use Interfaces;

##
