--------------------------------------------------------------------------
--  MD5.y
--
--  MD5 mesh / anim grammar file (AYACC)
--
--  Copyright (c) Gautier de Montmollin 2017
--  SWITZERLAND
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

-- NB: this is the MIT License, as found 28-Jul-2008 on the site
-- http://www.opensource.org/licenses/mit-license.php
----------------------------------------------------------------------------
--
-- xx-yyy-2017 GdM: Created
--

%token NUMBER
%token FLOAT_t, COMMA_t, BAR_t, LBRACE_t, RBRACE_t, NOT_t
       C_INCLUDE_t

-----------
-- Items --
-----------

%token MD5Version_t, commandline_t
--  Mesh file
%token numJoints_t, numMeshes_t, joints_t, mesh_t,
       shader_t, numverts_t, vert_t,
       numtris_t, tri_t,
       numweights_t, weight_t
--  Anim file
%token numFrames_t, frameRate_t, numAnimatedComponents_t,
       hierarchy_t, bounds_t, baseframe_t, frame_t

-- Misc --

%token IDENT_t, STRINGTABLE_t
%token RCString, INCString, CONSUME_EOL_t

%start MD5

{

  type const_type is (
    intval,
    floatval,
    doubleval,
    stringval,
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Long_Long_Integer;
     floatval: Long_Float;
  end record;

}

%%

MD5     : MD5_items {MD5_Help.YY_ACCEPT;}
        | error     {MD5_Help.YY_ABORT;}
        ;

MD5_items : MD5_mesh_items | 
            MD5_anim_items;

--------------
--  Meshes  -- 
--------------
            
MD5_mesh_items : 
            MD5Version_t              NUMBER 
            commandline_t             RCString
            numJoints_t               NUMBER
            { MD5_Help.num_joints:= Integer(yylval.intval); }
            numMeshes_t               NUMBER
            { MD5_Help.num_meshes:= Integer(yylval.intval); }
            { Ada.Text_IO.Put_Line(Current_Error, "Mesh or anim ?"); 
              Ada.Text_IO.Put_Line(Current_Error, "Mesh file."); 
              Ada.Text_IO.Put_Line(Current_Error, "  Skeleton has" & Integer'Image(MD5_Help.num_joints) & " joints.");
              Ada.Text_IO.Put_Line(Current_Error, "  There is/are" & Integer'Image(MD5_Help.num_meshes) & " mesh(es).");
            }
            joints
            { Ada.Text_IO.Put_Line(Current_Error, "  Mesh list..."); }
            mesh_list
            ;

joints :    joints_t
            LBRACE_t
            { Ada.Text_IO.Put_Line(Current_Error, "  Joint list..."); }
            joint_list
            RBRACE_t
         ;

joint_list : 
            joint_item
          | joint_item joint_list
          ;
       
--  Joint_item example: "head"	6 ( 0.002305 -0.182803 5.534186 ) ( -0.707074 -0.007565 -0.707059 )		// neck
          
joint_item : RCString 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Joint name: " & yytext); }
             NUMBER 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Parent:" & Integer'Image(Integer(yylval.intval))); }
             vector 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    v1"); }
             vector
            --  { Ada.Text_IO.Put_Line(Current_Error, "    v2"); }
             ;

mesh_list : 
            mesh
          | mesh mesh_list
          ;

mesh :       mesh_t 
            { Ada.Text_IO.Put_Line(Current_Error, "    Mesh..."); }
            LBRACE_t
            shader
            numverts_t NUMBER
            { Ada.Text_IO.Put_Line(Current_Error, "      Vertices  :" & Integer'Image(Integer(yylval.intval))); }
            vert_list
            numtris_t NUMBER
            { Ada.Text_IO.Put_Line(Current_Error, "      Triangles :" & Integer'Image(Integer(yylval.intval))); }
            tri_list
            numweights_t NUMBER
            { Ada.Text_IO.Put_Line(Current_Error, "      Weights   :" & Integer'Image(Integer(yylval.intval))); }
            weight_list
            RBRACE_t
            ;

shader :     shader_t RCString
           |
             --  nothing: shader is omitted by the blender exporter
           ;
            
vert_list : 
            vert
          | vert vert_list
          ;
          
vert : vert_t 
       NUMBER 
       --  { Ada.Text_IO.Put_Line(Current_Error, "      Vertex:" & Integer'Image(Integer(yylval.intval))); }
       '(' float_or_int float_or_int ')' 
       NUMBER 
       NUMBER
       ;

tri_list : 
            tri
          | tri tri_list
          ;
          
tri : tri_t 
       NUMBER 
       --  { Ada.Text_IO.Put_Line(Current_Error, "      triangle:" & Integer'Image(Integer(yylval.intval))); }
       NUMBER 
       NUMBER 
       NUMBER
       ;
       
weight_list : 
            weight
          | weight weight_list
          ;
          
weight : weight_t 
       NUMBER 
       --  { Ada.Text_IO.Put_Line(Current_Error, "      weight:" & Integer'Image(Integer(yylval.intval))); }
       NUMBER  
       float_or_int 
       vector
       ;

------------------
--  Animations  -- 
------------------

MD5_anim_items : 
            MD5Version_t              NUMBER 
            commandline_t             RCString
            numFrames_t               NUMBER
            { MD5_Help.num_frames:= Integer(yylval.intval); }
            numJoints_t               NUMBER
            { MD5_Help.num_joints:= Integer(yylval.intval); }
            frameRate_t               NUMBER
            { MD5_Help.frame_rate:= Integer(yylval.intval); }
            numAnimatedComponents_t   NUMBER
            { MD5_Help.num_animated_components:= Integer(yylval.intval); }
            { Ada.Text_IO.Put_Line(Current_Error, "Mesh or anim ?"); 
              Ada.Text_IO.Put_Line(Current_Error, "Animation file.");
              Ada.Text_IO.Put_Line(Current_Error, "  Animation has" & Integer'Image(MD5_Help.num_frames) & " frame(s).");
              Ada.Text_IO.Put_Line(Current_Error, "  Frame rate is" & Integer'Image(MD5_Help.frame_rate) & " frames per second."); 
              Ada.Text_IO.Put_Line(Current_Error, "  Animated components per frame:" & Integer'Image(MD5_Help.num_animated_components)); 
              Ada.Text_IO.Put_Line(Current_Error, "  Skeleton has" & Integer'Image(MD5_Help.num_joints) & " joints.");
            }
            hierarchy
            bounds
            baseframe
            frame_list
            ;
       
hierarchy : hierarchy_t
            LBRACE_t
            { Ada.Text_IO.Put_Line(Current_Error, "  Joint list..."); }
            joint_anim_list
            RBRACE_t
         ;
     
joint_anim_list : 
            joint_anim_item
          | joint_anim_item joint_anim_list
          ;
       
joint_anim_item :
            RCString 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Joint name  : " & yytext); }
            NUMBER 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Parent      :" & Integer'Image(Integer(yylval.intval))); }
            NUMBER 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Flags       :" & Integer'Image(Integer(yylval.intval))); }
            NUMBER 
            --  { Ada.Text_IO.Put_Line(Current_Error, "    Start index :" & Integer'Image(Integer(yylval.intval))); }
            ;

--  Bounding boxes
            
bounds : bounds_t
            LBRACE_t
            { Ada.Text_IO.Put_Line(Current_Error, "    Bounding box list..."); }
            bound_list
            RBRACE_t
         ;
     
bound_list : 
            bounding_box
          | bounding_box bound_list
          ;
       
bounding_box :
            vector
            vector
            ;

--  Base frame data
            
baseframe : baseframe_t
            LBRACE_t
            { Ada.Text_IO.Put_Line(Current_Error, "    Base frame data..."); }
            baseframe_list
            RBRACE_t
         ;
     
baseframe_list : 
            baseframe_item
          | baseframe_item baseframe_list
          ;
       
baseframe_item :
            vector
            vector
            ;

-- Frames

frame_list : 
            frame
          | frame frame_list
          ;

frame :     frame_t 
            NUMBER
            { Ada.Text_IO.Put_Line(Current_Error, "    Frame #" & Integer'Image(Integer(yylval.intval))); }
            LBRACE_t
            frame_component_list
            RBRACE_t
            ;

frame_component_list :
             frame_component
           | frame_component frame_component_list
           ;

frame_component : 
           float_or_int
           ;

------------------------
-- +/- Terminal items --
------------------------

vector : '(' float_or_int float_or_int float_or_int ')';

--  Float with or without decimal. In any case yylval.floatval is fed.
float_or_int : 
                FLOAT_t
              |
                NUMBER ;

Style_Ident : IDENT_t | NUMBER ;

%%

-- This header comes from RC.y (bottom)

with MD5_Tokens, MD5_Shift_Reduce, MD5_Goto, MD5_Help, MD5_IO;
use  MD5_Tokens, MD5_Shift_Reduce, MD5_Goto, MD5_Help, MD5_IO;

with MD5_DFA, YYroutines, YYerror;
use  MD5_DFA, YYroutines;

with Ada.Text_IO;                       use Ada.Text_IO;
with Text_IO; -- for compat.

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Interfaces;                        use Interfaces;

-- Header end.

##