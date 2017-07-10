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
%token numJoints_t, numMeshes_t, joints_t
--  Anim file
%token numFrames_t, frameRate_t, numAnimatedComponents_t

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

MD5_items : MD5_mesh_items | MD5_anim_items;

MD5_mesh_items : 
            MD5Version_t              NUMBER 
            commandline_t             RCString
            numJoints_t               NUMBER
            { MD5_Help.num_joints:= Integer(yylval.intval); }
            numMeshes_t               NUMBER
            { MD5_Help.num_meshes:= Integer(yylval.intval); }
            { Ada.Text_IO.Put_Line(
                Current_Error, 
                "Mesh file. Skeleton has" & 
                Integer'Image(MD5_Help.num_joints) & 
                " joints and" & 
                Integer'Image(MD5_Help.num_meshes) & 
                " meshes."
              ); 
            }
            joints_t
            ;

MD5_anim_items : 
            MD5Version_t              NUMBER 
            commandline_t             RCString
            numFrames_t               NUMBER
            { MD5_Help.num_frames:= Integer(yylval.intval); }
            frameRate_t               NUMBER
            { MD5_Help.frame_rate:= Integer(yylval.intval); }
            numAnimatedComponents_t   NUMBER
            { Ada.Text_IO.Put_Line(Current_Error, "Animation file."); }
            ;

joints :    joints_t
            LBRACE_t
            joint_list
            RBRACE_t
         ;

joint_list : 
            joint_item
          | joint_item joint_list
          ;

--  Joint_item example: "head"	6 ( 0.002305 -0.182803 5.534186 ) ( -0.707074 -0.007565 -0.707059 )		// neck
          
joint_item : RCString 
            { Ada.Text_IO.Put_Line(Current_Error, "  Joint name: " & yytext); }
             NUMBER vector vector ;

vector : '(' NUMBER NUMBER NUMBER ')';

--------------------
-- Terminal items --
--------------------

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