-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2000 Hans-Frieder Vogt                                --
--                                                                           --
--  Adabindx is free software; you can redistribute it and/or modify it      --
--  under the terms of the GNU General Public License as published by the    --
--  Free Software Foundation; either version 2 of the License, or (at your   --
--  option) any later version.                                               --
--                                                                           --
--  This program is distributed in the hope that it will be useful, but      --
--  WITHOUT ANY WARRANTY; without even the implied warranty of               --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  See the GNU General Public License for more details.                     --
--                                                                           --
--  You should have received a copy of the GNU General Public License        --
--  along with this program; if not, write to the                            --
--  Free Software Foundation, Inc.,                                          --
--  59 Temple Place - Suite 330,                                             --
--  Boston, MA 02111-1307, USA.                                              --
--                                                                           --
--  As a special exception, if other files instantiate generics from this    --
--  unit, or you link this unit with other files to produce an executable,   --
--  this unit does not by itself cause the resulting executable to be        --
--  covered by the GNU General Public License. This exception does not       --
--  however invalidate any other reasons why the executable file might be    --
--  covered by the GNU General Public License.                               --
--                                                                           --
--  X Window System is copyrighted by the X Consortium                       --
--  Motif(tm)       is copyrighted by the Open Software Foundation, Inc.     --
--                  and by The Open Group                                    --
--                                                                           --
--                                                                           --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--
-- HISTORY:
--          June 20, 1998 begin of history
--
-------------------------------------------------------------------------------

package X_Lib.Predefined_Atoms is

   XA_PRIMARY              : constant Atom := Atom (1);
   XA_SECONDARY            : constant Atom := Atom (2);
   XA_ARC                  : constant Atom := Atom (3);
   XA_Atom                 : constant Atom := Atom (4);
   XA_BITMAP               : constant Atom := Atom (5);
   XA_CARDINAL             : constant Atom := Atom (6);
   XA_COLORMAP             : constant Atom := Atom (7);
   XA_CURSOR               : constant Atom := Atom (8);
   XA_CUT_BUFFER0          : constant Atom := Atom (9);
   XA_CUT_BUFFER1          : constant Atom := Atom (10);
   XA_CUT_BUFFER2          : constant Atom := Atom (11);
   XA_CUT_BUFFER3          : constant Atom := Atom (12);
   XA_CUT_BUFFER4          : constant Atom := Atom (13);
   XA_CUT_BUFFER5          : constant Atom := Atom (14);
   XA_CUT_BUFFER6          : constant Atom := Atom (15);
   XA_CUT_BUFFER7          : constant Atom := Atom (16);
   XA_DRAWABLE             : constant Atom := Atom (17);
   XA_FONT                 : constant Atom := Atom (18);
   XA_INTEGER              : constant Atom := Atom (19);
   XA_PIXMAP               : constant Atom := Atom (20);
   XA_POINT                : constant Atom := Atom (21);
   XA_RECTANGLE            : constant Atom := Atom (22);
   XA_RESOURCE_MANAGER     : constant Atom := Atom (23);
   XA_RGB_COLOR_MAP        : constant Atom := Atom (24);
   XA_RGB_BEST_MAP         : constant Atom := Atom (25);
   XA_RGB_BLUE_MAP         : constant Atom := Atom (26);
   XA_RGB_DEFAULT_MAP      : constant Atom := Atom (27);
   XA_RGB_GRAY_MAP         : constant Atom := Atom (28);
   XA_RGB_GREEN_MAP        : constant Atom := Atom (29);
   XA_RGB_RED_MAP          : constant Atom := Atom (30);
   XA_STRING               : constant Atom := Atom (31);
   XA_VISUALID             : constant Atom := Atom (32);
   XA_WINDOW               : constant Atom := Atom (33);
   XA_WM_COMMAND           : constant Atom := Atom (34);
   XA_WM_HINTS             : constant Atom := Atom (35);
   XA_WM_CLIENT_MACHINE    : constant Atom := Atom (36);
   XA_WM_ICON_NAME         : constant Atom := Atom (37);
   XA_WM_ICON_SIZE         : constant Atom := Atom (38);
   XA_WM_NAME              : constant Atom := Atom (39);
   XA_WM_NORMAL_HINTS      : constant Atom := Atom (40);
   XA_WM_SIZE_HINTS        : constant Atom := Atom (41);
   XA_WM_ZOOM_HINTS        : constant Atom := Atom (42);
   XA_MIN_SPACE            : constant Atom := Atom (43);
   XA_NORM_SPACE           : constant Atom := Atom (44);
   XA_MAX_SPACE            : constant Atom := Atom (45);
   XA_END_SPACE            : constant Atom := Atom (46);
   XA_SUPERSCRIPT_X        : constant Atom := Atom (47);
   XA_SUPERSCRIPT_Y        : constant Atom := Atom (48);
   XA_SUBSCRIPT_X          : constant Atom := Atom (49);
   XA_SUBSCRIPT_Y          : constant Atom := Atom (50);
   XA_UNDERLINE_POSITION   : constant Atom := Atom (51);
   XA_UNDERLINE_THICKNESS  : constant Atom := Atom (52);
   XA_STRIKEOUT_ASCENT     : constant Atom := Atom (53);
   XA_STRIKEOUT_DESCENT    : constant Atom := Atom (54);
   XA_ITALIC_ANGLE         : constant Atom := Atom (55);
   XA_X_HEIGHT             : constant Atom := Atom (56);
   XA_QUAD_HEIGHT          : constant Atom := Atom (57);
   XA_WEIGHT               : constant Atom := Atom (58);
   XA_POINT_SIZE           : constant Atom := Atom (59);
   XA_RESOLUTION           : constant Atom := Atom (60);
   XA_COPYRIGHT            : constant Atom := Atom (61);
   XA_NOTICE               : constant Atom := Atom (62);
   XA_FONT_NAME            : constant Atom := Atom (63);
   XA_FAMILY_NAME          : constant Atom := Atom (64);
   XA_FULL_NAME            : constant Atom := Atom (65);
   XA_CAP_HEIGHT           : constant Atom := Atom (66);
   XA_WM_CLASS             : constant Atom := Atom (67);
   XA_WM_TRANSIENT_FOR     : constant Atom := Atom (68);

   XA_LAST_PREDEFINED      : constant Atom := XA_WM_TRANSIENT_FOR;
   

end X_Lib.Predefined_Atoms;
