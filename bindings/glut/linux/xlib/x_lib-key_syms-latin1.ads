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

package X_Lib.Key_Syms.Latin1 is

   -- -------------------------------------------------------------------------
   --
   --   L A T I N   1
   --
   -- Byte 3 is 16#00#
   --
   XK_space                      : constant Key_Sym_ID := 16#20#;
   XK_exclam                     : constant Key_Sym_ID := 16#21#;
   XK_quotedbl                   : constant Key_Sym_ID := 16#22#;
   XK_numbersign                 : constant Key_Sym_ID := 16#23#;
   XK_dollar                     : constant Key_Sym_ID := 16#24#;
   XK_percent                    : constant Key_Sym_ID := 16#25#;
   XK_ampersand                  : constant Key_Sym_ID := 16#26#;
   XK_apostrophe                 : constant Key_Sym_ID := 16#27#;
   XK_quoteright                 : constant Key_Sym_ID := 16#27#; -- deprecated 
   XK_parenleft                  : constant Key_Sym_ID := 16#28#;
   XK_parenright                 : constant Key_Sym_ID := 16#29#;
   XK_asterisk                   : constant Key_Sym_ID := 16#2a#;
   XK_plus                       : constant Key_Sym_ID := 16#2b#;
   XK_comma                      : constant Key_Sym_ID := 16#2c#;
   XK_minus                      : constant Key_Sym_ID := 16#2d#;
   XK_period                     : constant Key_Sym_ID := 16#2e#;
   XK_slash                      : constant Key_Sym_ID := 16#2f#;
   XK_0                          : constant Key_Sym_ID := 16#30#;
   XK_1                          : constant Key_Sym_ID := 16#31#;
   XK_2                          : constant Key_Sym_ID := 16#32#;
   XK_3                          : constant Key_Sym_ID := 16#33#;
   XK_4                          : constant Key_Sym_ID := 16#34#;
   XK_5                          : constant Key_Sym_ID := 16#35#;
   XK_6                          : constant Key_Sym_ID := 16#36#;
   XK_7                          : constant Key_Sym_ID := 16#37#;
   XK_8                          : constant Key_Sym_ID := 16#38#;
   XK_9                          : constant Key_Sym_ID := 16#39#;
   XK_colon                      : constant Key_Sym_ID := 16#3a#;
   XK_semicolon                  : constant Key_Sym_ID := 16#3b#;
   XK_less                       : constant Key_Sym_ID := 16#3c#;
   XK_equal                      : constant Key_Sym_ID := 16#3d#;
   XK_greater                    : constant Key_Sym_ID := 16#3e#;
   XK_question                   : constant Key_Sym_ID := 16#3f#;
   XK_at                         : constant Key_Sym_ID := 16#40#;
   XK_A                          : constant Key_Sym_ID := 16#41#;
   XK_B                          : constant Key_Sym_ID := 16#42#;
   XK_C                          : constant Key_Sym_ID := 16#43#;
   XK_D                          : constant Key_Sym_ID := 16#44#;
   XK_E                          : constant Key_Sym_ID := 16#45#;
   XK_F                          : constant Key_Sym_ID := 16#46#;
   XK_G                          : constant Key_Sym_ID := 16#47#;
   XK_H                          : constant Key_Sym_ID := 16#48#;
   XK_I                          : constant Key_Sym_ID := 16#49#;
   XK_J                          : constant Key_Sym_ID := 16#4a#;
   XK_K                          : constant Key_Sym_ID := 16#4b#;
   XK_L                          : constant Key_Sym_ID := 16#4c#;
   XK_M                          : constant Key_Sym_ID := 16#4d#;
   XK_N                          : constant Key_Sym_ID := 16#4e#;
   XK_O                          : constant Key_Sym_ID := 16#4f#;
   XK_P                          : constant Key_Sym_ID := 16#50#;
   XK_Q                          : constant Key_Sym_ID := 16#51#;
   XK_R                          : constant Key_Sym_ID := 16#52#;
   XK_S                          : constant Key_Sym_ID := 16#53#;
   XK_T                          : constant Key_Sym_ID := 16#54#;
   XK_U                          : constant Key_Sym_ID := 16#55#;
   XK_V                          : constant Key_Sym_ID := 16#56#;
   XK_W                          : constant Key_Sym_ID := 16#57#;
   XK_X                          : constant Key_Sym_ID := 16#58#;
   XK_Y                          : constant Key_Sym_ID := 16#59#;
   XK_Z                          : constant Key_Sym_ID := 16#5a#;
   XK_bracketleft                : constant Key_Sym_ID := 16#5b#;
   XK_backslash                  : constant Key_Sym_ID := 16#5c#;
   XK_bracketright               : constant Key_Sym_ID := 16#5d#;
   XK_asciicircum                : constant Key_Sym_ID := 16#5e#;
   XK_underscore                 : constant Key_Sym_ID := 16#5f#;
   XK_grave                      : constant Key_Sym_ID := 16#60#;
   XK_quoteleft                  : constant Key_Sym_ID := 16#60#; -- deprecated 
   XK_Lower_A                    : constant Key_Sym_ID := 16#61#;
   XK_Lower_B                    : constant Key_Sym_ID := 16#62#;
   XK_Lower_C                    : constant Key_Sym_ID := 16#63#;
   XK_Lower_D                    : constant Key_Sym_ID := 16#64#;
   XK_Lower_E                    : constant Key_Sym_ID := 16#65#;
   XK_Lower_F                    : constant Key_Sym_ID := 16#66#;
   XK_Lower_G                    : constant Key_Sym_ID := 16#67#;
   XK_Lower_H                    : constant Key_Sym_ID := 16#68#;
   XK_Lower_I                    : constant Key_Sym_ID := 16#69#;
   XK_Lower_J                    : constant Key_Sym_ID := 16#6a#;
   XK_Lower_K                    : constant Key_Sym_ID := 16#6b#;
   XK_Lower_L                    : constant Key_Sym_ID := 16#6c#;
   XK_Lower_M                    : constant Key_Sym_ID := 16#6d#;
   XK_Lower_N                    : constant Key_Sym_ID := 16#6e#;
   XK_Lower_O                    : constant Key_Sym_ID := 16#6f#;
   XK_Lower_P                    : constant Key_Sym_ID := 16#70#;
   XK_Lower_Q                    : constant Key_Sym_ID := 16#71#;
   XK_Lower_R                    : constant Key_Sym_ID := 16#72#;
   XK_Lower_S                    : constant Key_Sym_ID := 16#73#;
   XK_Lower_T                    : constant Key_Sym_ID := 16#74#;
   XK_Lower_U                    : constant Key_Sym_ID := 16#75#;
   XK_Lower_V                    : constant Key_Sym_ID := 16#76#;
   XK_Lower_W                    : constant Key_Sym_ID := 16#77#;
   XK_Lower_X                    : constant Key_Sym_ID := 16#78#;
   XK_Lower_Y                    : constant Key_Sym_ID := 16#79#;
   XK_Lower_Z                    : constant Key_Sym_ID := 16#7a#;
   XK_braceleft                  : constant Key_Sym_ID := 16#7b#;
   XK_bar                        : constant Key_Sym_ID := 16#7c#;
   XK_braceright                 : constant Key_Sym_ID := 16#7d#;
   XK_asciitilde                 : constant Key_Sym_ID := 16#7e#;

   XK_nobreakspace               : constant Key_Sym_ID := 16#a0#;
   XK_exclamdown                 : constant Key_Sym_ID := 16#a1#;
   XK_cent                       : constant Key_Sym_ID := 16#a2#;
   XK_sterling                   : constant Key_Sym_ID := 16#a3#;
   XK_currency                   : constant Key_Sym_ID := 16#a4#;
   XK_yen                        : constant Key_Sym_ID := 16#a5#;
   XK_brokenbar                  : constant Key_Sym_ID := 16#a6#;
   XK_section                    : constant Key_Sym_ID := 16#a7#;
   XK_diaeresis                  : constant Key_Sym_ID := 16#a8#;
   XK_copyright                  : constant Key_Sym_ID := 16#a9#;
   XK_ordfeminine                : constant Key_Sym_ID := 16#aa#;
   XK_guillemotleft              : constant Key_Sym_ID := 16#ab#; -- left angle quotation mark 
   XK_notsign                    : constant Key_Sym_ID := 16#ac#;
   XK_hyphen                     : constant Key_Sym_ID := 16#ad#;
   XK_registered                 : constant Key_Sym_ID := 16#ae#;
   XK_macron                     : constant Key_Sym_ID := 16#af#;
   XK_degree                     : constant Key_Sym_ID := 16#b0#;
   XK_plusminus                  : constant Key_Sym_ID := 16#b1#;
   XK_twosuperior                : constant Key_Sym_ID := 16#b2#;
   XK_threesuperior              : constant Key_Sym_ID := 16#b3#;
   XK_acute                      : constant Key_Sym_ID := 16#b4#;
   XK_mu                         : constant Key_Sym_ID := 16#b5#;
   XK_paragraph                  : constant Key_Sym_ID := 16#b6#;
   XK_periodcentered             : constant Key_Sym_ID := 16#b7#;
   XK_cedilla                    : constant Key_Sym_ID := 16#b8#;
   XK_onesuperior                : constant Key_Sym_ID := 16#b9#;
   XK_masculine                  : constant Key_Sym_ID := 16#ba#;
   XK_guillemotright             : constant Key_Sym_ID := 16#bb#; -- right angle quotation mark 
   XK_onequarter                 : constant Key_Sym_ID := 16#bc#;
   XK_onehalf                    : constant Key_Sym_ID := 16#bd#;
   XK_threequarters              : constant Key_Sym_ID := 16#be#;
   XK_questiondown               : constant Key_Sym_ID := 16#bf#;
   XK_Agrave                     : constant Key_Sym_ID := 16#c0#;
   XK_Aacute                     : constant Key_Sym_ID := 16#c1#;
   XK_Acircumflex                : constant Key_Sym_ID := 16#c2#;
   XK_Atilde                     : constant Key_Sym_ID := 16#c3#;
   XK_Adiaeresis                 : constant Key_Sym_ID := 16#c4#;
   XK_Aring                      : constant Key_Sym_ID := 16#c5#;
   XK_AE                         : constant Key_Sym_ID := 16#c6#;
   XK_Ccedilla                   : constant Key_Sym_ID := 16#c7#;
   XK_Egrave                     : constant Key_Sym_ID := 16#c8#;
   XK_Eacute                     : constant Key_Sym_ID := 16#c9#;
   XK_Ecircumflex                : constant Key_Sym_ID := 16#ca#;
   XK_Ediaeresis                 : constant Key_Sym_ID := 16#cb#;
   XK_Igrave                     : constant Key_Sym_ID := 16#cc#;
   XK_Iacute                     : constant Key_Sym_ID := 16#cd#;
   XK_Icircumflex                : constant Key_Sym_ID := 16#ce#;
   XK_Idiaeresis                 : constant Key_Sym_ID := 16#cf#;
   XK_Eth                        : constant Key_Sym_ID := 16#d0#;
   XK_Ntilde                     : constant Key_Sym_ID := 16#d1#;
   XK_Ograve                     : constant Key_Sym_ID := 16#d2#;
   XK_Oacute                     : constant Key_Sym_ID := 16#d3#;
   XK_Ocircumflex                : constant Key_Sym_ID := 16#d4#;
   XK_Otilde                     : constant Key_Sym_ID := 16#d5#;
   XK_Odiaeresis                 : constant Key_Sym_ID := 16#d6#;
   XK_multiply                   : constant Key_Sym_ID := 16#d7#;
   XK_Ooblique                   : constant Key_Sym_ID := 16#d8#;
   XK_Ugrave                     : constant Key_Sym_ID := 16#d9#;
   XK_Uacute                     : constant Key_Sym_ID := 16#da#;
   XK_Ucircumflex                : constant Key_Sym_ID := 16#db#;
   XK_Udiaeresis                 : constant Key_Sym_ID := 16#dc#;
   XK_Yacute                     : constant Key_Sym_ID := 16#dd#;
   XK_Thorn                      : constant Key_Sym_ID := 16#de#;
   XK_ssharp                     : constant Key_Sym_ID := 16#df#;
   XK_Lower_Agrave               : constant Key_Sym_ID := 16#e0#;
   XK_Lower_Aacute               : constant Key_Sym_ID := 16#e1#;
   XK_Lower_Acircumflex          : constant Key_Sym_ID := 16#e2#;
   XK_Lower_Atilde               : constant Key_Sym_ID := 16#e3#;
   XK_Lower_Adiaeresis           : constant Key_Sym_ID := 16#e4#;
   XK_Lower_Aring                : constant Key_Sym_ID := 16#e5#;
   XK_Lower_AE                   : constant Key_Sym_ID := 16#e6#;
   XK_Lower_Ccedilla             : constant Key_Sym_ID := 16#e7#;
   XK_Lower_Egrave               : constant Key_Sym_ID := 16#e8#;
   XK_Lower_Eacute               : constant Key_Sym_ID := 16#e9#;
   XK_Lower_Ecircumflex          : constant Key_Sym_ID := 16#ea#;
   XK_Lower_Ediaeresis           : constant Key_Sym_ID := 16#eb#;
   XK_Lower_Igrave               : constant Key_Sym_ID := 16#ec#;
   XK_Lower_Iacute               : constant Key_Sym_ID := 16#ed#;
   XK_Lower_Icircumflex          : constant Key_Sym_ID := 16#ee#;
   XK_Lower_idiaeresis           : constant Key_Sym_ID := 16#ef#;
   XK_Lower_Eth                  : constant Key_Sym_ID := 16#f0#;
   XK_Lower_Ntilde               : constant Key_Sym_ID := 16#f1#;
   XK_Lower_Ograve               : constant Key_Sym_ID := 16#f2#;
   XK_Lower_Oacute               : constant Key_Sym_ID := 16#f3#;
   XK_Lower_Ocircumflex          : constant Key_Sym_ID := 16#f4#;
   XK_Lower_Otilde               : constant Key_Sym_ID := 16#f5#;
   XK_Lower_Odiaeresis           : constant Key_Sym_ID := 16#f6#;
   XK_division                   : constant Key_Sym_ID := 16#f7#;
   XK_oslash                     : constant Key_Sym_ID := 16#f8#;
   XK_Lower_Ugrave               : constant Key_Sym_ID := 16#f9#;
   XK_Lower_Uacute               : constant Key_Sym_ID := 16#fa#;
   XK_Lower_Ucircumflex          : constant Key_Sym_ID := 16#fb#;
   XK_Lower_Udiaeresis           : constant Key_Sym_ID := 16#fc#;
   XK_Lower_Yacute               : constant Key_Sym_ID := 16#fd#;
   XK_Lower_Thorn                : constant Key_Sym_ID := 16#fe#;
   XK_ydiaeresis                 : constant Key_Sym_ID := 16#ff#;

end X_Lib.Key_Syms.Latin1;
