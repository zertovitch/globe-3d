with vrml_dfa; use vrml_dfa;
--  Warning: This file is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.

with Text_IO; use Text_IO;

package vrml_io is
--  Warning: This file is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.

  user_input_file  : File_Type;
  user_output_file : File_Type;
  NULL_IN_INPUT : exception;
  AFLEX_INTERNAL_ERROR : exception;
  UNEXPECTED_LAST_MATCH : exception;
  PUSHBACK_OVERFLOW : exception;
  AFLEX_SCANNER_JAMMED : exception;
  type eob_action_type is ( EOB_ACT_RESTART_SCAN,
                            EOB_ACT_END_OF_FILE,
                            EOB_ACT_LAST_MATCH );
  YY_END_OF_BUFFER_CHAR :  constant Character :=  ASCII.NUL;
  yy_n_chars : Integer;       -- number of characters read into yy_ch_buf

  -- True when we've seen an EOF for the current input file
  yy_eof_has_been_seen : Boolean;


  procedure YY_INPUT (buf: out unbounded_character_array; result: out Integer; max_size: in Integer);
  function yy_get_next_buffer return eob_action_type;
  procedure yyUnput( c : Character; yy_bp: in out Integer );
  procedure Unput(c : Character);
  function Input return Character;
  procedure Output(c : Character);
  procedure Output_New_Line;
  function Output_Column return Text_IO.Count;
  function Input_Line return Text_IO.Count;
  function yyWrap return Boolean;
  procedure Open_Input(fname : in String);
  procedure Close_Input;
  procedure Create_Output(fname : in String := "");
  procedure Close_Output;


end vrml_io;
