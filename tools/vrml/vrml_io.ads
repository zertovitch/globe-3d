with vrml_dfa; use vrml_dfa;
--  Warning: This file is automatically generated by AFLEX.
--  *******   It is useless to modify it. Change the ".Y" & ".L" files instead.

with Text_IO; use Text_IO;

package vrml_io is
--  Warning: This file is automatically generated by AFLEX.
--  *******   It is useless to modify it. Change the ".Y" & ".L" files instead.

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
  YY_END_OF_BUFFER_CHAR :  constant Character:=  ASCII.NUL;
  yy_n_chars : Integer;       --  Number of characters read into yy_ch_buf

  --  True when we've seen an EOF for the current input file
  yy_eof_has_been_seen : Boolean;

-- UMASS CODES :
  --   In order to support YY_Get_Token_Line, we need
  --   a variable to hold current line.
  type String_Ptr is access String;
  Saved_Tok_Line1 : String_Ptr := null;
  Line_Number_Of_Saved_Tok_Line1 : Integer := 0;
  Saved_Tok_Line2 : String_Ptr := null;
  Line_Number_Of_Saved_Tok_Line2 : Integer := 0;
  --  Aflex will try to get next buffer before it processs the
  --  last token. Since now Aflex has been changed to accept
  --  one line by one line, the last token in the buffer is
  -- always end_of_line ( or end_of_buffer ). So before the
  -- end_of_line is processed, next line will be retrieved
  -- into the buffer. So we need to maintain two lines,
  -- which line will be returned in Get_Token_Line is
  -- determined according to the line number. It is the same
  -- reason that we can not reinitialize tok_end_col to 0 in
  -- Yy_Input, but we must do it in yylex after we process the
  -- end_of_line.
  Tok_Begin_Line : Integer := 1;
  Tok_End_Line   : Integer := 1;
  Tok_End_Col    : Integer := 0;
  Tok_Begin_Col  : Integer := 0;
  Token_At_End_Of_Line : Boolean := False;
  -- Indicates whether or not last matched token is end_of_line.
-- END OF UMASS CODES.

  procedure YY_INPUT (buf: out unbounded_character_array; result: out Integer; max_size: in Integer);
  function yy_get_next_buffer return eob_action_type;
  procedure yyUnput (c : Character; yy_bp: in out Integer);
  procedure Unput (c : Character);
  function Input return Character;
  procedure Output(c : Character);
  procedure Output_New_Line;
  function Output_Column return Text_IO.Count;
  function Input_Line return Text_IO.Count;
  function yyWrap return Boolean;
  procedure Open_Input (fname : in String);
  procedure Close_Input;
  procedure Create_Output (fname : in String := "");
  procedure Close_Output;

-- UMASS CODES :
  procedure Yy_Get_Token_Line ( Yy_Line_String : out String;
                                Yy_Line_Length : out Natural );
  --  Returns the entire line in the input, on which the currently
  --  matched token resides.

  function Yy_Line_Number return Natural;
  --  Returns the line number of the currently matched token.
  --  In case a token spans lines, then the line number of the first line
  --  is returned.

  function Yy_Begin_Column return Natural;
  function Yy_End_Column return Natural;
  --  Returns the beginning and ending column positions of the
  --  currently matched token. If the token spans lines then the
  --  begin column number is the column number on the first line
  --  and the end columne number is the column number on the last line.

-- END OF UMASS CODES.

end vrml_io;
