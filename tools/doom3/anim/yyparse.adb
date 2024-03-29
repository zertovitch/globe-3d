
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

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.

with YY_Sizes;
-- ^ 14-Jan-2006 (GdM): configurable sizes instead of hard-coded
--   ones in AYACC's output

procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Md5_Goto;
    package yy_shift_reduce_tables renames
      Md5_Shift_Reduce;
    package yy_tokens              renames
      Md5_Tokens;
    package yy_io                  renames -- (+GdM 2008)
      Md5_IO;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       --  Affects error 'Stack size exceeded on state_stack'
       stack_size : constant Natural := yy_sizes.stack_size; -- was 300, then 8192

       -- subtype rule         is Natural;
       subtype parse_state  is Natural;
       -- subtype nonterminal  is Integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : Natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.YYSType;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : Integer;
       rule_id            : Rule;
       input_symbol       : yy_tokens.Token:= Error;


       -- error recovery flag
       error_flag : Natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : Boolean := True;
       index      : Integer;

       -- Is Debugging option on or off
       debug : constant Boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : Nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.Token) return Integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : Nonterminal) return yy.parse_state is
        index : Integer;
    begin
        index := goto_offset(state);
        while Integer (Goto_Matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return Integer (Goto_Matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return Integer is
        index      : Integer;
        tok_pos    : Integer;
        default    : constant Integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := Shift_Reduce_Offset(state);
        while Integer (Shift_Reduce_Matrix(index).t) /= tok_pos and then
              Integer (Shift_Reduce_Matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return Integer (shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure Handle_Error is
      temp_action : Integer;
    begin

      if yy.error_flag = 3 then  --  no shift yet, clobber input.
      if yy.debug then
          Text_IO.Put_Line("  -- Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'Image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  --  don't discard,
        if yy.debug then
            Text_IO.Put_Line("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quitting...");
        end if;
        raise yy_tokens.Syntax_Error;
        end if;

        yy.look_ahead := True;   --  get next token
        return;                  --  and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        if yy_io.Input_Line > 1 then
            yyerror("Syntax Error at line" & Text_IO.Count'Image(yy_io.Input_Line));
        else
            yyerror("Syntax Error at line 1 (or possibly later and the AFLex -E option was omitted).");
        end if;
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        Text_IO.Put_Line("  -- Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          Text_IO.Put_Line("  -- Ayacc.YYParse: Examining State " &
               yy.parse_state'Image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    Text_IO.Put_Line("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            Text_IO.Put_Line("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        Text_IO.Put_Line("  -- Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'Image(yy.state_stack(yy.tos)));
    end if;

    end Handle_Error;

   --  Print debugging information for a shift operation
   procedure Shift_Debug (state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       Text_IO.Put_Line("  -- Ayacc.YYParse: Shift "& yy.parse_state'Image(state_id)&" on input symbol "&
               yy_tokens.token'Image(lexeme) );
   end Shift_Debug;

   --  Print debugging information for a reduce operation
   procedure Reduce_Debug (rule_id: rule; state_id: yy.parse_state) is
   begin
       Text_IO.Put_Line("  -- Ayacc.YYParse: Reduce by rule "&rule'Image(rule_id)&" goto state "&
               yy.parse_state'Image(state_id));
   end Reduce_Debug;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := True;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := Shift_Reduce_Offset(yy.state_stack(yy.tos));
        if Integer (shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := Integer (shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := False;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
              Shift_Debug (yy.action, yy.input_symbol);
            end if;

            --  Enter new state
            if yy.tos = yy.stack_size then
                Text_IO.Put_Line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := YYLVal;

        if yy.error_flag > 0 then  --  Indicate a valid shift
          yy.error_flag := yy.error_flag - 1;
        end if;

            --  Advance lookahead
            yy.look_ahead := True;

        elsif yy.action = yy.error_code then       -- ERROR

            Handle_Error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                Text_IO.Put_Line("  -- Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when 1 => -- #line 81
MD5_Help.YY_ACCEPT;

when 2 => -- #line 82
MD5_Help.YY_ABORT;

when 5 => -- #line 96
 MD5_Help.num_joints:= Integer(yylval.intval); 

when 6 => -- #line 98
 MD5_Help.num_meshes:= Integer(yylval.intval); 

when 7 => -- #line 99
 Ada.Text_IO.Put_Line(Current_Error, "Mesh or anim ?"); 
              Ada.Text_IO.Put_Line(Current_Error, "Mesh file."); 
              Ada.Text_IO.Put_Line(Current_Error, "  Skeleton has" & Integer'Image(MD5_Help.num_joints) & " joints.");
              Ada.Text_IO.Put_Line(Current_Error, "  There is/are" & Integer'Image(MD5_Help.num_meshes) & " mesh(es).");
            

when 8 => -- #line 105
 Ada.Text_IO.Put_Line(Current_Error, "  Mesh list..."); 

when 10 => -- #line 111
 Ada.Text_IO.Put_Line(Current_Error, "  Joint list..."); 

when 17 => -- #line 139
 Ada.Text_IO.Put_Line(Current_Error, "    Mesh..."); 

when 18 => -- #line 143
 Ada.Text_IO.Put_Line(Current_Error, "      Vertices  :" & Integer'Image(Integer(yylval.intval))); 

when 19 => -- #line 146
 Ada.Text_IO.Put_Line(Current_Error, "      Triangles :" & Integer'Image(Integer(yylval.intval))); 

when 20 => -- #line 149
 Ada.Text_IO.Put_Line(Current_Error, "      Weights   :" & Integer'Image(Integer(yylval.intval))); 

when 33 => -- #line 206
 MD5_Help.num_frames:= Integer(yylval.intval); 

when 34 => -- #line 208
 MD5_Help.num_joints:= Integer(yylval.intval); 

when 35 => -- #line 210
 MD5_Help.frame_rate:= Integer(yylval.intval); 

when 36 => -- #line 212
 MD5_Help.num_animated_components:= Integer(yylval.intval); 

when 37 => -- #line 213
 Ada.Text_IO.Put_Line(Current_Error, "Mesh or anim ?"); 
              Ada.Text_IO.Put_Line(Current_Error, "Animation file.");
              Ada.Text_IO.Put_Line(Current_Error, "  Animation has" & Integer'Image(MD5_Help.num_frames) & " frame(s).");
              Ada.Text_IO.Put_Line(Current_Error, "  Frame rate is" & Integer'Image(MD5_Help.frame_rate) & " frames per second."); 
              Ada.Text_IO.Put_Line(Current_Error, "  Animated components per frame:" & Integer'Image(MD5_Help.num_animated_components)); 
              Ada.Text_IO.Put_Line(Current_Error, "  Skeleton has" & Integer'Image(MD5_Help.num_joints) & " joints.");
            

when 39 => -- #line 228
 Ada.Text_IO.Put_Line(Current_Error, "  Joint list..."); 

when 44 => -- #line 253
 Ada.Text_IO.Put_Line(Current_Error, "    Bounding box list..."); 

when 49 => -- #line 272
 Ada.Text_IO.Put_Line(Current_Error, "    Base frame data..."); 

when 56 => -- #line 296
 Ada.Text_IO.Put_Line(Current_Error, "    Frame #" & Integer'Image(Integer(yylval.intval))); 

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            if yy.rule_id < 0 then
              raise Constraint_Error with "yy.rule_id = " & Integer'Image (yy.rule_id) & " < 0";
            end if;
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                Text_IO.Put_Line (" Stack size exceeded on state_stack");
                raise yy_Tokens.Syntax_Error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 Get_LHS_Rule (yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               Get_LHS_Rule (yy.rule_id)));
            end if;

        end if;


    end loop;


end YYParse;
