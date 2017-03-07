package body Languages_Parser is
   procedure Parse (File_Name : String) is
      State : Lexer_State;
   begin
      IO.Open (State.File, IO.In_File, File_Name, "ENCODING=UTF8,WCEM=8");
      begin
         Language_Spec (State);
      exception
         when others =>
            Log.Error
              ("[Languages_Parser] Caught exception when parsing " &
               File_Name & " on line " &
               Positive'Image (State.Line_Number));
            IO.Close (State.File);
            raise;
      end;
      IO.Close (State.File);
   end Parse;
   
   function Next_Token (State : in out Lexer_State) return Token_Type is
      Symbol : UChar;
      Newline : Boolean;
      package Latin_1 renames Ada.Characters.Latin_1;
      
      procedure Enter_String_State (Terminator : UChar) is
      begin
         State.Buffer := +"";
         State.In_String_State := True;
         State.String_Terminator := Terminator;
      end Enter_String_State;

      function Exit_String_State return Token_Type is
         String_Value : UString := State.Buffer;
      begin
         if not State.In_String_State Then
            raise Parse_Error
              with "Trying to exit string state without being in it";
         end if;
         State.In_String_State := False;
         State.Last_Token := (Token_String, String_Value);
         return State.Last_Token;
      end Exit_String_State;
   begin
      if State.Last_Token.Variant /= Token_None then
         return State.Last_Token;
      end if;
      while not IO.End_Of_File (State.File) loop
         IO.Look_Ahead (State.File, Symbol, Newline);
--         Ada.Text_IO.Put_Line ("Found symbol <" & Symbol & "> and newline? " & (if Newline then "yes" else "no"));
         if Newline and State.In_String_State and State.String_Terminator = +Latin_1.LF then
            return Exit_String_State;
         elsif Newline then
            State.Line_Number := State.Line_Number + 1;
            Advance (State);
         elsif Symbol = ' ' then
            --  Other kinds of whitespace needn't be covered here because
            --  they're not considered "graphic" characters
            raise Parse_Error with "Whitespace not allowed in definitions";
         elsif Symbol = '.' and not State.In_String_State then
            Enter_String_State (+Latin_1.LF);
            Advance (State);
            State.Last_Token := (Variant => Token_Period);
            return State.Last_Token;
         elsif Symbol = State.String_Terminator and State.In_String_State then
            return Exit_String_State;
         elsif Symbol = State.String_Terminator then
            Enter_String_State (+Latin_1.LF);
            Advance (State);
            State.Last_Token := (Variant => Token_Equals);
            return State.Last_Token;
         elsif Wide_Wide_Character'Pos (Symbol) > 16#20# then
            if not State.In_String_State then
               Enter_String_State ('=');
            end if;
            State.Buffer := State.Buffer & Symbol;
            Advance (State);
         else
            raise Unexpected_Symbol;
         end if;
      end loop;
      if State.In_String_State then
         return Exit_String_State;
      end if;

      raise End_Of_File;
   end Next_Token;

   procedure Accept_Token (State : in out Lexer_State) is
   begin
      State.Last_Token := (Variant => Token_None);
   end Accept_Token;
   
   procedure Language_Spec (State : in out Lexer_State) is
   begin
      loop
         begin
            Section (State);
         exception
            when End_Of_File =>
               return;
         end;
      end loop;
   end Language_Spec;
   
   procedure Section (State : in out Lexer_State) is
      Unused : Token_Type;
   begin
      Unused := Expecting (State, Token_Period);
      Accept_Token (State);
      begin
         Substitutions (State);
      exception
         when Parse_Error =>
            Keys (State);
      end;
   end Section;
   
   function New_Section (State : in out Lexer_State) return Boolean is
      Next : Token_Type;
   begin
      Next := Next_Token (State);
      return Next.Variant = Token_Period;
   end New_Section;
   
   procedure Substitutions (State : in out Lexer_State) is
      Position : Languages.Substitution_Type;
      Pattern : UString;
      Replacement : UString;
      Unused : Token_Type;
   begin
      Position_Name (State, Position);
      Accept_Token (State);
      loop
         Substitution_Body (State, Pattern, Replacement);
         Accept_Token (State);
         Languages.User_Language.Add_Substitution (Position, Pattern, Replacement);
         if New_Section (State) then
            exit;
         end if;
      end loop;
   end Substitutions;
   
   procedure Position_Name (State : in out Lexer_State; Position : out Languages.Substitution_Type) is
      Position_Name : Token_Type;
   begin
      Position_Name := Expecting (State, Token_String);
      if Position_Name.String_Value = +"left" then
         Position := Languages.Left;
      elsif Position_Name.String_Value = +"middle" then
         Position := Languages.Middle;
      elsif Position_Name.String_Value = +"right" then
         Position := Languages.Right;
      else
         raise Parse_Error with
           "string """ & (+Position_Name.String_Value) &
           """ is not one of (left, middle, right)";
      end if;
   end Position_Name;

   procedure Substitution_Body (State : in out Lexer_State; Pattern : out UString; Replacement : out UString) is
      Unused : Token_Type;
   begin
      Graphic_String (State, Pattern);
      Accept_Token (State);
      Unused := Expecting (State, Token_Equals);
      Accept_Token (State);
      Graphic_String (State, Replacement);
   end Substitution_Body;

   procedure Graphic_String (State : in out Lexer_State; Out_String : out UString) is
      Token : Token_Type := Next_Token (State);
   begin
      Token := Expecting (State, Token_String);
      Out_String := Token.String_Value;
   end Graphic_String;
   
   procedure Keys (State : in out Lexer_State) is
      Modifier : Softkey;
      Key : Softkey;
      Symbol : UChar;
      Unused : Token_Type;
      Next : Token_Type;
   begin
      Key_Name (State, Modifier);
      Accept_Token (State);
      loop
         Keys_Body (State, Key, Symbol);
         Accept_Token (State);
         Languages.User_Language.Add_Key (Modifier, Key, Symbol);
         if New_Section (State) then
            exit;
         end if;
      end loop;
   end Keys;

   procedure Keys_Body (State : in out Lexer_State; Out_Key : out Softkey; Out_Character : out UChar) is
      Unused : Token_Type;
   begin
      Key_Name (State, Out_Key);
      Accept_Token (State);
      Unused := Expecting (State, Token_Equals);
      Accept_Token (State);
      Graphic_Character (State, Out_Character);
   end Keys_Body;
   
   procedure Key_Name (State : in out Lexer_State; Out_Key : out Softkey) is
      Key_Name : Token_Type;
   begin
      Key_Name := Expecting (State, Token_String);
      Out_Key := Softkey'Value (+ (Key_Name.String_Value));
   end Key_Name;

   procedure Graphic_Character (State : in out Lexer_State; Out_Character : out UChar) is
      Token : Token_Type;
   begin
      Token := Expecting (State, Token_String);
      if Length (Token.String_Value) = 1 then
         Out_Character := Element (Token.String_Value, 1);
         return;
      end if;
      raise Parse_Error with
        "string """ & (+Token.String_Value) &
        """ does not represent a character";
   end Graphic_Character;

   function Expecting (State : in out Lexer_State; Variant : Token_Variant) return Token_Type is
      Token : Token_Type := Next_Token (State);
   begin
      if Token.Variant = Variant then
         return Token;
      else
         raise Parse_Error with
           "wrong token type (expected " &
           Token_Variant'Image (Variant) & ", got "
           & Token_Variant'Image (Token.Variant) & ")";
      end if;
   end Expecting;

   procedure Advance (State : Lexer_State) is
      Symbol : UChar;
      Newline : Boolean;
   begin
      IO.Look_Ahead (State.File, Symbol, Newline);
      if Newline then
         IO.Get_Immediate (State.File, Symbol);
      else
         IO.Get (State.File, Symbol);
      end if;
   end Advance;
   
end Languages_Parser;
