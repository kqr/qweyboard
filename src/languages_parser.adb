package body Languages_Parser is
   
   function Parse (File_Name : String) return Languages.Language is
      State : Lexer_State;
      K : Keys.Softkey;
      C : Character;
   begin
      --  TODO also catch missing file exception?
      Ada.Text_IO.Open (State.File, Ada.Text_IO.In_File, File_Name);
      begin
         Keys_Body (State, K, C);
         Ada.Text_IO.Put_Line ("Got key " & Softkey'Image (K) & " with character " & C);
      exception
         --  TODO: exception handling here as well, if the parser fails?
         when others => raise;
      end;
      Ada.Text_IO.Close (State.File);
      return Languages.Get_Standard;
   end Parse;

   function Next_Token (State : in out Lexer_State) return Token_Type is
      Symbol : Character;
      Newline : Boolean;

      function String_Token return Token_Type is
         String_Value : Unbounded_String := State.Buffer;
      begin
         State.Buffer := To_Unbounded_String ("");
         State.Buffer_Open := False;
         return (Token_String, String_Value);
      end String_Token;
   begin
      while not Ada.Text_IO.End_Of_File (State.File) loop
         Ada.Text_IO.Look_Ahead (State.File, Symbol, Newline);
--         Ada.Text_IO.Put_Line ("Found symbol <" & Symbol & "> and newline? " & (if Newline then "yes" else "no"));
         if Newline and State.Buffer_Open then
            return String_Token;
         elsif Newline then
            Advance (State);
         elsif Symbol = ' ' and not State.Buffer_Open then
            Advance (State);
         elsif Symbol = '[' and not State.Buffer_Open then
            State.Buffer_Open := True;
            Advance (State);
            return (Variant => Token_Open_Bracket);
         elsif Symbol = ']' and State.Buffer_Open then
            return String_Token;
         elsif Symbol = ']' then
            Advance (State);
            return (Variant => Token_Close_Bracket);
         elsif Symbol = '=' and State.Buffer_Open then
            return String_Token;
         elsif Symbol = '=' then
            Advance (State);
            State.Buffer_Open := True;
            return (Variant => Token_Equals);
         elsif Ada.Characters.Handling.Is_Graphic (Symbol) and State.Buffer_Open then
            State.Buffer := State.Buffer & Symbol;
            Advance (State);
         elsif Ada.Characters.Handling.Is_Graphic (Symbol) then
            State.Buffer_Open := True;
         else
            raise Unexpected_Symbol;
         end if;
      end loop;
      if State.Buffer_Open then
         return String_Token;
      end if;

      return (Variant => Token_End_Of_File);
   end Next_Token;

   procedure Keys_Body (State : in out Lexer_State; Out_Key : out Softkey; Out_Character : out Character) is
      Unused : Token_Type;
   begin
      Key_Name (State, Out_Key);
      Unused := Expecting (State, Token_Equals);
      Graphic_Character (State, Out_Character);
   end Keys_Body;

   procedure Key_Name (State : in out Lexer_State; Out_Key : out Softkey) is
      Token : Token_Type;
   begin
      Token := Expecting (State, Token_String);
      for Key in Softkey'Range loop
         if Softkey'Image (Key) = Token.String_Value then
            Ada.Text_IO.Put_Line ("Successfully parsed key " & Softkey'Image (Key));
            Out_Key := Key;
            return;
         end if;
      end loop;
      raise Parse_Error with
        "string """ & To_String (Token.String_Value) &
        """ does not match existing key";
   end Key_Name;

   procedure Graphic_Character (State : in out Lexer_State; Out_Character : out Character) is
      Token : Token_Type;
   begin
      Token := Expecting (State, Token_String);
      if Length (Token.String_Value) = 1 then
         Out_Character := Element (Token.String_Value, 1);
         return;
      end if;
      raise Parse_Error with
        "string """ & To_String (Token.String_Value) &
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
      Symbol : Character;
      Newline : Boolean;
   begin
      Ada.Text_IO.Look_Ahead (State.File, Symbol, Newline);
      if Newline then
         Ada.Text_IO.Get_Immediate (State.File, Symbol);
      else
         Ada.Text_IO.Get (State.File, Symbol);
      end if;
   end Advance;
   
end Languages_Parser;
