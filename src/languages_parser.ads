with String_Helpers; use String_Helpers;
with Ada.Finalization;
with Logging; use Logging;
with Languages;
with Keys; use Keys;

package Languages_Parser is
   --  <language spec>     ::= <section> *
   --  <section>           ::= '.' <section type>
   --  <section type>      ::= <substitutions> | <keys>
   --  <substitutions>     ::= <substitution type> NL <substitution body> *
   --  <substitution type> ::= 'left' | 'middle' | 'right'
   --  <substitution body> ::= <string> '=' <string> NL
   --  <string>            ::= <character> *
   --  <keys>              ::= <key name> NL <keys body>*
   --  <keys body>         ::= <key name> '=' <character> NL
   --  <key name>          ::= 'LZ' | 'RJ' | 'MSHI' | 'NOKEY' | ...
   --  <character>         ::= Is_Graphic
   --
   --  TOKENS: . = <string>
   --  <string> can be special case inits, tails, key name and character
   
   Unexpected_Symbol : exception;
   Parse_Error : exception;
   End_Of_File : exception;
   
   procedure Parse (File_Name : String);
private
   use Unbounded;

   type Token_Variant is
     (Token_String,
      Token_Period,
      Token_Equals,
      Token_None);

   type Token_Type (Variant : Token_Variant := Token_None) is record
      case Variant is
         when Token_String =>
            String_Value : Unbounded_Wide_Wide_String;
         when others =>
            null;
      end case;
   end record;

   type Lexer_State is new Ada.Finalization.Limited_Controlled with record
      File : IO.File_Type;
      Buffer : Unbounded_Wide_Wide_String;
      In_String_State : Boolean;
      String_Terminator : Wide_Wide_Character;
      Last_Token : Token_Type;
      Line_Number : Positive := 1;
   end record;
   procedure Finalize (State : in out Lexer_State);
   
   procedure Advance (State : Lexer_State);
   
   procedure Accept_Token (State : in out Lexer_State);
   function Next_Token (State : in out Lexer_State) return Token_Type;

   procedure Language_Spec (State : in out Lexer_State);
   procedure Section (State : in out Lexer_State);
   function New_Section (State : in out Lexer_State) return Boolean;
   procedure Substitutions (State : in out Lexer_State);
   procedure Position_Name (State : in out Lexer_State; Position : out Languages.Substitution_Type);
   procedure Substitution_Body (State : in out Lexer_State; Pattern : out Unbounded_Wide_Wide_String; Replacement : out Unbounded_Wide_Wide_String);
   procedure Graphic_String (State : in out Lexer_State; Out_String : out Unbounded_Wide_Wide_String);
   procedure Keys (State : in out Lexer_State);
   procedure Key_Name (State : in out Lexer_State; Out_Key : out Softkey);
   procedure Keys_Body (State : in out Lexer_State; Out_Key : out Softkey; Out_Character : out Wide_Wide_Character);
   procedure Graphic_Character (State : in out Lexer_State; Out_Character : out Wide_Wide_Character);
   function Expecting (State : in out Lexer_State; Variant : Token_Variant) return Token_Type;
end Languages_Parser;
