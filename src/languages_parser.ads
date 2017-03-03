with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Languages;
with Keys; use Keys;

package Languages_Parser is
   --  <language spec>
   --      ::= e | <section> <language_spec>
   --  
   --  <section>
   --      ::= '[' <section type>
   --  <section type>
   --      ::= <substitutions> | <keys>
   --  
   --  <substitutions>
   --      ::= <substitution type> ']' <newline>
   --          <substitution body>
   --  <substitution type>
   --      ::= 'inits' | 'tails'
   --  <substitution body>
   --      ::= <string> '=' <string> <newline>
   --  <string>
   --      ::= e | <character> <string>
   --  
   --  <keys>
   --      ::= <key name> ']' <newline>
   --          <keys body>
   --  <keys body>
   --      ::= e
   --       | <key name> '=' <character> <newline>
   --         <keys body>
   --  <key name>
   --      ::= 'LZ' | 'RJ' | 'MSHI' | 'NOKEY' | ...
   --  <character>
   --      ::= Non_Control_Character
   --
   --  TOKENS: [ = ] <string>
   --  <string> can be special case inits, tails, key name and character
   
   Unexpected_Symbol : exception;
   Parse_Error : exception;
   
   function Parse (File_Name : String) return Languages.Language;
private
   type Lexer_State is record
      --  TODO: set up a controlled type around this?
      File : File_Type;
      Buffer : Unbounded_String;
      Buffer_Open : Boolean := False;
   end record;

   procedure Advance (State : Lexer_State);
   
   type Token_Variant is
     (Token_String,
      Token_Open_Bracket,
      Token_Close_Bracket,
      Token_Equals,
      Token_End_Of_File,
      Token_Unknown);

   type Token_Type (Variant : Token_Variant := Token_Unknown) is record
      case Variant is
         when Token_String =>
            String_Value : Unbounded_String;
         when Token_Open_Bracket => null;
         when Token_Close_Bracket => null;
         when Token_Equals => null;
         when Token_End_Of_File => null;
         when Token_Unknown => null;
      end case;
   end record;
   
   function Next_Token (State : in out Lexer_State) return Token_Type;
   procedure Keys_Body (State : in out Lexer_State; Out_Key : out Softkey; Out_Character : out Character);
   procedure Key_Name (State : in out Lexer_State; Out_Key : out Softkey);
   procedure Graphic_Character (State : in out Lexer_State; Out_Character : out Character);
   function Expecting (State : in out Lexer_State; Variant : Token_Variant) return Token_Type;

end Languages_Parser;
