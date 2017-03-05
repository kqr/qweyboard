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
   --      ::= 'left' | 'middle' | 'right'
   --  <substitution body>
   --      ::= e
   --       |  <string> '=' <string> <newline>
   --       |  <substitution body>
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
   --      ::= Is_Graphic
   --
   --  TOKENS: [ = ] <string>
   --  <string> can be special case inits, tails, key name and character
   
   Unexpected_Symbol : exception;
   Parse_Error : exception;
   End_Of_File : exception;
   
   procedure Parse (File_Name : String);
private
   type Token_Variant is
     (Token_String,
      Token_Open_Bracket,
      Token_Close_Bracket,
      Token_Equals,
      Token_None);

   type Token_Type (Variant : Token_Variant := Token_None) is record
      case Variant is
         when Token_String =>
            String_Value : Unbounded_String;
         when Token_Open_Bracket => null;
         when Token_Close_Bracket => null;
         when Token_Equals => null;
         when Token_None => null;
      end case;
   end record;

   type Lexer_State is record
      --  TODO: set up a controlled type around this?
      File : File_Type;
      Buffer : Unbounded_String;
      Buffer_Open : Boolean := False;
      Last_Token : Token_Type;
   end record;

   procedure Advance (State : Lexer_State);
   
   procedure Accept_Token (State : in out Lexer_State);
   function Next_Token (State : in out Lexer_State) return Token_Type;

   procedure Language_Spec (State : in out Lexer_State);
   procedure Section (State : in out Lexer_State);
   function New_Section (State : in out Lexer_State) return Boolean;
   procedure Substitutions (State : in out Lexer_State);
   procedure Position_Name (State : in out Lexer_State; Position : out Languages.Substitution_Type);
   procedure Substitution_Body (State : in out Lexer_State; Pattern : out Unbounded_String; Replacement : out Unbounded_String);
   procedure Graphic_String (State : in out Lexer_State; Out_String : out Unbounded_String);
   procedure Keys (State : in out Lexer_State);
   procedure Key_Name (State : in out Lexer_State; Out_Key : out Softkey);
   procedure Keys_Body (State : in out Lexer_State; Out_Key : out Softkey; Out_Character : out Character);
   procedure Graphic_Character (State : in out Lexer_State; Out_Character : out Character);
   function Expecting (State : in out Lexer_State; Variant : Token_Variant) return Token_Type;
end Languages_Parser;
