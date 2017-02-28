with Ada.Strings.Unbounded;
with Ada.Text_IO;
private with Ada.Containers.Ordered_Maps;
private with Ada.Characters.Latin_1;
private with Ada.Characters.Handling;

with Logging;

package Dictionaries is
   use Logging;
   use Ada.Strings.Unbounded;
   
   PARSE_ERROR : Exception;

   type Dictionary is private;
   procedure Add_Definition (To_Dictionary : in out Dictionary; From : Unbounded_String; To : Unbounded_String);
   function Apply_To (Using_Dictionary : in out Dictionary; Text : Unbounded_String) return Unbounded_String;
   function Parse (Source : Ada.Text_IO.File_Type) return Dictionary;
private
   package Dictionary_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String, Element_Type => Unbounded_String);
   type Dictionary is record
      --  Keeping the possibility open of having dictionary entries for both
      --  syllable replacements and word replacements. Currently only syllable
      --  ones used.
      Syllables : Dictionary_Maps.Map;
   end record;
end Dictionaries;
