with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;

package Unicode_Strings is
   --  Let's say this is for "unicode string"... or "unbounded string"...
   --  or "unternationally workable string"... or "unefficient string"...
   subtype UString is Unbounded_Wide_Wide_String;
   subtype UChar is Wide_Wide_Character;
   

   package IO renames Ada.Wide_Wide_Text_IO;
   
   function "&" (Left : UString; Right : UChar) return UString
     renames Ada.Strings.Wide_Wide_Unbounded."&";
   function "&" (Left : UString; Right : UString) return UString
     renames Ada.Strings.Wide_Wide_Unbounded."&";
   function "=" (Left : UString; Right : UString) return Boolean
     renames Ada.Strings.Wide_Wide_Unbounded."=";
   function "<" (Left : UString; Right : UString) return Boolean
     renames Ada.Strings.Wide_Wide_Unbounded."<";
   function Length (Item : UString) return Natural
     renames Ada.Strings.Wide_Wide_Unbounded.Length;
   function Element (Item : UString; Index : Positive) return UChar
     renames Ada.Strings.Wide_Wide_Unbounded.Element;
   function Slice (Item : UString; Low : Positive; High : Natural) return UString
     renames Ada.Strings.Wide_Wide_Unbounded.Unbounded_Slice;
   procedure Replace_Slice (Item : in out UString; Low : Positive; High : Natural; By : Wide_Wide_String)
     renames Ada.Strings.Wide_Wide_Unbounded.Replace_Slice;

   function "+" (Item : Character) return UChar
     renames Ada.Characters.Conversions.To_Wide_Wide_Character;
   function "+" (Item : UString) return Wide_Wide_String
     renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;
   --  This needs custom implementations because it consists of several steps!
   function "+" (Item : String) return UString;
   function "+" (Item : UString) return String;
end Unicode_Strings;
