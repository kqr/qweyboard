with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Wide_Wide_Text_IO;

--  Package to make it easier to deal with strings in general, but wide,
--  international strings in particular.
package String_Helpers is
   package Unbounded
     renames Ada.Strings.Wide_Wide_Unbounded;
   package Conversions
     renames Ada.Characters.Conversions;
   package IO
     renames Ada.Wide_Wide_Text_IO;
   package Character_Maps
     renames Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;

   function W (Item : String)
     return Wide_Wide_String
     renames Conversions.To_Wide_Wide_String;
   
   function From_Unbounded (Item : Unbounded.Unbounded_Wide_Wide_String)
     return Wide_Wide_String
     renames Unbounded.To_Wide_Wide_String;
   
   function To_Unbounded (Item : Wide_Wide_String)
     return Unbounded.Unbounded_Wide_Wide_String
     renames Unbounded.To_Unbounded_Wide_Wide_String;
   
--   function To_Lower (Item : Unbounded.Unbounded_Wide_Wide_String)
--     return Unbounded.Unbounded_Wide_Wide_String is
--     (Unbounded.Translate (Item, Character_Maps.Lower_Case_Map));
   
   package Characters is
      LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0a#); 
   end Characters;
end String_Helpers;
