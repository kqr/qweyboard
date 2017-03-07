package body Unicode_Strings is
   function "+" (Item : String) return UString is
   begin
      return To_Unbounded_Wide_Wide_String (To_Wide_Wide_String (Item));
   end;
   function "+" (Item : UString) return String is
   begin
      return To_String (To_Wide_Wide_String (Item));
   end;
end Unicode_Strings;
