with Ada.Command_Line;
with Qweyboard;

package Configuration is
   package CLI renames Ada.Command_Line;
   
   ARGUMENTS_ERROR : exception;
   
   type Settings is record
      Language : Qweyboard.Language_Variant;
      Timeout : Duration;
   end record;
   
   procedure Get_Settings (Config : out Settings);
private
   function Get_Argument (Count : Natural; Language : out Qweyboard.Language_Variant) return Boolean;
   function Get_Argument (Count : Natural; Timeout : out Duration) return Boolean;
end Configuration;
