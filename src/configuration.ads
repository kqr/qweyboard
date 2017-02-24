with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Qweyboard;

package Configuration is
   use Ada.Strings.Unbounded;
   package CLI renames Ada.Command_Line;
   
   ARGUMENTS_ERROR : exception;
   
   type Settings is record
      Layout_File_Name : Unbounded_String;
      Timeout : Duration;
      Layout : Qweyboard.Layout;
   end record;
   
   procedure Get_Settings (Config : out Settings);
   
   procedure Load_Layout (Config : in out Settings);
private
   function Get_Argument (Count : Natural; Layout_File_Name : out Unbounded_String) return Boolean;
   function Get_Argument (Count : Natural; Timeout : out Duration) return Boolean;
end Configuration;
