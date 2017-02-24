with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Qweyboard;
with Logging;

package Configuration is
   use Ada.Strings.Unbounded;
   package CLI renames Ada.Command_Line;
   
   ARGUMENTS_ERROR : exception;
   
   type Settings is record
      Timeout : Duration;
      Layout_File_Name : Unbounded_String;  -- Currently unused
      Dictionary_File_Name : Unbounded_String;  -- Currently unused
      Log_Level : Logging.Verbosity_Level;
      Layout : Qweyboard.Layout;
   end record;
   
   procedure Get_Settings (Config : out Settings);
   procedure Load_Layout (Config : in out Settings);
private
   function Get_Argument (Count : in out Positive; Flag : String; File_Name : out Unbounded_String) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Verbosity : out Logging.Verbosity_Level) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Timeout : out Duration) return Boolean;
end Configuration;
