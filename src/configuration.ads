with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Languages;
with Logging;

package Configuration is
   use Ada.Strings.Unbounded;
   package CLI renames Ada.Command_Line;
   
   ARGUMENTS_ERROR : exception;
   
   type Settings is record
      Timeout : Duration := 0.5;
      Language_File_Name : Unbounded_String;
      Log_Level : Logging.Verbosity_Level := Logging.Log_Error;
      Language : Languages.Language;
   end record;
   
   procedure Get_Settings (Config : in out Settings);
   procedure Load_Language (Config : in out Settings);
private
   function Get_Argument (Count : in out Positive; Flag : String; File_Name : in out Unbounded_String) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Verbosity : in out Logging.Verbosity_Level) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Timeout : in out Duration) return Boolean;
end Configuration;
