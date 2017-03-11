--  Using non-wide strings for file names only
with Ada.Strings.Unbounded;
with Ada.Real_Time;
with Logging;
private with Ada.Command_Line;
private with Qweyboard.Languages.Parser;

package Configuration is
   use Ada.Strings.Unbounded;
   
   ARGUMENTS_ERROR : exception;
   
   type Settings is record
      Timeout : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (500);
      Language_File_Name : Unbounded_String;
      Log_Level : Logging.Verbosity_Level := Logging.Log_Error;
   end record;
   
   procedure Get_Settings (Config : in out Settings);
   procedure Load_Language (Config : in out Settings);
private
   package CLI renames Ada.Command_Line;

   function Get_Argument (Count : in out Positive; Flag : String; File_Name : in out Unbounded_String) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Verbosity : in out Logging.Verbosity_Level) return Boolean;
   function Get_Argument (Count : in out Positive; Flag : String; Timeout : in out Ada.Real_Time.Time_Span) return Boolean;
end Configuration;
