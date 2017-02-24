with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package Logging is
   function "*" (Left : Natural; Right : Character) return String renames Ada.Strings.Fixed."*";
   type Verbosity_Level is (Log_Info, Log_Error);
   task Log is
      entry Set_Verbosity (User_Verbosity : Verbosity_Level);
      entry Info (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
      entry Error (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
   end Log;
end Logging;
