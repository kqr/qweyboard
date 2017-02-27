with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Task_Termination;
with Ada.Task_Identification;
with Ada.Exceptions;

package Logging is
   use Ada.Text_IO;
   use Ada.Task_Termination;
   use Ada.Task_Identification;
   use Ada.Exceptions;

   function "*" (Left : Natural; Right : Character) return String renames Ada.Strings.Fixed."*";
   type Verbosity_Level is (Log_Error, Log_Warning, Log_Info, Log_Chatty);
   task Log is
      entry Set_Verbosity (User_Verbosity : Verbosity_Level);
      entry Chat (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
      entry Info (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
      entry Warning (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
      entry Error (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF);
   end Log;

   protected Termination_Handler is
      procedure Diagnostics (C : Cause_Of_Termination; T : Task_Id; X : Exception_Occurrence);
   end Termination_Handler;
end Logging;
