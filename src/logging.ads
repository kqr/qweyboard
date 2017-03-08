with String_Helpers; use String_Helpers;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;

package Logging is
   type Verbosity_Level is
     (Log_Error, Log_Warning, Log_Info, Log_Chatty);
   
   task Log is
      entry Set_Verbosity
        (User_Verbosity : Verbosity_Level);
      entry Chat
        (Text : Wide_Wide_String;
         Suffix : Wide_Wide_Character := Characters.LF);
      entry Info
        (Text : Wide_Wide_String;
         Suffix : Wide_Wide_Character := Characters.LF);
      entry Warning
        (Text : Wide_Wide_String;
         Suffix : Wide_Wide_Character := Characters.LF);
      entry Error
        (Text : Wide_Wide_String;
         Suffix : Wide_Wide_Character := Characters.LF);
   end Log;

   protected Termination_Handler is
      procedure Diagnostics
        (C : Cause_Of_Termination;
         T : Task_Id;
         X : Exception_Occurrence);
   end Termination_Handler;
end Logging;
