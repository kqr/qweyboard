package body Logging is
   protected body Termination_Handler is
      --  Intentionally not using the logging task here because it
      --  could very well be the logging task that terminates prematurely...
      procedure Diagnostics (C : Cause_Of_Termination; T : Task_Id; X : Exception_Occurrence) is
      begin
         case C is
            when Normal =>
               null;
            when Abnormal =>
               Put_Line (Standard_Error, "Something caused termination in task " & Image (T));
            when Unhandled_Exception =>
               Put_Line (Standard_Error, "Uncaught exception terminated task " & Image (T) & ":");
               Put_Line (Standard_Error, Exception_Information (X));
         end case;
      end Diagnostics;
   end Termination_Handler;

   task body Log is
      Verbosity : Verbosity_Level := Log_Error;
   begin
      loop
         select
            accept Set_Verbosity (User_Verbosity : Verbosity_Level) do
               Verbosity := User_Verbosity;
            end Set_Verbosity;
         or
            accept Chat (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF) do
               if Verbosity >= Log_Chatty then
                  Put (Standard_Error, Text & Suffix);
               end if;
            end Chat;
         or
            accept Info (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF) do
               if Verbosity >= Log_Info then
                  Put (Standard_Error, Text & Suffix);
               end if;
            end Info;
         or
            accept Warning (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF) do
               if Verbosity >= Log_Warning then
                  Put (Standard_Error, Text & Suffix);
               end if;
            end Warning;
         or
            accept Error (Text : String; Suffix : String := 1*Ada.Characters.Latin_1.LF) do
               if Verbosity >= Log_Error then
                  Put (Standard_Error, Text & Suffix);
               end if;
            end Error;
         or
            terminate;
         end select;
      end loop;
   end Log;
end Logging;
