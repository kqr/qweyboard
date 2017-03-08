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
               IO.Put_Line (IO.Current_Error, W ("Something caused termination in task " & Image (T)));
            when Unhandled_Exception =>
               IO.Put_Line (IO.Current_Error, W ("Uncaught exception terminated task " & Image (T) & ":"));
               IO.Put_Line (IO.Current_Error, W (Exception_Information (X)));
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
            accept Chat (Text : Wide_Wide_String; Suffix : Wide_Wide_Character := Characters.LF) do
               if Verbosity >= Log_Chatty then
                  IO.Put (IO.Current_Error, Text & Suffix);
               end if;
            end Chat;
         or
            accept Info (Text : Wide_Wide_String; Suffix : Wide_Wide_Character := Characters.LF) do
               if Verbosity >= Log_Info then
                  IO.Put (IO.Current_Error, Text & Suffix);
               end if;
            end Info;
         or
            accept Warning (Text : Wide_Wide_String; Suffix : Wide_Wide_Character := Characters.LF) do
               if Verbosity >= Log_Warning then
                  IO.Put (IO.Current_Error, Text & Suffix);
               end if;
            end Warning;
         or
            accept Error (Text : Wide_Wide_String; Suffix : Wide_Wide_Character := Characters.LF) do
               if Verbosity >= Log_Error then
                  IO.Put (IO.Current_Error, Text & Suffix);
               end if;
            end Error;
         or
            terminate;
         end select;
      end loop;
   end Log;
end Logging;
