with Ada.Command_Line;
with Configuration;
with Qweyboard;
with Backend;
with Logging;

procedure Main is
   use Logging;
   package CLI renames Ada.Command_Line;

   Settings : Configuration.Settings;
begin
   --  Make this command-line configurable
   Log.Set_Verbosity (Log_Info);

   Configuration.Get_Settings (Settings);
   Configuration.Load_Layout (Settings);

   declare
      Softboard : Qweyboard.Softboard := Qweyboard.Make_Softboard (Settings.Layout);
      Event : Qweyboard.Key_Event;
   begin
      loop
         select
            Backend.Input.Get_Key_Event (Event);
            Qweyboard.Handle (Softboard, Event);
         or
            delay 0.5;
            Backend.Output (Qweyboard.Timeout (Softboard));
         end select;
      end loop;
   end;
exception
   when Configuration.ARGUMENTS_ERROR =>
      Log.Error ("Usage: " & CLI.Command_Name & " [-l <standard|swedish>] [-t <0-9999>]");
      Log.Error ("");
      Log.Error ("-l sets the language (currently only Standard and Swedish available)");
      Log.Error ("-t sets the timeout delta in milliseconds (i.e. how fast or slow you have to type)");
      Log.Error ("");
      Log.Error ("Running without arguments is equivalent to running " & CLI.Command_Name & " -l standard -t 500");
      CLI.Set_Exit_Status (1);
end Main;
