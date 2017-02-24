with Ada.Command_Line;
with Configuration;
with Qweyboard;
with Backend;
with Logging;

procedure Main is
   use Logging;
   package CLI renames Ada.Command_Line;

   Settings : Configuration.Settings;
   Softboard : Qweyboard.Softboard;
   Event : Qweyboard.Key_Event;
begin
   Configuration.Get_Settings (Settings);
   Log.Set_Verbosity (Settings.Log_Level);
   Log.Chat ("[Main] Got settings and set log verbosity");

   Log.Chat ("[Main] Loading layout");
   Configuration.Load_Layout (Settings);


   Log.Chat ("[Main] Making softboard");
   Softboard := Qweyboard.Make_Softboard (Settings.Layout);
   Log.Chat ("[Main] Starting capture on backend");
   Backend.Input.Start_Capture;
   Log.Chat ("[Main] Entering main loop");
   loop
      select
         Backend.Input.Get_Key_Event (Event);
         Log.Chat ("[Main] Got a key event from input backend");
         Qweyboard.Handle (Softboard, Event);
      or
         delay 0.5;
         Backend.Output (Qweyboard.Timeout (Softboard));
      end select;
   end loop;
exception
   when Configuration.ARGUMENTS_ERROR =>
      Log.Error ("Usage: " & CLI.Command_Name & " [OPTION]");
      Log.Error ("                                                                      ");
      Log.Error ("[OPTION] is any combination of the following options:                 ");
      Log.Error ("                                                                      ");
      Log.Error ("    -l <layout file>     : Modifies the standard layout with the      ");
      Log.Error ("                           key mappings indicated in the specified    ");
      Log.Error ("                           layout file.  [CURRENTLY UNUSED]           ");
      Log.Error ("                                                                      ");
      Log.Error ("    -t <seconds>         : Set the timeout for what counts as one     ");
      Log.Error ("                           stroke. If you want 0, NKRO is strongly    ");
      Log.Error ("                           recommended. Default value is 0.5.         ");
      Log.Error ("                                                                      ");
      Log.Error ("    -d <dictionary file> : Specifies a file of abbreviations to use.  ");
      Log.Error ("                           [CURRENTLY UNUSED]                         ");
      Log.Error ("                                                                      ");
      Log.Error ("    -v,-vv               : Sets the log level of the software. If you ");
      Log.Error ("                           want to know what goes on inside, this is  ");
      Log.Error ("                           where to poke...                           ");
      CLI.Set_Exit_Status (1);
end Main;
