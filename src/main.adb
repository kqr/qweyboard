with Ada.Command_Line;
with Ada.Task_Termination;
with Configuration;
with Configuration;
with Logging;
with Qweyboard;
with Input_Backend;


procedure Main is
   use Logging;

   Settings : Configuration.Settings;
begin
   Ada.Task_Termination.Set_Dependents_Fallback_Handler (Logging.Termination_Handler.Diagnostics'Access);

   Configuration.Get_Settings (Settings);
   Log.Set_Verbosity (Settings.Log_Level);
   Log.Chat ("[Main] Got settings and set log verbosity");

   Log.Chat ("[Main] Loading layout");
   Configuration.Load_Layout (Settings);

   Qweyboard.Softboard.Set_Layout (Settings.Layout);
exception
   when Configuration.ARGUMENTS_ERROR =>
      Log.Error ("Usage: " & Ada.Command_Line.Command_Name & " [OPTION]");
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
      Ada.Command_Line.Set_Exit_Status (1);
end Main;
