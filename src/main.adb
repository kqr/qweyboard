with Ada.Command_Line;
with Ada.Task_Termination;
with Ada.Task_Identification;
with Configuration;
with Configuration;
with Logging;
with Qweyboard;
with Input_Backend;
with Output_Backend;


procedure Main is
   use Logging;

   Settings : Configuration.Settings;
begin
   Ada.Task_Termination.Set_Specific_Handler (Ada.Task_Identification.Current_Task, Logging.Termination_Handler.Diagnostics'Access);
   Ada.Task_Termination.Set_Dependents_Fallback_Handler (Logging.Termination_Handler.Diagnostics'Access);

   Configuration.Get_Settings (Settings);
   Log.Set_Verbosity (Settings.Log_Level);
   Log.Chat ("[Main] Got settings and set log verbosity");

   Log.Chat ("[Main] Loading dictionary");
   Configuration.Load_Dictionary (Settings);
   Log.Chat ("[Main] Loading layout");
   Configuration.Load_Layout (Settings);

   --  Then kick off the Softboard!
   Qweyboard.Softboard.Ready_Wait;
   Log.Chat ("[Main] Softboard started");
   -- Configure softboard
   Qweyboard.Softboard.Set_Timeout (Settings.Timeout);
   Qweyboard.Softboard.Set_Layout (Settings.Layout);
   Qweyboard.Softboard.Set_Dictionary (Settings.Dictionary);
   Log.Chat ("[Main] Softboard configured");

   --  First wait for the output backend to be ready
   Output_Backend.Output.Ready_Wait;
   Log.Chat ("[Main] Output backend ready");
   --  Then wait for input backend to be ready
   Input_Backend.Input.Ready_Wait;
   Log.Chat ("[Main] Input backend ready");

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
      Log.Error ("    -t <milliseconds>    : Set the timeout for what counts as one     ");
      Log.Error ("                           stroke. If you want 0, NKRO is strongly    ");
      Log.Error ("                           recommended. Default value is 500.         ");
      Log.Error ("                                                                      ");
      Log.Error ("    -d <dictionary file> : Specifies a file of abbreviations to use.  ");
      Log.Error ("                                                                      ");
      Log.Error ("    -v,-vv,-vvv          : Sets the log level of the software. If you ");
      Log.Error ("                           want to know what goes on inside, this is  ");
      Log.Error ("                           where to poke...                           ");
      Ada.Command_Line.Set_Exit_Status (1);
end Main;
