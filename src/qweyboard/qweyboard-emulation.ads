with Configuration;
private with Unicode_Strings;
private with Qweyboard.Languages;
private with Logging;
private with Output_Backend;

package Qweyboard.Emulation is
   task Process is
      entry Ready_Wait;
      entry Configure (Settings : Configuration.Settings);
      entry Handle (Event : Key_Event);
      entry Shut_Down;
   end Process;
private
   use Unbounded;
   use Logging;

   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set);
end Qweyboard.Emulation;
