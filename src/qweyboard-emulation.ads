with String_Helpers; use String_Helpers;
with Configuration;
private with Languages;
private with Logging;
private with Output_Backend;

package Qweyboard.Emulation is
   use Unbounded;

   task Process is
      entry Ready_Wait;
      entry Configure (Settings : Configuration.Settings);
      entry Handle (Event : Key_Event);
      entry Shut_Down;
   end Process;
private
   use Logging;

   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set);
end Qweyboard.Emulation;
