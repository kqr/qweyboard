with Configuration;
with Ada.Real_Time;
private with Unicode_Strings;
private with Qweyboard.Languages;
private with Logging;
private with Output_Backend;

package Qweyboard.Emulation is
   package RT renames Ada.Real_Time;
   use type RT.Time, RT.Time_Span;

   task type Timer_Task;

   protected Softboard is
      procedure Configure (Settings : Configuration.Settings);
      procedure Handle (Event : Key_Event);
      entry Get_Deadline (Time : out RT.Time);
      procedure Timeout;
   private
      Deadline : RT.Time := RT.Time_First;
      Current_Timeout : RT.Time_Span;
      Timer : Timer_Task;
      Pressed : Key_Sets.Set;
      Released : Key_Sets.Set;
      Last_Output : Output;

      procedure Commit;
      procedure Erase;
   end Softboard;
private
   use Unbounded;
   use Logging;

   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set);
end Qweyboard.Emulation;
