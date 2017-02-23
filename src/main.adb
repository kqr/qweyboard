with Ada.Text_IO;
with Ada.Command_Line;
with Configuration;
with Qweyboard;
with Backend;

procedure Main is
   use Ada.Text_IO;
   package CLI renames Ada.Command_Line;

   Settings : Configuration.Settings;
begin
   Configuration.Get_Settings (Settings);

   declare
      Softboard : Qweyboard.Softboard := Qweyboard.Make_Softboard (Settings.Language);
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
      Put_Line ("Usage: " & CLI.Command_Name & " [-l <standard|swedish>] [-t <0-9999>]");
      Put_Line ("");
      Put_Line ("-l sets the language (currently only Standard and Swedish available)");
      Put_Line ("-t sets the timeout delta in milliseconds (i.e. how fast or slow you have to type)");
      Put_Line ("");
      Put_Line ("Running without arguments is equivalent to running " & CLI.Command_Name & " -l standard -t 500");
      CLI.Set_Exit_Status (1);
end Main;
