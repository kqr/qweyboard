with Ada.Text_IO;
with Ada.Command_Line;
with Qweyboard;
with Linux_X11_Backend;

procedure Main is
   use Ada.Text_IO;
   package Backend renames Linux_X11_Backend;
   package CLI renames Ada.Command_Line;

   function Get (Count : Natural; Language : in out Qweyboard.Language_Variant) return Boolean is
   begin
      if CLI.Argument (Count) /= "-l" then
         return False;
      end if;
      if CLI.Argument (Count + 1) = "standard" then
         Language := Qweyboard.Standard;
      elsif CLI.Argument (Count + 1) = "swedish" then
         Language := Qweyboard.Swedish;
      else
         return False;
      end if;
      return True;
   end Get;
   
   function Get (Count : Natural; Timeout : in out Natural) return Boolean is
   begin
      if CLI.Argument (Count) /= "-t" then
         return False;
      end if;
      Timeout := Natural'Value (CLI.Argument (Count + 1));
      return True;
   exception
      when CONSTRAINT_ERROR =>
         return False;
   end Get;

   Language : Qweyboard.Language_Variant := Qweyboard.Standard;
   Timeout : Natural := 500;
begin
   if CLI.Argument_Count = 0 then
      null;
   elsif CLI.Argument_Count = 2 and (Get (1, Language) or Get (1, Timeout)) then
      null;
   elsif CLI.Argument_Count = 4 and Get (1, Language) and Get (3, Timeout) then
      null;
   elsif CLI.Argument_Count = 4 and Get (3, Language) and Get (1, Timeout) then
      null;
   else
      Put_Line ("Usage: " & CLI.Command_Name & " [-l <standard|swedish>] [-t <0-9999>]");
      Put_Line ("");
      Put_Line ("-l sets the language (currently only Standard and Swedish available)");
      Put_Line ("-t sets the timeout delta in milliseconds (i.e. how fast or slow you have to type)");
      Put_Line ("");
      Put_Line ("Running without arguments is equivalent to running " & CLI.Command_Name & " -l standard -t 500");
      CLI.Set_Exit_Status (1);
      return;
   end if;

   declare
      Softboard : Qweyboard.Softboard := Qweyboard.Make_Softboard (Language, Timeout);
      Event : Qweyboard.Key_Event;
   begin
      loop
         select
            Backend.Input_Backend.Next_Key (Event);
            Qweyboard.Handle (Softboard, Event);
         or
            delay 0.5;
            Backend.Output (Qweyboard.Timeout (Softboard));
         end select;
      end loop;
   end;
end Main;
