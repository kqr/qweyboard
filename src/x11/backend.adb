with X11;
with Ada.Containers.Ordered_Maps;

--  X11 specific implementation of the universal backend interface specified
--  by backend.ads.
package body Backend Is
   package Keycode_Mapping is new Ada.Containers.Ordered_Maps (Key_Type => X11.Keycode, Element_Type => Qweyboard.Softkey, "=" => Qweyboard."=");
   From_Keycode : Keycode_Mapping.Map;

   task body Input is
      use type X11.Event_Variant_Type, X11.Key_Event_Variant_Type;
      Event : X11.Event;

      function Is_Escape_Event (Event : X11.Event) return Boolean is
      begin
         Log.Chat ("[Backend] Checking if event is escape event");
         --  TODO find a way to make this customizeable in a cross-platform way
         return Event.Event_Variant = X11.Key_Event and then
           (Event.Key_Variant = X11.Key_Press and Event.Key = 53 and X11.Shift_Pressed (Event.Modifiers) and X11.Control_Pressed (Event.Modifiers));
      end;

      Suspended : Boolean := True;
   begin
      select
         accept Start_Capture do
            Log.Chat ("[Backend] Starting capture, enabling grabs and setting suspended to false");
            X11.Thread.Enable_Grabs;
            Suspended := False;
         end Start_Capture;
      or
         terminate;
      end select;

      Log.Chat ("[Backend] Entering input loop");
      loop
         Log.Chat ("[Backend] Getting next event from X11");
         X11.Thread.Get_Key_Event (Event);
         if Is_Escape_Event (Event) then
            Log.Chat ("[Backend] User toggled suspend status!");
            if Suspended then
               X11.Thread.Enable_Grabs;
            else
               X11.Thread.Disable_Grabs;
            end if;
            Suspended := not Suspended;
         end if;
         Log.Chat ("[Backend] Seeing if we're allowed to deal with this event");
         if not Suspended and Event.Event_Variant = X11.Key_Event then
            if From_Keycode.Contains (Event.Key) then
               Log.Chat ("[Backend] Not suspended, found key event");
               select
                  accept Get_Key_Event (Out_Event : out Qweyboard.Key_Event) do
                     Out_Event := (From_Keycode (Event.Key),
                                   (if Event.Key_Variant = X11.Key_Press
                                      then Qweyboard.Key_Press
                                      else Qweyboard.Key_Release));
                  end Get_Key_Event;
               or
                  terminate;
               end select;
             end if;
         end if;
         Log.Chat ("[Backend] We were not allowed to deal with the event");
      end loop;
   end Input;
   
   procedure Output (Text : String) is
   begin
      X11.Thread.Output (Text);
   end Output;
begin
   Log.Chat ("[Backend] Setting up keycode table");
   From_Keycode.Insert (12, Qweyboard.LP);
   From_Keycode.Insert (13, Qweyboard.LK);
   From_Keycode.Insert (14, Qweyboard.LI);
   From_Keycode.Insert (15, Qweyboard.MAPO);
   From_Keycode.Insert (16, Qweyboard.RO);
   From_Keycode.Insert (17, Qweyboard.RK);
   From_Keycode.Insert (18, Qweyboard.RP);
   From_Keycode.Insert (25, Qweyboard.LF);
   From_Keycode.Insert (26, Qweyboard.LT);
   From_Keycode.Insert (27, Qweyboard.LJ);
   From_Keycode.Insert (28, Qweyboard.LO);
   From_Keycode.Insert (29, Qweyboard.MU);
   From_Keycode.Insert (30, Qweyboard.RI);
   From_Keycode.Insert (31, Qweyboard.RJ);
   From_Keycode.Insert (32, Qweyboard.RT);
   From_Keycode.Insert (33, Qweyboard.RF);
   From_Keycode.Insert (38, Qweyboard.LZ);
   From_Keycode.Insert (39, Qweyboard.LS);
   From_Keycode.Insert (40, Qweyboard.LC);
   From_Keycode.Insert (41, Qweyboard.LR);
   From_Keycode.Insert (42, Qweyboard.LE);
   From_Keycode.Insert (43, Qweyboard.MA);
   From_Keycode.Insert (44, Qweyboard.RE);
   From_Keycode.Insert (45, Qweyboard.RR);
   From_Keycode.Insert (46, Qweyboard.RC);
   From_Keycode.Insert (47, Qweyboard.RS);
   From_Keycode.Insert (48, Qweyboard.RZ);
   From_Keycode.Insert (54, Qweyboard.LL);
   From_Keycode.Insert (55, Qweyboard.LN);
   From_Keycode.Insert (56, Qweyboard.MY);
   From_Keycode.Insert (57, Qweyboard.RN);
   From_Keycode.Insert (58, Qweyboard.RL);
--   From_Keycode.Insert (22, Qweyboard.BS);
   From_Keycode.Insert (50, Qweyboard.MSHI);
   From_Keycode.Insert (62, Qweyboard.MSHI);
   From_Keycode.Insert (65, Qweyboard.NOSP);

   Log.Chat ("[Backend] Preparing keycode array for X11 capture");
   declare
      Keycodes : X11.Keycode_Array (1 .. Integer (From_Keycode.LEngth));
      I : Positive := 1;
   begin
      for C in From_Keycode.Iterate loop
         Keycodes (I) := Keycode_Mapping.Key (C);
         I := I + 1;
      end loop;
      Log.Chat ("[Backend] Asking X11 to capture keys");
      X11.Thread.Capture_Keys (Keycodes);
   end;
end Backend;
