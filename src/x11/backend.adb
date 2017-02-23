with X11;

package body Backend Is
   package Keycode_Mapping is new Ada.Containers.Ordered_Maps (Key_Type => X11.Keycode, Element_Type => Qweyboard.Softkey, "=" => Qweyboard."=");
   From_Keycode : Keycode_Mapping.Map;

   task body Input is
      use type X11.Event_Variant_Type, X11.Key_Event_Variant_Type;
      Event : X11.Event;
   begin
      loop
         X11.Thread.Get_Key_Event (Event);
         if Event.Event_Variant = X11.Key_Event then
            select
               when Event.Key_Event_Variant = X11.Key_Press =>
                  accept Get_Key_Event (Out_Event : out Qweyboard.Key_Event) do
                     Out_Event := (From_Keycode (Event.Key), Qweyboard.Key_Press);
                  end Get_Key_Event;
            or
               when Event.Key_Event_Variant = X11.Key_Release =>
                  accept Get_Key_Event (Out_Event : out Qweyboard.Key_Event) do
                     Out_Event := (From_Keycode (Event.Key), Qweyboard.Key_Release);
                  end Get_Key_Event;
            or
               terminate;
            end select;
         end if;
      end loop;
   end Input;

   procedure Output (Text : String) is
   begin
      X11.Thread.Output (Text);
   end Output;
begin
   From_Keycode.Insert (12, Qweyboard.LP);
   From_Keycode.Insert (13, Qweyboard.LK);
   From_Keycode.Insert (14, Qweyboard.LI);
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
   From_Keycode.Insert (65, Qweyboard.NOSP);

   declare
      Keycodes : X11.Keycode_Array (1 .. Integer (From_Keycode.LEngth));
      I : Positive := 1;
   begin
      for C in From_Keycode.Iterate loop
         Keycodes (I) := Keycode_Mapping.Key (C);
         I := I + 1;
      end loop;
      X11.Thread.Capture_Keys (Keycodes);
   end;
end Backend;
