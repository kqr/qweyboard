package body Linux_X11_Backend Is
   package Keycode_To_Key is new Ada.Containers.Ordered_Maps
     (Key_Type => X11.Keycode,
      Element_Type => Qweyboard.Softkey,
      "=" => Qweyboard."=");
   From_Keycode_Map : Keycode_To_Key.Map;

   procedure Output (Text : String) is
   begin
      X11.Thread.Fake_Input_String (Text);
   end;
   
   task body Input_Backend is
      use type X11.Event_Variant_Type, X11.Key_Event_Variant_Type;
      Event : X11.Event;
   begin
      loop
         X11.Thread.Next_Event (Event);
         if Event.Event_Variant = X11.Key_Event then
            select
               when Event.Key_Event_Variant = X11.Key_Press =>
                  accept Next_Key (Out_Event : out Qweyboard.Key_Event) do
                     Out_Event := (From_Keycode (Event.Key), Qweyboard.Key_Press, Event.Time);
                  end Next_Key;
            or
               when Event.Key_Event_Variant = X11.Key_Release =>
                  accept Next_Key (Out_Event : out Qweyboard.Key_Event) do
                     Out_Event := (From_Keycode (Event.Key), Qweyboard.Key_Release, Event.Time);
                  end Next_Key;
            or
               terminate;
            end select;
         end if;
      end loop;
   end Input_Backend;

   function Keycodes return X11.Keycode_Array is
      Codes : X11.Keycode_Array (1 .. Integer (From_Keycode_Map.Length));
      I : Positive := 1;
   begin
      for C in From_Keycode_Map.Iterate loop
         Codes (I) := Keycode_To_Key.Key (C);
         I := I + 1;
      end loop;
      return Codes;
   end;

   function From_Keycode (Key : X11.Keycode) return Qweyboard.Softkey is
   begin
      return From_Keycode_Map (Key);
   end;
begin
   From_Keycode_Map.Insert (12, Qweyboard.LP);
   From_Keycode_Map.Insert (13, Qweyboard.LK);
   From_Keycode_Map.Insert (14, Qweyboard.LI);
   From_Keycode_Map.Insert (16, Qweyboard.RO);
   From_Keycode_Map.Insert (17, Qweyboard.RK);
   From_Keycode_Map.Insert (18, Qweyboard.RP);
   From_Keycode_Map.Insert (25, Qweyboard.LF);
   From_Keycode_Map.Insert (26, Qweyboard.LT);
   From_Keycode_Map.Insert (27, Qweyboard.LJ);
   From_Keycode_Map.Insert (28, Qweyboard.LO);
   From_Keycode_Map.Insert (29, Qweyboard.MU);
   From_Keycode_Map.Insert (30, Qweyboard.RI);
   From_Keycode_Map.Insert (31, Qweyboard.RJ);
   From_Keycode_Map.Insert (32, Qweyboard.RT);
   From_Keycode_Map.Insert (33, Qweyboard.RF);
   From_Keycode_Map.Insert (38, Qweyboard.LZ);
   From_Keycode_Map.Insert (39, Qweyboard.LS);
   From_Keycode_Map.Insert (40, Qweyboard.LC);
   From_Keycode_Map.Insert (41, Qweyboard.LR);
   From_Keycode_Map.Insert (42, Qweyboard.LE);
   From_Keycode_Map.Insert (43, Qweyboard.MA);
   From_Keycode_Map.Insert (44, Qweyboard.RE);
   From_Keycode_Map.Insert (45, Qweyboard.RR);
   From_Keycode_Map.Insert (46, Qweyboard.RC);
   From_Keycode_Map.Insert (47, Qweyboard.RS);
   From_Keycode_Map.Insert (48, Qweyboard.RZ);
   From_Keycode_Map.Insert (54, Qweyboard.LL);
   From_Keycode_Map.Insert (55, Qweyboard.LN);
   From_Keycode_Map.Insert (56, Qweyboard.MY);
   From_Keycode_Map.Insert (57, Qweyboard.RN);
   From_Keycode_Map.Insert (58, Qweyboard.RL);
   From_Keycode_Map.Insert (65, Qweyboard.NOSP);

   X11.Thread.Setup_Keygrabs (Keycodes);
end Linux_X11_Backend;
