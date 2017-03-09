package body Input_Backend is
   type Key_Array is array (Positive range <>) of Softkey;

   procedure Chord (Keys : Key_Array) is
   begin
      for Key of Keys loop
         Qweyboard.Softboard.Handle ((Key, Qweyboard.Key_Press));
      end loop;
      for Key of reverse Keys loop
         Qweyboard.Softboard.Handle ((Key, Qweyboard.Key_Release));
      end loop;
   end;
   
   function Single (Key : Softkey) return Key_Array is
      A : Key_Array (1..1) := (others => Key);
   begin
      return A;
   end;

   task body Input is
   begin
      accept Ready_Wait;
      --  This is not something input backends should generally do, but we
      --  do it here to ensure the "test" works, regardless of user settings.

      Qweyboard.Softboard.Configure ((0.2, others => <>));

      --  Begin pressing keys!
      Chord (Single (RI));
      delay 0.3;
      Chord ((LJ, LN, MA, RN, RT));
      delay 0.3;
      Chord ((LT, LO));
      delay 0.3;
      Chord ((LT, MY, RP, RF));
      delay 0.3;
      Chord ((LO, RN));
      delay 0.3;
      Chord (Single (MA));
      delay 0.3;
      Chord ((LC, LF));
      delay 0.3;
      Chord ((LJ, LN, LE, MY, NOSP));
      delay 0.3;
      Chord ((LJ, LP, LO, MA, RR, RJ, RT, NOSP));
      delay 0.3;
      Chord ((LC, RO, RN));
      delay 0.3;
      Chord ((LS, LT, MA, RN, RT, NOSP));
      delay 0.3;
      Chord ((LL, MY, NOSP));

      Qweyboard.Softboard.Shut_Down;
   end Input;
end Input_Backend;
