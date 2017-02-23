package body Qweyboard is
   function Make_Softboard (Language : Language_Variant) return Softboard is
   begin
      -- Local layout modifications for language variants
      case Language is
         when Standard =>
            null;
         when Swedish =>
            Layer (Left).Replace (LI, 'Å');
            Layer (Left).Replace (LO, 'Ä');
            Layer (Left).Replace (LE, 'Ö');
       end case;

      -- Construct a normal layer from the current left, middle and right layers
      Layer (Normal) := Layer (Left);
      Layer (Normal) := Union (Layer (Normal), Layer (Middle));
      Layer (Normal) := Union (Layer (Normal), Layer (Right));

      return (others => Key_Sets.Empty_Set);
   end;

   procedure Handle (Board : in out Softboard; Event : Key_Event) is
   begin
      case Event.Key_Event_Variant is
         when Key_Press =>
            Board.Pressed.Include (Event.Key);
         when Key_Release =>
            Board.Pressed.Exclude (Event.Key);
            Board.Released.Include (Event.Key);
      end case;
   end Handle;

   function Timeout (Board : in out Softboard) return String is
      Final_Pressed : Key_Sets.Set := Key_Sets.Empty_Set;
   begin
      if Board.Pressed.Is_Empty then
         Final_Pressed := Board.Released;
         Board.Released.Clear;
      end if;
      return Apply (Final_Pressed);
   end;
   


   function Union (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map is
      Ret : Key_Maps.Map := A.Copy;
   begin
      for C in B.Iterate loop
         if not Ret.Contains (Key_Maps.Key (C)) then
            Ret.Insert (Key_Maps.Key (C), Key_Maps.Element (C));
         end if;
      end loop;
      return Ret;
   end Union;

   function Intersection (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map is
      Ret : Key_Maps.Map := Key_Maps.Empty_Map;
   begin
      for C in B.Iterate loop
         if A.Contains (Key_Maps.Key (C)) then
            Ret.Insert (Key_Maps.Key (C), Key_Maps.Element (C));
         end if;
      end loop;
      return Ret;
   end Intersection;

   function Difference (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map is
      Ret : Key_Maps.Map := A.Copy;
   begin
      for C in B.Iterate loop
         if Ret.Contains (Key_Maps.Key (C)) then
            Ret.Exclude (Key_Maps.Key (C));
         end if;
      end loop;
      return Ret;
   end Difference;
   
   function Mod_Layer (Final_Presses : Key_Maps.Map; Key : Softkey) return Key_Maps.Map is
   begin
      if Final_Presses.Contains (Key) then
         case Key is
            when LJ => return Difference (Layer (J), Layer (Right));
            when LR => return Difference (Layer (R), Layer (Right));
            when RJ => return Difference (Layer (J), Layer (Left));
            when RR => return Difference (Layer (R), Layer (Left));
            when others => null;
         end case;
      end if;
      return Key_Maps.Empty_Map;
   end Mod_Layer;

   procedure Current_Mod_Map (Final_Presses : in out Key_Maps.Map; Key : Softkey) is
      Modded : Key_Maps.Map := Intersection (Final_Presses, Mod_Layer (Final_Presses, Key));
   begin
      Final_Presses := Union (Modded, Final_Presses);
      if not Modded.Is_Empty then
         Final_Presses.Delete (Key);
      end if;
   end Current_Mod_Map;

   function Virtual_Layer (Pressed : Key_Sets.Set) return Key_Maps.Map is
      Final_Presses : Key_Maps.Map := Key_Maps.Empty_Map;
   begin
      for C in Pressed.Iterate loop
         Final_Presses.Insert (Key_Sets.Element (C), Layer (Normal).Element (Key_Sets.Element (C)));
      end loop;
      for C in Pressed.Iterate loop
         Current_Mod_Map (Final_Presses, Key_Sets.Element (C));
      end loop;
      return Final_Presses;
   end Virtual_Layer;

   function Apply (Pressed : Key_Sets.Set) return String is
      function Only_Letter_Keys (Keys : Key_Sets.Set) return Key_Sets.Set is
         Ret : Key_Sets.Set := Keys.Copy;
      begin
         Ret.Exclude (NOSP);
         return Ret;
      end Only_Letter_Keys;

      Final_Presses : Key_Maps.Map := Virtual_Layer (Only_Letter_Keys (Pressed));
      Ret : String (1 .. Natural (Final_Presses.Length));
      I : Positive := 1;
   begin
      -- As if by accident, the declaration order of Softkey in conjunction with
      -- the implementation of Ordered_(Sets|Maps) means this iteration happens
      -- in precisely the order we need it to...
      for C in Final_Presses.Iterate loop
         Ret (I) := Key_Maps.Element (C);
         I := I + 1;
      end loop;
      if Final_Presses.Is_Empty or Pressed.Contains (NOSP) then
         return Ret;
      else
         return Ret & " ";
      end if;
   end Apply;
begin
   Layer (Left) := Key_Maps.Empty_Map;
   Layer (Left).Insert (LZ, 'Z');
   Layer (Left).Insert (LF, 'F');
   Layer (Left).Insert (LS, 'S');
   Layer (Left).Insert (LP, 'P');
   Layer (Left).Insert (LT, 'T');
   Layer (Left).Insert (LC, 'C');
   Layer (Left).Insert (LK, 'K');
   Layer (Left).Insert (LJ, 'J');
   Layer (Left).Insert (LR, 'R');
   Layer (Left).Insert (LL, 'L');
   Layer (Left).Insert (LI, 'I');
   Layer (Left).Insert (LO, 'O');
   Layer (Left).Insert (LE, 'E');
   Layer (Left).Insert (LN, 'N');

   Layer (Middle) := Key_Maps.Empty_Map;
   Layer (Middle).Insert (MU, 'U');
   Layer (Middle).Insert (MA, 'A');
   Layer (Middle).Insert (MY, 'Y');

   Layer (Right) := Key_Maps.Empty_Map;
   Layer (Right).Insert (RO, 'O');
   Layer (Right).Insert (RI, 'I');
   Layer (Right).Insert (RE, 'E');
   Layer (Right).Insert (RN, 'N');
   Layer (Right).Insert (RK, 'K');
   Layer (Right).Insert (RJ, 'J');
   Layer (Right).Insert (RR, 'R');
   Layer (Right).Insert (RL, 'L');
   Layer (Right).Insert (RP, 'P');
   Layer (Right).Insert (RT, 'T');
   Layer (Right).Insert (RC, 'C');
   Layer (Right).Insert (RF, 'F');
   Layer (Right).Insert (RS, 'S');
   Layer (Right).Insert (RZ, 'Z');
   
   Layer (J) := Key_Maps.Empty_Map;
   Layer (J).Insert (LP, 'B');
   Layer (J).Insert (LT, 'D');
   Layer (J).Insert (LC, 'G');
   Layer (J).Insert (LL, 'H');
   Layer (J).Insert (LN, 'W');
   Layer (J).Insert (RN, 'W');
   Layer (J).Insert (RL, 'H');
   Layer (J).Insert (RP, 'B');
   Layer (J).Insert (RT, 'D');
   Layer (J).Insert (RC, 'G');

   Layer (R) := Key_Maps.Empty_Map;
   Layer (R).Insert (LL, 'V');
   Layer (R).Insert (LN, 'M');
   Layer (R).Insert (RN, 'M');
   Layer (R).Insert (RL, 'V');
end Qweyboard;


--      Ada.Text_IO.Put ("Pressed: [ ");
--      for Key of Board.Pressed loop
--         Ada.Text_IO.Put (Softkey'Image (Key) & " ");
--      end loop;
--      Ada.Text_IO.Put ("] ");
--      Ada.Text_IO.Put ("Released: [ ");
--      for Key of Board.Released loop
--         Ada.Text_IO.Put (Softkey'Image (Key) & " ");
--      end loop;
--      Ada.Text_IO.Put_Line ("]");
