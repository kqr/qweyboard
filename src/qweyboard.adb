package body Qweyboard is
   procedure Add_Key (To_Layout : in out Layout; Modifier : Softkey; Key : Softkey; Letter : Character; Replace : Boolean := False) is
   begin
      if not To_Layout.Layers.Contains (Modifier) then
         if Modifier /= NOKEY then
            To_Layout.Modifiers.Append (Modifier);
         end if;
         To_Layout.Layers.Insert (Modifier, Layer_Maps.Empty_Map);
      end if;
      if Replace then
         Layer_Maps.Replace (To_Layout.Layers (Modifier), Key, Letter);
      else
         To_Layout.Layers (Modifier).Insert (Key, Letter);
      end if;
   end;

   function Make_Softboard (User_Layout : Layout) return Softboard is
   begin
      return (User_Layout => User_Layout, others => Key_Sets.Empty_Set);
   end;

   procedure Handle (Board : in out Softboard; Event : Key_Event) is
      procedure Log_Board is
      begin
         Log.Info ("[Qweyboard] Pressed: [", Suffix => " ");
         for Key of Board.Pressed loop
            Log.Info (Softkey'Image (Key), Suffix => " ");
         end loop;
         Log.Info ("]", Suffix => " ");
         Log.Info ("Released: [", Suffix => " ");
         for Key of Board.Released loop
            Log.Info (Softkey'Image (Key), Suffix => " ");
         end loop;
         Log.Info ("]");
      end Log_Board;
   begin
      case Event.Key_Event_Variant is
         when Key_Press =>
            Board.Pressed.Include (Event.Key);
         when Key_Release =>
            Board.Pressed.Exclude (Event.Key);
            Board.Released.Include (Event.Key);
      end case;
      Log_Board;
   end Handle;

   function Timeout (Board : in out Softboard) return String is
      Final_Pressed : Key_Sets.Set := Key_Sets.Empty_Set;
   begin
      if Board.Pressed.Is_Empty then
         Final_Pressed := Board.Released;
         Board.Released.Clear;
      end if;
      return Apply (Board.User_Layout, Final_Pressed);
   end;
   

   function Apply (User_Layout : Layout; Pressed : Key_Sets.Set) return String is
      Final_Presses : Layer_Maps.Map := Virtual_Layer (User_Layout, Pressed);
      Ret : String (1 .. Natural (Final_Presses.Length));
      I : Positive := 1;
   begin
      -- As if by accident, the declaration order of Softkey in conjunction with
      -- the implementation of Ordered_(Sets|Maps) means this iteration happens
      -- in precisely the order we need it to...
      for C in Final_Presses.Iterate loop
         Ret (I) := Layer_Maps.Element (C);
         I := I + 1;
      end loop;
      if Final_Presses.Is_Empty or Pressed.Contains (NOSP) then
         return Ret;
      else
         return Ret & " ";
      end if;
   end Apply;

   function Virtual_Layer (User_Layout : Layout; Pressed : Key_Sets.Set) return Layer_Maps.Map is
      Final_Presses : Layer_Maps.Map;
      Already_Handled : Key_Sets.Set;
      
      procedure Handle_Modifier (Modifier : Softkey) is
         Layer : Layer_Maps.Map := Mod_Layer (User_Layout, Modifier, Key_Sets.Difference (Pressed, Already_Handled));
      begin
         if not Layer.Is_Empty then
            Already_Handled.Insert (Modifier);
         end if;
         for C in Layer.Iterate loop
            Already_Handled.Insert (Layer_Maps.Key (C));
            Final_Presses.Insert (Layer_Maps.Key (C), Layer_Maps.Element (C));
         end loop;
      end;
   begin
      for Modifier of User_Layout.Modifiers loop
         Handle_Modifier (Modifier);
      end loop;
      Handle_Modifier (NOKEY);
      return Final_Presses;
   end Virtual_Layer;

   function Mod_Layer (User_Layout : Layout; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map is
      Layer : Layer_Maps.Map;
   begin
      if Modifier = NOKEY or Pressed.Contains (Modifier) then
         for Key of Pressed loop
            if User_Layout.Layers (Modifier).Contains (Key) then
               Layer.Insert (Key, User_Layout.Layers (Modifier) (Key));
            end if;
         end loop;
      end if;
      return Layer;
   end Mod_Layer;





   function Union (A : Layer_Maps.Map; B : Layer_Maps.Map) return Layer_Maps.Map is
      Ret : Layer_Maps.Map := A.Copy;
   begin
      for C in B.Iterate loop
         if not Ret.Contains (Layer_Maps.Key (C)) then
            Ret.Insert (Layer_Maps.Key (C), Layer_Maps.Element (C));
         end if;
      end loop;
      return Ret;
   end Union;

   function Intersection (A : Layer_Maps.Map; B : Layer_Maps.Map) return Layer_Maps.Map is
      Ret : Layer_Maps.Map := Layer_Maps.Empty_Map;
   begin
      for C in B.Iterate loop
         if A.Contains (Layer_Maps.Key (C)) then
            Ret.Insert (Layer_Maps.Key (C), Layer_Maps.Element (C));
         end if;
      end loop;
      return Ret;
   end Intersection;

   function Difference (A : Layer_Maps.Map; B : Layer_Maps.Map) return Layer_Maps.Map is
      Ret : Layer_Maps.Map := A.Copy;
   begin
      for C in B.Iterate loop
         if Ret.Contains (Layer_Maps.Key (C)) then
            Ret.Exclude (Layer_Maps.Key (C));
         end if;
      end loop;
      return Ret;
   end Difference;
end Qweyboard;


