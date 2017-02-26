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
   
   task body Softboard is
      Initialised : Boolean := False;
      Has_Event : Boolean := False;
      Current_Layout : Layout;
      Pressed : Key_Sets.Set;
      Released : Key_Sets.Set;
   begin
      loop
         select
            accept Set_Layout (User_Layout : Layout) do
               Current_Layout := User_Layout;
               Initialised := True;
            end Set_Layout;
         or
            accept Handle (Event : Key_Event) do
               Has_Event := True;
               case Event.Key_Event_Variant is
                  when Key_Press =>
                     Pressed.Include (Event.Key);
                  when Key_Release =>
                     Pressed.Exclude (Event.Key);
                     Released.Include (Event.Key);
               end case;
               --Log_Board;
            end Handle;
         or
            delay 0.5;
            declare
               Result : Unbounded_String;
            begin
               Result := Apply (Current_Layout, Released);
               Output_Backend.Output.Enter (Result);
               Released.Clear;
            end;
         end select;
      end loop;
   end Softboard;
   


---   procedure Log_Board is
---   begin
---      Log.Info ("[Qweyboard] Pressed: [", Suffix => " ");
---      for Key of Pressed loop
---         Log.Info (Softkey'Image (Key), Suffix => " ");
---      end loop;
---      Log.Info ("]", Suffix => " ");
---      Log.Info ("Released: [", Suffix => " ");
---      for Key of Released loop
---         Log.Info (Softkey'Image (Key), Suffix => " ");
---      end loop;
---      Log.Info ("]");
---   end Log_Board;


   function Apply (User_Layout : Layout; Pressed : Key_Sets.Set) return Unbounded_String is
      Final_Presses : Layer_Maps.Map := Virtual_Layer (User_Layout, Pressed);
      Ret : Unbounded_String;
   begin
      -- As if by accident, the declaration order of Softkey in conjunction with
      -- the implementation of Ordered_(Sets|Maps) means this iteration happens
      -- in precisely the order we need it to...
      for C in Final_Presses.Iterate loop
         Append (Ret, Layer_Maps.Element (C));
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
end Qweyboard;


