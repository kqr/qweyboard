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
      Current_Layout : Layout;
      Pressed : Key_Sets.Set;
      Released : Key_Sets.Set;
      Last_Output : Output;
      
      procedure Commit is
         Final_Presses : Layer_Maps.Map;
         Ret : Unbounded_String;
      begin
         Final_Presses := Virtual_Layer (Current_Layout, Released);
         if not Final_Presses.Is_Empty then
            -- As if by accident, the declaration order of Softkey in conjunction with
            -- the implementation of Ordered_(Sets|Maps) means this iteration happens
            -- in precisely the order we need it to...
            for C in Final_Presses.Iterate loop
               Append (Ret, Layer_Maps.Element (C));
            end loop;
            Last_Output := (Syllable, Ret, not Released.Contains (NOSP));
            
            case Last_Output.Variant is
               when Syllable =>
                  Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Completes_Word);
               when Erase =>
                  Output_Backend.Output.Erase (Last_Output.Amount);
               when Nothing =>
                  null;
            end case;
         end if;
         Released.Clear;
      end Commit;
      
      procedure Erase is
      begin
         Pressed.Clear;
         Released.Clear;
         case Last_Output.Variant is
            when Syllable =>
               if Last_Output.Completes_Word then
                  Last_Output.Completes_Word := False;
                  Output_Backend.Output.Erase (1);
               else
                  Last_Output := (Erase, Length (Last_Output.Text));
                  Output_Backend.Output.Erase (Last_Output.Amount);
               end if;
            when others =>
               Last_Output := (Erase, 1);
               Output_Backend.Output.Erase (1);
        end case;
      end;
   begin
      loop
         select
            accept Set_Layout (User_Layout : Layout) do
               Current_Layout := User_Layout;
               Initialised := True;
            end Set_Layout;
         or
            accept Handle (Event : Key_Event) do
               if Event.Key = SUSP then
                  Last_Output := (Variant => Nothing);
                  Commit;
               else
                  case Event.Key_Event_Variant is
                     when Key_Press =>
                        if Event.Key = BS then
                           Erase;
                        elsif Event.Key = NOSP and Released.Is_Empty and Pressed.Is_Empty then
                           Output_Backend.Output.Enter (To_Unbounded_String (" "), False);
                        else
                           Pressed.Include (Event.Key);
                        end if;
                     when Key_Release =>
                        Pressed.Exclude (Event.Key);
                        Released.Include (Event.Key);
                  end case;
               end if;
               Log_Board (Pressed, Released);
            end Handle;
         or
            accept Shut_Down;
            Output_Backend.Output.Shut_Down;
            exit;
         or
            delay 0.5;
            if Pressed.Is_Empty then
               Commit;
            end if;
         end select;
      end loop;
   end Softboard;
   
   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set) is
   begin
      Log.Info ("[Qweyboard] Pressed: [", Suffix => " ");
      for Key of Pressed loop
         Log.Info (Softkey'Image (Key), Suffix => " ");
      end loop;
      Log.Info ("]", Suffix => " ");
      Log.Info ("Released: [", Suffix => " ");
      for Key of Released loop
         Log.Info (Softkey'Image (Key), Suffix => " ");
      end loop;
      Log.Info ("]");
   end Log_Board;


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


