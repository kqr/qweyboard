package body Qweyboard is
   procedure Add_Key (To_Layout : in out Layout; Modifier : Softkey; Key : Softkey; Letter : Character; Replace : Boolean := False) is
      use Ada.Characters.Handling;
   begin
      if not To_Layout.Layers.Contains (Modifier) then
         if Modifier /= NOKEY then
            To_Layout.Modifiers.Append (Modifier);
         end if;
         To_Layout.Layers.Insert (Modifier, Layer_Maps.Empty_Map);
      end if;
      if Replace then
         Layer_Maps.Replace (To_Layout.Layers (Modifier), Key, To_Lower (Letter));
      else
         To_Layout.Layers (Modifier).Insert (Key, To_Lower (Letter));
      end if;
   end;
   
   task body Softboard is
      Current_Layout : Layout;
      Current_Dictionary : Dictionaries.Dictionary;
      Current_Timeout : Duration;
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

            Ret := Dictionaries.Apply_To (Current_Dictionary, Ret);

            Last_Output := (Syllable, Ret, Released.Contains (NOSP));
            
            case Last_Output.Variant is
               when Syllable =>
                  Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
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
               if Last_Output.Continues_Word then
                  Last_Output := (Erase, Length (Last_Output.Text));
                  Output_Backend.Output.Erase (Last_Output.Amount);
               else
                  Last_Output.Continues_Word := True;
                  Output_Backend.Output.Erase (1);
               end if;
            when others =>
               Last_Output := (Erase, 1);
               Output_Backend.Output.Erase (1);
        end case;
      end;
   begin
      Log.Chat ("[Qweyboard] Initialised, waiting for go signal");
      accept Ready_Wait;
      loop
         select
            accept Set_Timeout (Timeout_Amount : Duration) do
               Current_Timeout := Timeout_Amount;
               Log.Chat ("[Qweyboard] Setting timeout to" & Duration'Image (Current_Timeout));
            end Set_Timeout;
         or
            accept Set_Layout (User_Layout : Layout) do
               Current_Layout := User_Layout;
            end Set_Layout;
         or
            accept Set_Dictionary (User_Dictionary : Dictionaries.Dictionary) do
               Current_Dictionary := User_Dictionary;
            end Set_Dictionary;
         or
            accept Handle (Event : Key_Event) do
               Log.Chat ("[Qweyboard] Handling key event");
               if Event.Key = SUSP then
                  Last_Output := (Variant => Nothing);
                  Commit;
               else
                  case Event.Key_Event_Variant is
                     when Key_Press =>
                        if Event.Key = BS then
                           Erase;
                        else
                           Pressed.Include (Event.Key);
                        end if;
                     when Key_Release =>
                        Pressed.Exclude (Event.Key);
                        --  If only this key was pressed, and no other key is in the chord
                        --  These are some hardcoded special cases – doesn't feel worth it
                        --  to make 'em dynamic at the moment
                        declare
                           Special_Used : Boolean := False;
                        begin
                           if Released.Is_Empty and Pressed.Is_Empty then
                              if Event.Key = NOSP then
                                 Last_Output := (Syllable, To_Unbounded_String (" "), False);
                                 Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
                                 Special_Used := True;
                              elsif Event.Key = RO then
                                 Last_Output := (Syllable, To_Unbounded_String ("."), True);
                                 Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
                                 Special_Used := True;
                              elsif Event.Key = RJ then
                                 Last_Output := (Syllable, To_Unbounded_String (","), True);
                                 Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
                                 Special_Used := True;
                              end if;
                           end if;
                           if not Special_Used then
                              Released.Include (Event.Key);
                           end if;
                        end;
                  end case;
               end if;
               Log_Board (Pressed, Released);
            end Handle;
         or
            accept Shut_Down;
            Log.Info ("[Qweyboard] Received shut down command, committing and shutting down output backend");
            Commit;
            Output_Backend.Output.Shut_Down;
            exit;
         or
            delay Current_Timeout;
            if Pressed.Is_Empty then
               Log.Chat ("[Qweyboard] Timed out and pressed is empty, committing");
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


