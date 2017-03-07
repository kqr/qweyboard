package body Qweyboard is
   Return_Combo : Key_Sets.Set;
   Backspace_Combo : Key_Sets.Set;

   task body Softboard is
      Current_Timeout : Duration;
      Pressed : Key_Sets.Set;
      Released : Key_Sets.Set;
      Last_Output : Output;
      
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
      end Erase;

      procedure Commit is
         Result : UString;
         use type Key_Sets.Set;
      begin
         Result := Languages.User_Language.Decode (Released);

         if Released = Return_Combo then
            Log.Warning ("[Qweyboard] Return press not implemented");
         elsif Released = Backspace_Combo then
            Log.Chat ("[Qweyboard] Received backspace combination CR-RC; erasing");
            Erase;
         elsif Length (Result) > 0 then
            Last_Output := (Syllable, Result, Released.Contains (NOSP));
            Output_Backend.Output.Enter (Result, Released.Contains (NOSP));
         end if;
         Released.Clear;
      end Commit;
   begin
      Log.Chat ("[Qweyboard] Initialised, waiting for go signal");
      accept Ready_Wait;
      loop
         select
            accept Configure (Settings : Configuration.Settings) do
               Current_Timeout := Settings.Timeout;
               Log.Chat ("[Qweyboard] Setting timeout to" & Duration'Image (Current_Timeout));
            end Configure;
         or
            accept Handle (Event : Key_Event) do
               Log.Chat ("[Qweyboard] Handling key event");
               if Event.Key = SUSP then
                  Last_Output := (Variant => Nothing);
                  Commit;
               else
                  case Event.Key_Event_Variant is
                     when Key_Press =>
                        Pressed.Include (Event.Key);
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
                                 Last_Output := (Syllable, +" ", False);
                                 Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
                                 Special_Used := True;
                              elsif Event.Key = RO then
                                 Last_Output := (Syllable, +".", True);
                                 Output_Backend.Output.Enter (Last_Output.Text, Last_Output.Continues_Word);
                                 Special_Used := True;
                              elsif Event.Key = RJ then
                                 Last_Output := (Syllable, +",", True);
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
begin
   Return_Combo.Insert (LZ);
   Return_Combo.Insert (RZ);
   Backspace_Combo.Insert (LC);
   Backspace_Combo.Insert (LR);
   Backspace_Combo.Insert (RR);
   Backspace_Combo.Insert (RC);
end Qweyboard;


