with Unicode_Strings; use Unicode_Strings;
with Configuration;
with Keys; use Keys;
private with Languages;
private with Logging;
private with Output_Backend;

package Qweyboard is
   type Key_Event_Variant_Type is (Key_Press, Key_Release);
   type Key_Event is record
      Key : Softkey;
      Key_Event_Variant : Key_Event_Variant_Type;
   end record;
   
   type Output_Variant is (Nothing, Syllable, Erase);
   type Output (Variant : Output_Variant := Nothing) is record
      case Variant is
         when Nothing =>
            null;
         when Syllable =>
            Text : UString;
            Continues_Word : Boolean;
         when Erase =>
            Amount : Positive;
      end case;
   end record;

   task Softboard is
      entry Ready_Wait;
      entry Configure (Settings : Configuration.Settings);
      entry Handle (Event : Key_Event);
      entry Shut_Down;
   end Softboard;
   
private
   use Logging;

   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set);
end Qweyboard;
