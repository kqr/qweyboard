private with Ada.Containers.Ordered_Maps;
with X11;
with Qweyboard;
with Ada.Text_IO;

package Linux_X11_Backend Is
   task Input_Backend is
      entry Next_Key (Out_Event : out Qweyboard.Key_Event);
   end Input_Backend;
   procedure Output (Text : String);
private
   function From_Keycode (Key : X11.Keycode) return Qweyboard.Softkey;
   function Keycodes return X11.Keycode_Array;
end Linux_X11_Backend;
