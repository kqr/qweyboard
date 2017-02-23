private with Ada.Containers.Ordered_Maps;
with X11;
with Qweyboard;
with Ada.Text_IO;

package Linux_X11_Backend Is
   task Input is
      entry Get_Key_Event (Out_Event : out Qweyboard.Key_Event);
   end Input;
   procedure Output (Text : String);
end Linux_X11_Backend;
