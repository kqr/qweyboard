private with Ada.Containers.Ordered_Maps;
with Qweyboard;

package Backend is
   task Input is
      entry Get_Key_Event (Out_Event : out Qweyboard.Key_Event);
   end Input;
   procedure Output (Text : String);
end Backend;
