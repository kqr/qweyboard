private with Ada.Containers.Ordered_Maps;
with Qweyboard;

--  This is a universal interface for platform-dependent code. The Qweyboard
--  software needs to be able to read key presses and output text to the
--  operating system, and it does so through an Input task and an Output
--  procedure. Any platform-specific implementations should follow this
--  interface. As an example, see x11/backend.adb.
package Backend is
   task Input is
      entry Get_Key_Event (Out_Event : out Qweyboard.Key_Event);
   end Input;
   procedure Output (Text : String);
end Backend;
