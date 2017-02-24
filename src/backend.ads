with Qweyboard;
with Logging;

--  This is a universal interface for platform-dependent code. The Qweyboard
--  software needs to be able to read key presses and output text to the
--  operating system, and it does so through an Input task and an Output
--  procedure. Any platform-specific implementations should follow this
--  interface. As an example, see x11/backend.adb.
package Backend is
   use Logging;
   task Input is
      entry Start_Capture;
      entry Get_Key_Event (Out_Event : out Qweyboard.Key_Event);
   end Input;
   procedure Output (Text : String);
end Backend;
