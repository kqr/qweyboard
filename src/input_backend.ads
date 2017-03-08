with Qweyboard;
with Keys; use Keys;
with Configuration;
with Logging;

package Input_Backend is
   use Logging;

   task Input is
      entry Ready_Wait;
   end Input;
end Input_Backend;
