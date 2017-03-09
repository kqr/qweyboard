private with Qweyboard;
private with Qweyboard.Emulation;
private with Configuration;
private with Logging;

package Input_Backend is
   task Input is
      entry Ready_Wait;
   end Input;
private
   use Logging;
   use Qweyboard;
end Input_Backend;
