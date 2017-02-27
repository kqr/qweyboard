with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging;

package Output_Backend is
   use Logging;

   task Output is
      entry Ready_Wait;
      entry Enter (Text : Unbounded_String; Completes_Word : Boolean);
      entry Erase (Amount : Positive);
      entry Shut_Down;
   end Output;
end Output_Backend;
