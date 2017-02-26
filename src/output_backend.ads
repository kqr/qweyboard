with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging;

package Output_Backend is
   use Logging;

   task Output is
      entry Enter (Text : Unbounded_String);
   end Output;
end Output_Backend;
