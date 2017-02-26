with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging;

-- TODO: Universify this by putting all the C/X11 stuff in the body!
package Output_Backend is
   use Logging;

   task Output is
      entry Enter (Text : Unbounded_String);
   end Output;
private
end Output_Backend;
