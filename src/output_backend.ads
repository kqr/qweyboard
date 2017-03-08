with Logging;

package Output_Backend is
   use Logging;

   task Output is
      entry Ready_Wait;
      entry Enter (Text : Wide_Wide_String; Continues_Word : Boolean);
      entry Erase (Amount : Positive);
      entry Shut_Down;
   end Output;
end Output_Backend;
