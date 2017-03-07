with Unicode_Strings; use Unicode_Strings;
with Logging;

package Output_Backend is
   use Logging;

   task Output is
      entry Ready_Wait;
      entry Enter (Text : UString; Continues_Word : Boolean);
      entry Erase (Amount : Positive);
      entry Shut_Down;
   end Output;
end Output_Backend;
