with Ada.Wide_Wide_Text_IO;

package body Output_Backend is
   package IO renames Ada.Wide_Wide_Text_IO;

   task body Output is
   begin
      accept Ready_Wait;
      loop
         select
            accept Enter (Text : Wide_Wide_String; Continues_Word : Boolean) do
               IO.Put (if Continues_Word then "" else " ");
               IO.Put (Text);
            end Enter;
         or
            accept Erase (Amount : Positive) do
               for I in 1 .. Amount loop
                  IO.Put ("^H");
               end loop;
            end Erase;
         or
            accept Shut_Down;
            exit;
         end select;
      end loop;
   end Output;
end Output_Backend;
