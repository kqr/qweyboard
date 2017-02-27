with Ada.Text_IO;

package body Output_Backend is
   task body Output is
   begin
      accept Ready_Wait;
      loop
         select
            accept Enter (Text : Unbounded_String; Completes_Word : Boolean) do
               Ada.Text_IO.Put (To_String (Text));
               Ada.Text_IO.Put (if Completes_Word then " " else "");
            end Enter;
         or
            accept Erase (Amount : Positive) do
               for I in 1 .. Amount loop
                  Ada.Text_IO.Put ("^H");
               end loop;
            end Erase;
         or
            accept Shut_Down;
            exit;
         end select;
      end loop;
   end Output;
end Output_Backend;
