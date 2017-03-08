with String_Helpers; use String_Helpers;
with Ada.Finalization;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with XLib_H, XTest_H;

package body Output_Backend is
   package C renames Interfaces.C;
   use type C.Int;
   use XLib_H, XTest_H;

   type XTest_Data is new Ada.Finalization.Controlled with record
      Display : Display_Access;
   end record;

   procedure Fake_Input_String (Display : Display_Access; Text : Wide_Wide_String);
   procedure Fake_Input_Keysym (Display : Display_Access; Keysym : C.Unsigned_Long);
   function Scratch_Keycode (Display : Display_Access) return C.Int;
   procedure Fake_Press (Display : Display_Access; Key : C.Int);

   procedure Finalize (XTest : in out XTest_Data) is
      I : C.Int;
   begin
      Log.Warning ("[X11.Output] Closing display");
      I := XCloseDisplay (XTest.Display);
   end;

   task body Output is
      XTest : XTest_Data;
      XTest_Opcode : C.Int;
   begin
      Log.Chat ("[X11.Output] Opening display");
      XTest.Display := XOpenDisplay (C.Strings.Null_Ptr);
      Log.Chat ("[X11.Output] Display opened");

      declare
         Ev : C.Int;
         Err : C.Int;
         Ext_Name : C.Strings.Chars_Ptr := C.Strings.New_String ("XTEST");
      begin
         Log.Chat ("[X11.Output] Checking for extension");
         if XQueryExtension (XTest.Display, Ext_Name, XTest_Opcode, Ev, Err) = 0 then
            raise EXTENSION_MISSING with "XTest not available, cannot continue";
         end if;
         Log.Chat ("[X11.Output] Extension found, freeing C string");
         C.Strings.Free (Ext_Name);
         Log.Chat ("[X11.Output] C string free'd");
      end;

      accept Ready_Wait;
      Log.Chat ("[X11.Output] Entering loop");
      loop
         select
            accept Enter (Text : Wide_Wide_String; Continues_Word : Boolean) do
               Log.Chat ("Asked to enter """ & Text & """");
               if Continues_Word then
                  Log.Chat ("First erasing space");
                  Fake_Input_Keysym (XTest.Display, 16#ff08#);
               end if;
               Fake_Input_String (XTest.Display, Text & " ");
            end Enter;
         or
            accept Erase (Amount : Positive) do
               Log.Chat ("[X11.Output] Simulating" & W (Positive'Image (Amount)) & " backspace(s)...");
               for I in 1 .. Amount loop
                  Fake_Input_Keysym (XTest.Display, 16#ff08#);
               end loop;
            end Erase;
         or
            accept Shut_Down;
            Log.Warning ("Shutting down");
            exit;
         end select;
      end loop;
   end Output;


   procedure Fake_Input_String (Display : Display_Access; Text : Wide_Wide_String) is
      function Is_Basic_Latin1 (Letter : Wide_Wide_Character) return Boolean is
      begin
         return Wide_Wide_Character'Pos (Letter) >= 16#20# and Wide_Wide_Character'Pos (Letter) <= 16#fe#;
      end Is_Basic_Latin1;
--      Letter : Wide_Wide_Character;
   begin
      Log.Info ("[X11] Outputting """ & Text & """");
      for Letter of Text loop
--         Letter := Element (Text, I);
         if not Is_Basic_Latin1 (Letter) then
            raise ENCODING_ERROR with "Invalid character in string. Only basic Latin 1 has a simple mapping to Keysyms and that's as much as I'm prepared to deal with right now. Pull requests welcome!";
         end if;
         
         declare
            Keysym : C.Unsigned_Long := Wide_Wide_Character'Pos (Letter);
         begin
            Fake_Input_Keysym (Display, Keysym);
         end;
      end loop;
   end Fake_Input_String;
   
   procedure Fake_Input_Keysym (Display : Display_Access; Keysym : C.Unsigned_Long) is
      KS : C.Unsigned_Long := Keysym;
      Key : C.Int := Scratch_Keycode (Display);
      I : C.Int := 0;
      No_Symbol : C.Unsigned_Long := 0;
   begin
      I := XChangeKeyboardMapping (Display, Key, 1, KS, 1);
      I := XSync (Display, 0);
      Fake_Press (Display, Key);
      I := XSync (Display, 0);
      I := XChangeKeyboardMapping (Display, Key, 1, No_Symbol, 1);
      I := XSync (Display, 0);
   end Fake_Input_Keysym;
   
   function Scratch_Keycode (Display : Display_Access) return C.Int is
      use type C.Unsigned_Long;
      
      No_Symbol : C.Unsigned_Long := 0;
      Min_Keycode : C.Int := 0;
      Max_Keycode : C.Int := 0;
      Unused : C.Int;
   begin
      XDisplayKeycodes (Display, Min_Keycode, Max_Keycode);
      declare
         Key_Count : C.Int := Max_Keycode - Min_Keycode + 1;
         Keysyms_Per_Key : C.Int := 0;
         Keysyms_Ptr : Keysym_Accesses.Pointer := XGetKeyboardMapping (Display, Min_Keycode, Key_Count, Keysyms_Per_key);
         Keysyms : Keysym_Array := Keysym_Accesses.Value (Keysyms_Ptr, C.Ptrdiff_T (Key_Count * Keysyms_Per_Key));
      begin
         for Key in Min_Keycode .. Max_Keycode loop
            declare
               Key_Is_Empty : Boolean := True;
            begin
               for I in 0 .. Keysyms_Per_Key - 1 loop
                  if Keysyms (C.Unsigned ((Key - Min_Keycode) * Keysyms_Per_Key + I)) /= No_Symbol then
                     Key_Is_Empty := False;
                     exit;
                  end if;
               end loop;
               if Key_Is_Empty then
                  Unused := XFree (Keysyms_Ptr.all'Address);
                  return Key;
               end if;
            end;
         end loop;
         Unused := XFree (Keysyms_Ptr.all'Address);
      end;
      raise GENERAL_X11_ERROR with "No empty key available as scratch space. This is technically not a problem but so unusual that I haven't bothered to code for this scenario...";
   end;

   procedure Fake_Press (Display : Display_Access; Key : C.Int) is
      XTestRelease : constant C.Int := 0;
      XTestPress : constant C.Int := 1;
      I : C.Int;
   begin
      I := XTestFakeKeyEvent (Display, Key, XTestPress, 0);
      --  If we don't sync, the key press-and-release events may appear out of
      --  order and/or just generally be mangled
      I := XSync (Display, 0);
      I := XTestFakeKeyEvent (Display, Key, XTestRelease, 0);
      I := XSync (Display, 0);
   end Fake_Press;
end Output_Backend;
