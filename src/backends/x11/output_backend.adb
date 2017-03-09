with String_Helpers; use String_Helpers;
with Ada.Finalization;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with XLib_H, XTest_H;

package body Output_Backend is
   package C renames Interfaces.C;
   use type C.Int, C.Unsigned_Long;
   use XLib_H, XTest_H;
   
   --  When we map a key, we map it with (Keysym, Qweyboard_Marker) to indicate
   --  for ourselves that we have used this key. This is useful in case of 
   --  program crashes or whatever, because we don't need to leak keys.
   Qweyboard_Marker : C.Unsigned_Long := 16#01fffffe#;

   package Keysym_To_Keycode is new Ada.Containers.Ordered_Maps (Key_Type => C.Unsigned_Long, Element_Type => C.Int);
   package Keycode_To_Keysym is new Ada.Containers.Ordered_Maps (Key_Type => C.Int, Element_Type => C.Unsigned_Long);
   package Keycode_Sets is new Ada.Containers.Ordered_Sets (Element_Type => C.Int);
   type Key_Mapping is record
      Keysyms : Keysym_To_Keycode.Map;
      Keycodes : Keycode_To_Keysym.Map;
      Available : Keycode_Sets.Set;
      Recently_Used : Keycode_Sets.Set;
   end record;
   type XTest_Data is new Ada.Finalization.Controlled with record
      Display : Display_Access;
      Key_Map : Key_Mapping;
   end record;

   procedure Fake_Input_String (Display : Display_Access; Key_Map : in out Key_Mapping; Text : Wide_Wide_String);
   procedure Fake_Input_Keysym (Display : Display_Access; Key_Map : in out Key_Mapping; Keysym : C.Unsigned_Long);
   function Get_Keycode (Display : Display_Access; Key_Map : in out Key_Mapping; Keysym : C.Unsigned_Long) return C.Int;
   procedure Update_Mapping (Display : Display_Access; Key_Map : in out Key_Mapping);
   procedure Fake_Press (Display : Display_Access; Key : C.Int);

   procedure Finalize (XTest : in out XTest_Data) is
      I : C.Int;
      All_Keycodes : Keycode_Sets.Set := Keycode_Sets.Union(XTest.Key_Map.Available, XTest.Key_Map.Recently_Used);
      No_Symbol : C.Unsigned_Long := 0;
      Empty_Key : aliased Keysym_Array (0..1) := (others => No_Symbol);
   begin
      for Keycode of All_Keycodes loop
         I := XChangeKeyboardMapping (XTest.Display, Keycode, 2, Empty_Key, 1);
         I := XSync (XTest.Display, 0);
      end loop;
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
      
      Update_Mapping (XTest.Display, XTest.Key_Map);

      accept Ready_Wait;
      Log.Chat ("[X11.Output] Entering loop");
      loop
         select
            accept Enter (Text : Wide_Wide_String; Continues_Word : Boolean) do
               Log.Chat ("Asked to enter """ & Text & """");
               if Continues_Word then
                  Log.Chat ("First erasing space");
                  Fake_Input_Keysym (XTest.Display, XTest.Key_Map, 16#ff08#);
               end if;
               Fake_Input_String (XTest.Display, XTest.Key_Map, Text & " ");
            end Enter;
         or
            accept Erase (Amount : Positive) do
               Log.Chat ("[X11.Output] Simulating" & W (Positive'Image (Amount)) & " backspace(s)...");
               for I in 1 .. Amount loop
                  Fake_Input_Keysym (XTest.Display, XTest.Key_Map, 16#ff08#);
               end loop;
            end Erase;
         or
            accept Shut_Down;
            Log.Warning ("Shutting down");
            exit;
         end select;
      end loop;
   end Output;


   procedure Fake_Input_String (Display : Display_Access; Key_Map : in out Key_Mapping; Text : Wide_Wide_String) is
      function Is_Basic_Latin1 (Letter : Wide_Wide_Character) return Boolean is
      begin
         return Wide_Wide_Character'Pos (Letter) >= 16#20# and Wide_Wide_Character'Pos (Letter) <= 16#fe#;
      end Is_Basic_Latin1;
   begin
      Log.Info ("[X11] Outputting """ & Text & """");
      for Letter of Text loop
         if not Is_Basic_Latin1 (Letter) then
            raise ENCODING_ERROR with "Invalid character in string. Only basic Latin 1 has a simple mapping to Keysyms and that's as much as I'm prepared to deal with right now. Pull requests welcome!";
         end if;
         
         declare
            Keysym : C.Unsigned_Long := Wide_Wide_Character'Pos (Letter);
         begin
            Fake_Input_Keysym (Display, Key_Map, Keysym);
         end;
      end loop;
   end Fake_Input_String;
   

   procedure Fake_Input_Keysym (Display : Display_Access; Key_Map : in out Key_Mapping; Keysym : C.Unsigned_Long) is
      Key : C.Int := Get_Keycode (Display, Key_Map, Keysym);
      I : C.Int := 0;
   begin
      Fake_Press (Display, Key);
   end Fake_Input_Keysym;
   

   function Get_Keycode (Display : Display_Access; Key_Map : in out Key_Mapping; Keysym : C.Unsigned_Long) return C.Int is
      I : C.Int;
   begin
      if Key_Map.Keysyms.Contains (Keysym) then
         return Key_Map.Keysyms.Element (Keysym);
      end if;
      
      if Key_Map.Available.Is_Empty then
         Key_Map.Available := Key_Map.Recently_Used.Copy;
         Key_Map.Recently_Used.Clear;
      end if;

      declare
         Key : C.Int := Key_Map.Available.First_Element;
         Marked_Keysym : Keysym_Array (0..1) := (Keysym, Qweyboard_Marker);
      begin
         if Key_Map.Keycodes.Contains (Key) then
            Key_Map.Keysyms.Delete (Key_Map.Keycodes.Element (Key));
            Key_Map.Keycodes.Delete (Key);
         end if;

         Key_Map.Keycodes.Insert (Key, Keysym);
         Key_Map.Keysyms.Insert (Keysym, Key);

         I := XChangeKeyboardMapping (Display, Key, 2, Marked_Keysym, 1);
         I := XSync (Display, 0);
         
         Key_Map.Available.Delete (Key);
         Key_Map.Recently_Used.Insert (Key);
         return Key;
      end;
   end Get_Keycode;


   procedure Update_Mapping (Display : Display_Access; Key_Map : in out Key_Mapping) is
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
               use type C.Unsigned;
               Low : C.Unsigned := C.Unsigned ((Key - Min_Keycode) * Keysyms_Per_Key);
               High : C.Unsigned := (Low + C.Unsigned (Keysyms_Per_Key) - 1);
               Key_Is_Empty : Boolean :=
                 Keysyms (Low+1) = Qweyboard_Marker or
                 (for all KS of Keysyms (Low .. High) => KS = No_Symbol);
            begin
               if Key_Is_Empty then
                  Key_Map.Available.Include (Key);
               end if;
            end;
         end loop;
         Unused := XFree (Keysyms_Ptr.all'Address);
      end;
      if Key_Map.Available.Is_Empty then
         raise GENERAL_X11_ERROR with "No empty key available as scratch space. This is technically not a problem but so unusual that I haven't bothered to code for this scenario...";
      end if;
   end Update_Mapping;


   procedure Fake_Press (Display : Display_Access; Key : C.Int) is
      XTestRelease : constant C.Int := 0;
      XTestPress : constant C.Int := 1;
      I : C.Int;
   begin
      I := XTestFakeKeyEvent (Display, Key, XTestPress, 0);
      I := XSync (Display, 0);
      I := XTestFakeKeyEvent (Display, Key, XTestRelease, 0);
      I := XSync (Display, 0);
   end Fake_Press;
end Output_Backend;
