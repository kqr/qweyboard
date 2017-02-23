with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with System.Address_To_Access_Conversions;
with Ada.Containers.Vectors;
with XEvent;
with XLib_H; use XLib_H;

package XTest_H is
   package C renames Interfaces.C;

   XTestKeyboard : constant String := "Virtual core XTEST keyboard";

   function XTestFakeKeyEvent (Display : Display_Access; Keycode : C.Int; Is_Press : C.Int; After_Delay : C.Unsigned_Long) return C.Int;
   pragma Import (C, XTestFakeKeyEvent, "XTestFakeKeyEvent");
end XTest_H;
