with XLib_H; use XLib_H;

package XTest_H is
   XTestKeyboard : constant String := "Virtual core XTEST keyboard";

   function XTestFakeKeyEvent (Display : Display_Access; Keycode : C.Int; Is_Press : C.Int; After_Delay : C.Unsigned_Long) return C.Int;
   pragma Import (C, XTestFakeKeyEvent, "XTestFakeKeyEvent");
end XTest_H;
