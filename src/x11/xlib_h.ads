with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

package XLib_H is
   package C renames Interfaces.C;

   GenericEvent : constant C.Int := 35;

   type Display is null record;
   type Display_Access is access all Display;
   
   subtype Keycode is C.Int;
   
   subtype XID is C.Unsigned_Long;
   subtype Window is XID;
   subtype Keysym is XID;

   function XOpenDisplay (Display_Name : C.Strings.Chars_Ptr) return Display_Access;
   pragma Import (C, XOpenDisplay, "XOpenDisplay");

   function XQueryExtension (Display : Display_Access; Name : C.Strings.Chars_Ptr; Major_Opcode : out C.Int; Event : out C.Int; Error : out C.Int) return C.Int;
   pragma Import (C, XQueryExtension, "XQueryExtension");

   function XSync (Display : Display_Access; Discard : C.Int) return C.Int;
   pragma Import (C, XSync, "XSync");
   
   function XDefaultRootWindow (Display : Display_Access) return Window;
   pragma Import (C, XDefaultRootWindow, "XDefaultRootWindow");

   function XPending (Display : Display_Access) return C.Int;
   pragma Import (C, XPending, "XPending");

   type Keysym_Array is array (C.Unsigned range <>) of aliased Keysym;
   package Keysym_Accesses is new C.Pointers (Index => C.Unsigned, Element => Keysym, Element_Array => Keysym_Array, Default_Terminator => 0);

   procedure XDisplayKeycodes (Display : Display_Access; Min : out C.Int; Max : out C.Int);
   pragma Import (C, XDisplayKeycodes, "XDisplayKeycodes");
   
   function XGetKeyboardMapping (Display : Display_Access; Key : C.Int; Keycode_Count : C.Int; Keysyms_Count : out C.Int) return Keysym_Accesses.Pointer;
   pragma Import (C, XGetKeyboardMapping, "XGetKeyboardMapping");

   function XChangeKeyboardMapping (Display : Display_Access; Keycode : C.Int; Keysyms_Per_Key : C.Int; KeySyms : out Keysym; Num_Codes : C.Int) return C.Int;
   pragma Import (C, XChangeKeyboardMapping, "XChangeKeyboardMapping");
end XLib_H;
