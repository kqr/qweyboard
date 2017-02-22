with Ada.Text_IO;
private with Interfaces;
private with Interfaces.C;
private with Interfaces.C.Strings;
private with Interfaces.C.Pointers;
private with System.Address_To_Access_Conversions;
private with Ada.Containers.Vectors;
private with XEvent;

package X11 is
   subtype Keycode is Long_Long_Integer range 0 .. 2**32;
   type Keycode_Array is array (Positive range <>) of X11.Keycode;

   subtype Timestamp is Long_Long_Integer range 0 .. 2**32;
   
   type Event_Variant_Type is (Key_Event, Other_Event);
   type Key_Event_Variant_Type is (Key_Press, Key_Release);
   type Event (Event_Variant : Event_Variant_Type := Other_Event) is record
      case Event_Variant is
         when Key_Event =>
            Key : Keycode;
            Key_Event_Variant : Key_Event_Variant_Type;
            Time : Timestamp;
         when Other_Event =>
            null;
      end case;
   end record;
   
   task Thread is
      entry Setup_Keygrabs (Key_Array : Keycode_Array);
      entry Next_Event (The_Event : out Event);
      entry Fake_Input_String (Text : String);
   end Thread;
private
   package C renames Interfaces.C;
   use type C.Int;
   use type C.Unsigned;

   GenericEvent : constant C.Int := 35;
   XIAllDevices : constant C.Int := 0;
   XISlaveKeyboard : constant C.Int := 4;
   XIKeyClass : constant C.Int := 0;
   XIGrabModeSync : constant C.Int := 0;
   XIGrabModeAsync : constant C.Int := 1;
   XIKeyPress : constant C.Int := 2;
   XIKeyRelease : constant C.Int := 3;
   XIKeyPressMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, Integer (XIKeyPress));
   XIKeyReleaseMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, Integer (XIKeyRelease));
   XTestKeyboard : constant String := "Virtual core XTEST keyboard";
   
   type Display is null record;
   type Display_Access is access all Display;
   function Open_Display return Display_Access;

   function Query_Extension (Display : Display_Access; Name : String) return C.Int;
   
   function XSync (Display : Display_Access; Discard : C.Int) return C.Int;
   pragma Import (C, XSync, "XSync");
   
   function XPending (Display : Display_Access) return C.Int;
   pragma Import (C, XPending, "XPending");
   
   type Window is new C.Unsigned_Long;
   function Default_Root_Window (Display : Display_Access) return Window;
   
   type XIAnyClassInfo is record
      Class_Type : C.Int;
      Source_ID : C.Int;
   end record with Convention => C_Pass_By_Copy;
   type XIAnyClassInfo_Access is access all XIAnyClassInfo;
   type XIAnyClassInfo_Access_Array is array (C.Unsigned range <>) of aliased XIAnyClassInfo_Access;
   package XIAnyClassInfo_Access_Accesses is new C.Pointers
     (Index => C.Unsigned,
      Element => XIAnyClassInfo_Access,
      Element_Array => XIAnyClassInfo_Access_Array,
      Default_Terminator => null);
   
   type XIDeviceInfo is record
      Device_ID : C.Int;
      Name : C.Strings.Chars_Ptr;
      Use_Type : C.Int;
      Attachment : C.Int;
      Enabled : C.Int;
      Num_Classes : C.Int;
      Classes : XIAnyClassInfo_Access_Accesses.Pointer;
   end record with Convention => C_Pass_By_Copy;
   type Device_Array is array (Natural range <>) of aliased XIDeviceInfo;
   
   type XIEventMask is record
      Device_Id : C.Int;
      Mask_Len : C.Int;
      Mask : C.Strings.Chars_Ptr;
   end record with Convention => C_Pass_By_Copy;
   type XIEventMask_Access is access all XIEventMask;
   
   type XIGrabModifiers is record
      Modifiers : C.Int;
      Status : C.Int;
   end record with Convention => C_Pass_By_Copy;

   function Query_Devices (Display : Display_Access) return Device_Array;
   function Real_Keyboards (Devices : Device_Array) return Device_Array;
   procedure Select_Events (Display : Display_Access; Devices : Device_Array);
   procedure Grab_Keycodes (Display : Display_Access; Devices : Device_Array; Keys : Keycode_Array);

   package Device_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => XIDeviceInfo);
   package Keycode_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => X11.Keycode);
   type Parameters is record
      Display : Display_Access;
      XInput_Opcode : C.Int;
      Devices : Device_Vectors.Vector;
      Keys : Keycode_Vectors.Vector;
   end record;

   function Setup_Keygrabs_Body (Key_Array : Keycode_Array) return Parameters;
   function Next_Event_Body (Params : Parameters) return Event;
   procedure Fake_Input_String_Body (Params : Parameters; Text : String);
end X11;
