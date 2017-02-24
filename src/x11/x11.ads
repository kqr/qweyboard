private with Interfaces;
private with Interfaces.C;
private with Interfaces.C.Strings;
private with Interfaces.C.Pointers;
private with System.Address_To_Access_Conversions;
private with Ada.Containers.Ordered_Sets;
private with Logging;
private with XLib_H;
private with XEvent;
private with XInput2_H;
private with XTest_H;

package X11 is
   EXTENSION_MISSING, HARDWARE_PROBLEM : exception;
   GENERAL_X11_ERROR : exception;
   ENCODING_ERROR : exception;

   subtype Keycode is Long_Long_Integer range 0 .. 2**32;
   type Keycode_Array is array (Positive range <>) of X11.Keycode;

   type Event_Variant_Type is (Key_Event, Other_Event);
   type Key_Event_Variant_Type is (Key_Press, Key_Release);
   type Event (Event_Variant : Event_Variant_Type := Other_Event) is record
      case Event_Variant is
         when Key_Event =>
            Key : Keycode;
            Key_Event_Variant : Key_Event_Variant_Type;
         when Other_Event =>
            null;
      end case;
   end record;
   
   task Thread is
      entry Capture_Keys (Key_Array : Keycode_Array);
      entry Get_Key_Event (The_Event : out Event);
      entry Output (Text : String);
   end Thread;
private
   package C renames Interfaces.C;
   use type C.Int, C.Unsigned;
   use type C.Unsigned;
   use XLib_H, XEvent, XInput2_H, XTest_H;
   
   function "<" (A : XIDeviceInfo; B : XIDeviceInfo) return Boolean;
   package Device_Sets is new Ada.Containers.Ordered_Sets (Element_Type => XIDeviceInfo);
   package Keycode_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Keycode);

   type Parameters is record
      Display : Display_Access;
      XInput_Opcode : C.Int;
      XTest_Opcode : C.Int;
      Devices : Device_Sets.Set;
      Keys : Keycode_Sets.Set;
   end record;

   procedure Initialise (Params : in out Parameters);
   procedure Register_Real_Keyboards (Params : in out Parameters);
   procedure Grab_Keys (Params : in out Parameters; Keys : Keycode_Array);

   procedure Setup_Keygrabs (Params : in out Parameters; Key_Array : Keycode_Array);
   function Next_Event (Params : Parameters) return Event;
   procedure Fake_Input_String (Params : Parameters; Text : String);

   function Key_Event_Mask (Device_Id : C.Int) return XIEventMask;
end X11;
