with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with XLib_H; use XLib_H;

package XInput2_H is
   XIAllDevices : constant C.Int := 0;
   XISlaveKeyboard : constant C.Int := 4;
   XIKeyClass : constant C.Int := 0;
   XIGrabModeSync : constant C.Int := 0;
   XIGrabModeAsync : constant C.Int := 1;
   XIKeyPress : constant C.Int := 2;
   XIKeyRelease : constant C.Int := 3;
   XIKeyPressMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, Integer (XIKeyPress));
   XIKeyReleaseMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, Integer (XIKeyRelease));
   
   ShiftMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, 0);
   ControlMask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, 2);
   Mod1Mask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, 3);
   Mod2Mask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, 4);
   Mod3Mask : constant Interfaces.Unsigned_8 := Interfaces.Shift_Left (1, 5);

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
   type XIDeviceInfo_Array is array (Natural range <>) of aliased XIDeviceInfo;
   package XIDeviceInfo_Accesses is new C.Pointers
     (Index => Natural,
      Element => XIDeviceInfo,
      Element_Array => XIDeviceInfo_Array,
      Default_Terminator => (Name => C.Strings.Null_Ptr, Classes => null, others => 0));
   
   procedure XIFreeDeviceInfo (XIDeviceInfo_Ptr : XIDeviceInfo_Accesses.Pointer);
   pragma Import (C, XIFreeDeviceInfo, "XIFreeDeviceInfo");

   function XIQueryDevice
     (Display : Display_Access; 
      Device_ID : C.Int;
      Num_Devices : out C.Int) return XIDeviceInfo_Accesses.Pointer;
   pragma Import (C, XIQueryDevice, "XIQueryDevice");

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

   function XIGrabKeycode
     (Display : Display_Access;
      Device_ID : C.Int;
      X_Keycode : C.Int;
      Grab_Window : Window;
      Grab_Mode : C.Int;
      Paired_Device_Mode : C.Int;
      Owner_Events : C.Int;
      Mask : in out XIEventMask;
      Num_Modifiers : C.Int;
      Modifiers : in out XIGrabModifiers) return C.Int;
   pragma Import (C, XIGrabKeycode, "XIGrabKeycode");

   function XIUngrabKeycode
     (Display : Display_Access;
      Device_ID : C.Int;
      X_Keycode : C.Int;
      Grab_Window : Window;
      Num_Modifiers : C.Int;
      Modifiers : in out XIGrabModifiers) return C.Int;
   pragma Import (C, XIUngrabKeycode, "XIUngrabKeycode");

   function XISelectEvents (Display : Display_Access; Win : Window; Mask : in out XIEventMask; Num_Masks : C.Int) return C.Int;
   pragma Import (C, XISelectEvents, "XISelectEvents");
end XInput2_H;
