package body X11 is
   task body Thread is
      Thread_Params : Parameters;
   begin
      select
         accept Setup_Keygrabs (Key_Array : Keycode_Array) do
            Thread_Params := Setup_Keygrabs_Body (Key_Array);
         end Setup_Keygrabs;
      or
         terminate;
      end select;
      loop
         select
            when XPending (Thread_Params.Display) > 0 =>
               accept Next_Event (The_Event : out Event) do
                  The_Event := Next_Event_Body (Thread_Params);
               end Next_Event;
         or
            accept Fake_Input_String (Text : String) do
               Fake_Input_String_Body (Thread_Params, Text);
            end Fake_Input_String;
         or
            terminate;
         end select;
      end loop;
   end Thread;
   
   function Setup_Keygrabs_Body (Key_Array : Keycode_Array) return Parameters is
      Display : constant Display_Access := Open_Display;
      XInput_Opcode : C.Int := Query_Extension (Display, "XInputExtension");
      XTest_Opcode : C.Int := Query_Extension (Display, "XTEST");
      Keyboard_Array : constant XIDeviceInfo_Array := Real_Keyboards (Query_Devices (Display));
      I : C.Int := XSync (Display, 0);
      Keyboards : Device_Vectors.Vector;
      Keys : Keycode_Vectors.Vector;
   begin
      Grab_Keycodes (Display, Keyboard_Array, Key_Array);
      Select_Events (Display, Keyboard_Array);
      for Keyboard of Keyboard_Array loop
         Keyboards.Append (Keyboard);
      end loop;
      for Key of Key_Array loop
         Keys.Append (Key);
      end loop;
      I := XSync (Display, 0);
      return
        (Display => Display,
         XInput_Opcode => XInput_Opcode,
         Devices => Keyboards,
         Keys => Keys);
   end Setup_Keygrabs_Body;
   
   function Open_Display return Display_Access is
   begin
      return XOpenDisplay (C.Strings.Null_Ptr);
   end Open_Display;
   
   function Query_Extension (Display : Display_Access; Name : String) return C.Int is
      Opcode, Ev, Err : C.Int := 0;
   begin
      if XQueryExtension (Display, C.Strings.New_String (Name), Opcode, Ev, Err) = 0 then
         raise CONSTRAINT_ERROR with Name & " not available";
      end if;
      return Opcode;
   end Query_Extension;


   function Default_Root_Window (Display : Display_Access) return Window is
   begin
      return Window (XDefaultRootWindow (Display));
   end Default_Root_Window;


   function Query_Devices (Display : Display_Access) return XIDeviceInfo_Array is

      Num_Devices : C.Int := 0;
      XIDeviceInfo_Ptr : constant XIDeviceInfo_Accesses.Pointer := XIQueryDevice (Display, XIAllDevices, Num_Devices);
      Devices : constant XIDeviceInfo_Array := XIDeviceInfo_Accesses.Value (XIDeviceInfo_Ptr, C.Ptrdiff_T (Num_Devices));
      I : C.Int;
   begin
      XIFreeDeviceInfo (XIDeviceInfo_Ptr);
      I := XSync (Display, 0);
      return Devices;
   end Query_Devices;
   

   function Real_Keyboards (Devices : XIDeviceInfo_Array) return XIDeviceInfo_Array is
      function Is_Real_Keyboard (Device : XIDeviceInfo) return Boolean is
         Device_Classes : XIAnyClassInfo_Access_Array := XIAnyClassInfo_Access_Accesses.Value (Device.Classes, C.Ptrdiff_T (Device.Num_Classes));
         Is_Keyboard : Boolean := Device.Use_Type = XISlaveKeyboard;
         Is_Enabled : Boolean := Device.Enabled = 1;
         Is_Real : Boolean := C.Strings.Value (Device.Name) /= XTestKeyboard;
      begin
         if Is_Keyboard and Is_Enabled and Is_Real then
            for Class of Device_Classes loop
                if Class.Class_Type = XIKeyClass then
                   return True;
                end if;
            end loop;
         end if;
         return False;
      end Is_Real_Keyboard;

      Keyboards : XIDeviceInfo_Array (Devices'Range);
      Keyboard_Count : Natural := 0;
   begin
      for Device of Devices loop
         if Is_Real_Keyboard (Device) then
            Keyboard_Count := Keyboard_Count + 1;
            Keyboards (Keyboard_Count) := Device;
         end if;
      end loop;
      return Keyboards (1 .. Keyboard_Count);
   end Real_Keyboards;
   

   procedure Grab_Keycodes (Display : Display_Access; Devices : XIDeviceInfo_Array; Keys : Keycode_Array) is
      
      Root : constant Window := Default_Root_Window (Display);
      use type Interfaces.Unsigned_8;
      Mask_Bytes : constant C.Char_Array (0..0) := (others => C.To_C (Character'Val (XIKeyPressMask or XIKeyREleaseMask)));
      I : C.Int;
   begin
      for Key of Keys loop
         for Device of Devices loop
            declare
               Mask : XIEventMask := (Device.Device_ID, Mask_Bytes'Length, C.Strings.New_Char_Array (Mask_Bytes));
               Grab_Modifiers : XIGrabModifiers := (0,0);
            begin
               I := XIGrabKeycode (Display, Device.Device_ID, C.Int (Key), Root, XIGrabModeAsync, XIGrabModeAsync, 1, Mask, 1, Grab_Modifiers);
               if I /= 0 then
                  raise CONSTRAINT_ERROR with "Could not establish grab!";
               end if;
            end;
         end loop;
      end loop;
   end Grab_Keycodes;
   
   
   procedure Select_Events (Display : Display_Access; Devices : XIDeviceInfo_Array) is

      Root : constant Window := Default_Root_Window (Display);
      use type Interfaces.Unsigned_8;
      Mask_Bytes : constant C.Char_Array (0..0) := (others => C.To_C (Character'Val (XIKeyPressMask or XIKeyReleaseMask)));
      Mask : XIEventMask := (XIAllDevices, Mask_Bytes'Length, C.Strings.New_Char_Array (Mask_Bytes));
      I : C.Int := XISelectEvents (Display, Root, Mask, 1);
   begin
      if I /= 0 then
         raise CONSTRAINT_ERROR with "Could not select events!";
      end if;
   end Select_Events;


   function Next_Event_Body (Params : Parameters) return Event is
      function Correct_Device (Device_Id : C.Int) return Boolean is
      begin
         for Device of Params.Devices loop
            if Device.Device_Id = Device_Id then
               return True;
            end if;
         end loop;
         return False;
      end Correct_Device;

      function Correct_Key (Keycode : C.Int) return Boolean is
      begin
         for Key of Params.Keys loop
            if C.Int (Key) = Keycode then
               return True;
            end if;
         end loop;
         return False;
      end Correct_Key;

      Any_Event : XEvent.Event;
      I : C.Int;
   begin
      I := XEvent.XNextEvent (Params.Display.all'Address, Any_Event);
      if Any_Event.C_type = GenericEvent then
         declare
            Event_Cookie : XEvent.XGenericEventCookie := Any_Event.XCookie;
         begin
            I := XEvent.XGetEventData (Params.Display.all'Address, Event_Cookie);
            if I /= 0 and Event_Cookie.Extension = Params.XInput_Opcode then
               declare
                  package CastDeviceEvent is new System.Address_To_Access_Conversions (Object => XEvent.XIDeviceEvent);
                  subtype XIDeviceEvent_Access is CastDeviceEvent.Object_Pointer;
                  Device_Event : XIDeviceEvent_Access := CastDeviceEvent.To_Pointer (Event_Cookie.Data);
               begin
                  if Device_Event.Evtype = XIKeyPress or Device_Event.Evtype = XIKeyRelease then
                     if Correct_Device (Device_Event.SourceID) and Correct_Key (Device_Event.Detail) then return 
                          (Event_Variant => Key_Event,
                           Key => Keycode (Device_Event.Detail),
                           Key_Event_Variant => (if Device_Event.Evtype = XIKeyPress then Key_Press else Key_Release),
                           Time => Timestamp (Device_Event.The_Time));
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;
      return (Event_Variant => Other_Event);
   end Next_Event_Body;


   procedure Fake_Input_String_Body (Params : Parameters; Text : String) is

      procedure Fake_Press (Key : C.Int) is
         XTestRelease : constant C.Int := 0;
         XTestPress : constant C.Int := 1;
         I : C.Int;
      begin
         I := XTestFakeKeyEvent (Params.Display, Key, XTestPress, 0);
         I := XTestFakeKeyEvent (Params.Display, Key, XTestRelease, 0);
      end Fake_Press;
      
      function Bind_Character (Letter : Character) return C.Int is
         use type C.Unsigned_Long;
         
         No_Symbol : C.Unsigned_Long := 0;
         Min_Keycode : C.Int := 0;
         Max_Keycode : C.Int := 0;

      begin
         XDisplayKeycodes (Params.Display, Min_Keycode, Max_Keycode);
         declare
            Key_Count : C.Int := Max_Keycode - Min_Keycode + 1;
            Keysyms_Per_Key : C.Int := 0;
            Keysyms_Ptr : Keysym_Accesses.Pointer := XGetKeyboardMapping (Params.Display, Min_Keycode, Key_Count, Keysyms_Per_key);
            Keysyms : Keysym_Array := Keysym_Accesses.Value (Keysyms_Ptr, C.Ptrdiff_T (Key_Count * Keysyms_Per_Key));
            Scratch_Key : C.Int := 0;
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
                     Scratch_Key := Key;
                     exit;
                  end if;
               end;
            end loop;
            if Scratch_Key = 0 then
               raise CONSTRAINT_ERROR with "no empty key available for binding";
            end if;
            declare
               I : C.Int := 0;
               Keysym : C.Unsigned_Long := Character'Pos (Letter);
            begin
               I := XChangeKeyboardMapping (Params.Display, Scratch_Key, 1, Keysym, 1);
               I := XSync (Params.Display, 0);
            end;
            return Scratch_Key;
         end;
      end Bind_Character;

      function Is_Basic_Latin1 (Letter : Character) return Boolean is
      begin
         return Character'Pos (Letter) >= 16#20# and Character'Pos (Letter) <= 16#fe#;
      end Is_Basic_Latin1;
   begin
      for Letter of Text loop
         if not Is_Basic_Latin1 (Letter) then
            raise CONSTRAINT_ERROR with "Invalid character in string";
         end if;
         
         declare
            Key : C.Int := Bind_Character (Letter);
            I : C.Int := 0;
            Keysym : C.Unsigned_Long := 0;
         begin
            Fake_Press (Key);
            I := XSync (Params.Display, 0);
            I := XChangeKeyboardMapping (Params.Display, Key, 1, Keysym, 1);
            I := XSync (Params.Display, 0);
         end;
      end loop;
   end Fake_Input_String_Body;
end X11;
