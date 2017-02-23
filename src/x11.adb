package body X11 is
   task body Thread is
      Thread_Params : Parameters;
   begin
      select
         accept Setup_Keygrabs (Key_Array : Keycode_Array) do
            Setup_Keygrabs_Body (Thread_Params, Key_Array);
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
   
   procedure Setup_Keygrabs_Body (Params : in out Parameters; Key_Array : Keycode_Array) is
      Ev : C.Int;
      Err : C.Int;
      I : C.Int;
      Select_Event_Mask : XIEventMask;
   begin
      Params.Display := XOpenDisplay (C.Strings.Null_Ptr);
      Register_Real_Keyboards (Params);

      if XQueryExtension (Params.Display, C.Strings.New_String ("XInputExtension"), Params.XInput_Opcode, Ev, Err) = 0 then
         raise CONSTRAINT_ERROR with "XInputExtension not available";
      end if;
      if XQueryExtension (Params.Display, C.Strings.New_String ("XTEST"), Params.XTest_Opcode, Ev, Err) = 0 then
         raise CONSTRAINT_ERROR with "XTest not available";
      end if;

      Select_Event_Mask := Key_Event_Mask (XIAllDevices);
      if XISelectEvents (Params.Display, XDefaultRootWindow (Params.Display), Select_Event_Mask, 1) /= 0 then
         raise CONSTRAINT_ERROR with "Could not select events!";
      end if;

      Grab_Keys (Params, Key_Array);

      I := XSync (Params.Display, 0);
   end Setup_Keygrabs_Body;
   


   procedure Register_Real_Keyboards (Params : in out Parameters) is
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

      Num_Devices : C.Int;
      XIDeviceInfo_Ptr : XIDeviceInfo_Accesses.Pointer;
   begin
      XIDeviceInfo_Ptr := XIQueryDevice (Params.Display, XIAllDevices, Num_Devices);
      declare
         Devices : XIDeviceInfo_Array := XIDeviceInfo_Accesses.Value (XIDeviceInfo_Ptr, C.Ptrdiff_T (Num_Devices));
      begin
         for Device of Devices loop
            if Is_Real_Keyboard (Device) then
               Params.Devices.Include (Device);
            end if;
         end loop;
      end;
      XIFreeDeviceInfo (XIDeviceInfo_Ptr);
   end Register_Real_Keyboards;
   
   function Key_Event_Mask (Device_Id : C.Int) return XIEventMask is
      use type Interfaces.Unsigned_8;
      Mask_Bytes : C.Char_Array (0..0) := (others => C.To_C (Character'Val (XIKeyPressMask or XIKeyREleaseMask)));
   begin
      return (Device_ID, Mask_Bytes'Length, C.Strings.New_Char_Array (Mask_Bytes));
   end;

   procedure Grab_Keys (Params : in out Parameters; Keys : Keycode_Array) is
      use type Interfaces.Unsigned_8;
      I : C.Int;
   begin
      for Key of Keys loop
         for Device of Params.Devices loop
            declare
               Grab_Modifiers : XIGrabModifiers := (0,0);
               Mask : XIEventMask := Key_Event_Mask (Device.Device_ID);
            begin
               I := XIGrabKeycode (Params.Display, Device.Device_ID, C.Int (Key), XDefaultRootWindow (Params.Display), XIGrabModeAsync, XIGrabModeAsync, 1, Mask, 1, Grab_Modifiers);
               if I /= 0 then
                  raise CONSTRAINT_ERROR with "Could not establish grab!";
               end if;
            end;
         end loop;
         Params.Keys.Include (Key);
      end loop;
   end Grab_Keys;
   


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


   function "<" (A : XIDeviceInfo; B : XIDeviceInfo) return Boolean is
   begin
      return A.Device_ID < B.Device_ID;
   end;
end X11;
