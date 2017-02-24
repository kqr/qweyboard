package body X11 is
   task body Thread is
      Thread_Params : Parameters;
   begin
      select accept Capture_Keys (Key_Array : Keycode_Array) do
         Log.Chat ("[X11] Initialising thread params");
         Initialise (Thread_Params);
         Log.Chat ("[X11] Setting up key grabs and event selects");
         Setup_Keygrabs (Thread_Params, Key_Array);
         Log.Chat ("[X11] Grabbing keys");
         Grab_Keys (Thread_Params);
      end Capture_Keys; or terminate; end select;

      loop
         select
            accept Disable_Grabs do
               Log.Chat ("[X11] Asked to disable grabs");
               Grab_Keys (Thread_Params, Ungrab => True);
            end Disable_Grabs;
         or
            accept Enable_Grabs do
               Log.Chat ("[X11] Asked to enable grabs");
               Grab_Keys (Thread_Params);
            end Enable_Grabs;
         or
            when XPending (Thread_Params.Display) > 0 =>
               accept Get_Key_Event (The_Event : out Event) do
                  Log.Chat ("[X11] Asked to fetch the next event");
                  The_Event := Next_Event (Thread_Params);
               end Get_Key_Event;
         or
            accept Output (Text : String) do
               Log.Chat ("[X11] Outputting text");
               Fake_Input_String (Thread_Params, Text);
            end Output;
         or
            terminate;
         end select;
      end loop;
   end Thread;
   
   procedure Initialise (Params : in out Parameters) is
      Ev : C.Int;
      Err : C.Int;
   begin
      Params.Display := XOpenDisplay (C.Strings.Null_Ptr);

      if XQueryExtension (Params.Display, C.Strings.New_String ("XInputExtension"), Params.XInput_Opcode, Ev, Err) = 0 then
         raise EXTENSION_MISSING with "XInputExtension not available, cannot continue";
      end if;
      if XQueryExtension (Params.Display, C.Strings.New_String ("XTEST"), Params.XTest_Opcode, Ev, Err) = 0 then
         raise EXTENSION_MISSING with "XTest not available, cannot continue";
      end if;
   end Initialise;

   procedure Setup_Keygrabs (Params : in out Parameters; Key_Array : Keycode_Array) is
      I : C.Int;
      Select_Event_Mask : XIEventMask;
   begin
      Register_Real_Keyboards (Params);

      Select_Event_Mask := Key_Event_Mask (XIAllDevices);
      if XISelectEvents (Params.Display, XDefaultRootWindow (Params.Display), Select_Event_Mask, 1) /= 0 then
         raise HARDWARE_PROBLEM with "Unable to select events, cannot continue";
      end if;
      
      for Key of Key_Array loop
         Params.Keys.Include (Key);
      end loop;

      I := XSync (Params.Display, 0);
   end Setup_Keygrabs;

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

   procedure Grab_Keys (Params : in out Parameters; Ungrab : Boolean := False) is
      use type Interfaces.Unsigned_8;
      I : C.Int;
   begin
      for Key of Params.Keys loop
         for Device of Params.Devices loop
            declare
               Grab_Modifiers : XIGrabModifiers := (0,0);
               Mask : XIEventMask := Key_Event_Mask (Device.Device_ID);
            begin
               if Ungrab then
                  I := XIUngrabKeycode (Params.Display, Device.Device_ID, C.Int (Key), XDefaultRootWindow (Params.Display), 1, Grab_Modifiers);
               else
                  I := XIGrabKeycode (Params.Display, Device.Device_ID, C.Int (Key), XDefaultRootWindow (Params.Display), XIGrabModeAsync, XIGrabModeAsync, 1, Mask, 1, Grab_Modifiers);
               end if;
               if I /= 0 then
                  raise HARDWARE_PROBLEM with "Unable to establish key grabs, unable to continue";
               end if;
            end;
         end loop;
      end loop;
   end Grab_Keys;
   


   function Next_Event (Params : Parameters) return Event is
      package CastDeviceEvent is new System.Address_To_Access_Conversions (Object => XEvent.XIDeviceEvent);
      subtype XIDeviceEvent_Access is CastDeviceEvent.Object_Pointer;

      function Correct_Device (Event : XIDeviceEvent_Access) return Boolean is
      begin
         for Device of Params.Devices loop
            if Device.Device_Id = Event.SourceID then
               return True;
            end if;
         end loop;
         return False;
      end Correct_Device;

      Any_Event : XEvent.Event;
      Event_Cookie : XEvent.XGenericEventCookie;
      Device_Event : XIDeviceEvent_Access;
      I : C.Int;
   begin
      Log.Chat ("[X11] Trying to get next event from xlib");
      I := XEvent.XNextEvent (Params.Display.all'Address, Any_Event);
      Log.Chat ("[X11] Checking if event is a generic event");
      if Any_Event.C_type = GenericEvent then
         Event_Cookie := Any_Event.XCookie;
         Log.Chat ("[X11] Loading the extra xcookie data");
         I := XEvent.XGetEventData (Params.Display.all'Address, Event_Cookie);
         if I /= 0 and Event_Cookie.Extension = Params.XInput_Opcode then
            Log.Chat ("[X11] discovered xinput2 event, attempting to treat as device event");
            Device_Event := CastDeviceEvent.To_Pointer (Event_Cookie.Data);
            if Device_Event.Evtype = XIKeyPress or Device_Event.Evtype = XIKeyRelease then
               Log.Chat ("[X11] it is a key event!");
               if Correct_Device (Device_Event) then
                  Log.Chat ("[X11] device event is from a valid device, returning");
                  declare
                     Variant : Key_Event_Variant_Type := (if Device_Event.Evtype = XIKeyPress then Key_Press else Key_Release);
                     Key : Keycode := Keycode (Device_Event.Detail);
                     Mods : Modifier_Set := Modifier_Set (Device_Event.Mods.Base);
                  begin
                     return (Event_Variant => Key_Event, Key_Variant => Variant, Key => Key, Modifiers => Mods);
                  end;
               end if;
            end if;
         end if;
      end if;
      return (Event_Variant => Other_Event);
   end Next_Event;


   procedure Fake_Input_String (Params : Parameters; Text : String) is
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
               raise GENERAL_X11_ERROR with "No empty key available as scratch space. This is technically not a problem but so unusual that I haven't bothered to code for this scenario...";
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
      Log.Info ("[X11] Outputting <" & Text & ">");
      for Letter of Text loop
         if not Is_Basic_Latin1 (Letter) then
            raise ENCODING_ERROR with "Invalid character in string. Only basic Latin 1 has a simple mapping to Keysyms and that's as much as I'm prepared to deal with right now. Pull requests welcome!";
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
   end Fake_Input_String;


   function Shift_Pressed (Modifiers : Modifier_Set) return Boolean is
      use type Interfaces.Unsigned_8;
   begin
      return (Interfaces.Unsigned_8 (Modifiers) and ShiftMask) > 0;
   end;
   function Control_Pressed (Modifiers : Modifier_Set) return Boolean is
      use type Interfaces.Unsigned_8;
   begin
      return (Interfaces.Unsigned_8 (Modifiers) and ControlMask) > 0;
   end;

   function "<" (A : XIDeviceInfo; B : XIDeviceInfo) return Boolean is
   begin
      return A.Device_ID < B.Device_ID;
   end;
end X11;
