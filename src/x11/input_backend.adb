with Ada.Finalization;
with XLib_H; use XLib_H;
with XInput2_H; use XInput2_H;
with XEvent; use XEvent;
with XTest_H; use XTest_H;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

package body Input_Backend is
   package C renames Interfaces.C;
   use type C.Int;

   package Keycode_Mapping is new Ada.Containers.Ordered_Maps (Key_Type => C.Int, Element_Type => Qweyboard.Softkey, "=" => Qweyboard."=");
   From_Keycode : Keycode_Mapping.Map;

   function "<" (A : XIDeviceInfo; B : XIDeviceInfo) return Boolean is
   begin
      return A.Device_ID < B.Device_ID;
   end;
   package Device_Sets is new Ada.Containers.Ordered_Sets (Element_Type => XIDeviceInfo);
   package Keycode_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Keycode);

   type Grab_Status is new Ada.Finalization.Controlled with record
      Display : Display_Access;
      Enabled : Boolean;
      Devices : Device_Sets.Set;
      Keys : Keycode_Sets.Set;
   end record;

   procedure Setup_Keygrabs (Grab : in out Grab_Status);
   procedure Register_Real_Keyboards (Grab : in out Grab_Status);
   procedure Grab_Keys (Grab : in out Grab_Status; Ungrab : Boolean := False);
   function Next_Event (Display : Display_Access; XInput_Opcode : C.Int; Devices : Device_Sets.Set) return Qweyboard.Key_Event;
   function Convert_Event (Device_Event : XIDeviceEvent_Access) return Qweyboard.Key_Event;
   function Key_Event_Mask (Device_Id : C.Int) return XIEventMask;

   procedure Finalize (Grab : in out Grab_Status) is
      I : C.Int;
   begin
      if Grab.Enabled then
         Grab_Keys (Grab, Ungrab => True);
      end if;
      Log.Warning ("[X11.Input] Closing display");
      I := XCloseDisplay (Grab.Display);
   end;

   task body Input is
      Grab : Grab_Status;
      XInput_Opcode : C.Int;
      Last_Event : Qweyboard.Key_Event;
      Suspended : Boolean := False;
   begin
      Log.Chat ("[X11.Input] Opening display");
      Grab.Display := XOpenDisplay (C.Strings.Null_Ptr);
      declare
         Ev : C.Int;
         Err : C.Int;
         Ext_Name : C.Strings.Chars_Ptr := C.Strings.New_String ("XInputExtension");
      begin
         if XQueryExtension (Grab.Display, Ext_Name, XInput_Opcode, Ev, Err) = 0 then
            raise EXTENSION_MISSING with "XInputExtension not available, cannot continue";
         end if;
         C.Strings.Free (Ext_Name);
      end;

      Log.Chat ("[X11.Input] Setting up key grabs and event selects");
      Setup_Keygrabs (Grab);

      accept Ready_Wait;
      Log.Chat ("[X11.Input] Entering loop");
      loop
         if Suspended and Grab.Enabled then
            Log.Warning ("[X11.Input] Input backend suspended on user request");
            Grab_Keys (Grab, Ungrab => True);
         elsif not Suspended and not Grab.Enabled then
            Log.Warning ("[X11.Input] Input backend enabled on user request");
            Grab_Keys (Grab);
         end if;
         
         Log.Chat ("[X11.Input] Fetching next event");
         Last_Event := Next_Event (Grab.Display, XInput_Opcode, Grab.Devices);

         if not Suspended then
            Log.Chat ("[X11.Input] Telling backend about event");
            Qweyboard.Softboard.Handle (Last_Event);
         end if;
         
         if Qweyboard."=" (Last_Event.Key, Qweyboard.Susp) then
            Suspended := not Suspended;
         end if;
      end loop;
   end Input;
   
   procedure Setup_Keygrabs (Grab : in out Grab_Status) is
      Select_Event_Mask : XIEventMask;
   begin
      for C in From_Keycode.Iterate loop
         Grab.Keys.Include (Keycode_Mapping.Key (C));
      end loop;

      Register_Real_Keyboards (Grab);

      Select_Event_Mask := Key_Event_Mask (XIAllDevices);
      if XISelectEvents (Grab.Display, XDefaultRootWindow (Grab.Display), Select_Event_Mask, 1) /= 0 then
         raise HARDWARE_PROBLEM with "Unable to select events, cannot continue";
      end if;
      C.Strings.Free (Select_Event_Mask.Mask);
   end Setup_Keygrabs;

   procedure Register_Real_Keyboards (Grab : in out Grab_Status) is
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
      XIDeviceInfo_Ptr := XIQueryDevice (Grab.Display, XIAllDevices, Num_Devices);
      declare
         Devices : XIDeviceInfo_Array := XIDeviceInfo_Accesses.Value (XIDeviceInfo_Ptr, C.Ptrdiff_T (Num_Devices));
      begin
         for Device of Devices loop
            if Is_Real_Keyboard (Device) then
               Grab.Devices.Include (Device);
            end if;
         end loop;
      end;
      XIFreeDeviceInfo (XIDeviceInfo_Ptr);
   end Register_Real_Keyboards;

   procedure Grab_Keys (Grab : in out Grab_Status; Ungrab : Boolean := False) is
      use type Interfaces.Unsigned_8;
      I : C.Int;
   begin
      Grab.Enabled := not Ungrab;
      for Key of Grab.Keys loop
         for Device of Grab.Devices loop
            declare
               Grab_Modifiers : XIGrabModifiers := (0,0);
               Mask : XIEventMask := Key_Event_Mask (Device.Device_ID);
            begin
               if Ungrab then
                  I := XIUngrabKeycode (Grab.Display, Device.Device_ID, C.Int (Key), XDefaultRootWindow (Grab.Display), 1, Grab_Modifiers);
               else
                  I := XIGrabKeycode (Grab.Display, Device.Device_ID, C.Int (Key), XDefaultRootWindow (Grab.Display), XIGrabModeAsync, XIGrabModeAsync, 1, Mask, 1, Grab_Modifiers);
               end if;
               if I /= 0 then
                  raise HARDWARE_PROBLEM with "Unable to establish key grabs, unable to continue";
               end if;
               C.Strings.Free (Mask.Mask);
            end;
         end loop;
      end loop;
   end Grab_Keys;

   function Next_Event (Display : Display_Access; XInput_Opcode : C.Int; Devices : Device_Sets.Set) return Qweyboard.Key_Event is
      function Correct_Device (Event : XIDeviceEvent_Access) return Boolean is
      begin
         for Device of Devices loop
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
      loop
         Log.Chat ("[X11.Input] Trying to get next event from xlib");
         I := XEvent.XNextEvent (Display.all'Address, Any_Event);
         Log.Chat ("[X11.Input] Checking if event is a generic event");
         if Any_Event.C_type = GenericEvent then
            Event_Cookie := Any_Event.XCookie;
            Log.Chat ("[X11.Input] Loading the extra xcookie data");
            I := XEvent.XGetEventData (Display.all'Address, Event_Cookie);
            if I /= 0 and Event_Cookie.Extension = XInput_Opcode then
               Log.Chat ("[X11.Input] discovered xinput2 event, attempting to treat as device event");
               Device_Event := CastDeviceEvent.To_Pointer (Event_Cookie.Data);
               if Device_Event.Evtype = XIKeyPress or Device_Event.Evtype = XIKeyRelease then
                  Log.Chat ("[X11.Input] it is a key event!");
                  if Correct_Device (Device_Event) then
                     Log.Chat ("[X11.Input] device event is from a valid device, returning");
                     declare
                        Key_Event : Qweyboard.Key_Event := Convert_Event (Device_Event);
                     begin
                        XEvent.XFreeEventData (Display.all'Address, Event_Cookie);
                        return Key_Event;
                     end;
                  end if;
               end if;
            end if;
            XEvent.XFreeEventData (Display.all'Address, Event_Cookie);
         end if;
      end loop;
   end;
      
   function Convert_Event (Device_Event : XIDeviceEvent_Access) return Qweyboard.Key_Event is
      use Qweyboard;

      Variant : Key_Event_Variant_Type :=
        (if Device_Event.Evtype = XIKeyPress
           then Key_Press
           else Key_Release);
      Key : Softkey;
   begin
      --  TODO find a way to make this customizeable in a cross-platform way
      if Variant = Key_Press and then Device_Event.Detail = 53 and then Device_Event.Mods.Base = 5 then
         return (SUSP, Variant);
      end if;

      begin
         Key := From_Keycode (Keycode (Device_Event.Detail));
      exception
         when CONSTRAINT_ERROR =>
            return (NOKEY, Variant);
      end;

      --  TODO: call free() on the Device_Event pointer?
      return (Key, Variant);
   end;

   function Key_Event_Mask (Device_Id : C.Int) return XIEventMask is
      use type Interfaces.Unsigned_8;
      Mask_Bytes : C.Char_Array (0..0) :=
        (others => C.To_C (Character'Val (XIKeyPressMask or XIKeyREleaseMask)));
   begin
      return (Device_ID, Mask_Bytes'Length, C.Strings.New_Char_Array (Mask_Bytes));
   end;

begin
   Log.Chat ("[Backend] Setting up keycode table");
   From_Keycode.Insert (10, Qweyboard.NOKEY);
   From_Keycode.Insert (11, Qweyboard.NOKEY);
   From_Keycode.Insert (12, Qweyboard.LP);
   From_Keycode.Insert (13, Qweyboard.LK);
   From_Keycode.Insert (14, Qweyboard.LI);
   From_Keycode.Insert (15, Qweyboard.MAPO);
   From_Keycode.Insert (16, Qweyboard.RO);
   From_Keycode.Insert (17, Qweyboard.RK);
   From_Keycode.Insert (18, Qweyboard.RP);
   From_Keycode.Insert (19, Qweyboard.NOKEY);
   From_Keycode.Insert (20, Qweyboard.NOKEY);
   From_Keycode.Insert (21, Qweyboard.NOKEY);
   From_Keycode.Insert (22, Qweyboard.BS);
   From_Keycode.Insert (24, Qweyboard.NOKEY);
   From_Keycode.Insert (25, Qweyboard.LF);
   From_Keycode.Insert (26, Qweyboard.LT);
   From_Keycode.Insert (27, Qweyboard.LJ);
   From_Keycode.Insert (28, Qweyboard.LO);
   From_Keycode.Insert (29, Qweyboard.MU);
   From_Keycode.Insert (30, Qweyboard.RI);
   From_Keycode.Insert (31, Qweyboard.RJ);
   From_Keycode.Insert (32, Qweyboard.RT);
   From_Keycode.Insert (33, Qweyboard.RF);
   From_Keycode.Insert (34, Qweyboard.NOKEY);
   From_Keycode.Insert (35, Qweyboard.NOKEY);
   From_Keycode.Insert (38, Qweyboard.LZ);
   From_Keycode.Insert (39, Qweyboard.LS);
   From_Keycode.Insert (40, Qweyboard.LC);
   From_Keycode.Insert (41, Qweyboard.LR);
   From_Keycode.Insert (42, Qweyboard.LE);
   From_Keycode.Insert (43, Qweyboard.MA);
   From_Keycode.Insert (44, Qweyboard.RE);
   From_Keycode.Insert (45, Qweyboard.RR);
   From_Keycode.Insert (46, Qweyboard.RC);
   From_Keycode.Insert (47, Qweyboard.RS);
   From_Keycode.Insert (48, Qweyboard.RZ);
   From_Keycode.Insert (49, Qweyboard.NOKEY);
   From_Keycode.Insert (50, Qweyboard.MSHI);
   From_Keycode.Insert (51, Qweyboard.NOKEY);
   From_Keycode.Insert (52, Qweyboard.NOKEY);
   From_Keycode.Insert (53, Qweyboard.NOKEY);
   From_Keycode.Insert (54, Qweyboard.LL);
   From_Keycode.Insert (55, Qweyboard.LN);
   From_Keycode.Insert (56, Qweyboard.MY);
   From_Keycode.Insert (57, Qweyboard.RN);
   From_Keycode.Insert (58, Qweyboard.RL);
   From_Keycode.Insert (59, Qweyboard.NOKEY);
   From_Keycode.Insert (60, Qweyboard.NOKEY);
   From_Keycode.Insert (61, Qweyboard.NOKEY);
   From_Keycode.Insert (62, Qweyboard.MSHI);
   From_Keycode.Insert (65, Qweyboard.NOSP);
end Input_Backend;
