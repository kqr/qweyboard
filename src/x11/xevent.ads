with System;
with Interfaces.C;
with System.Address_To_Access_Conversions;

package XEvent is
   package C renames Interfaces.C;

   type XKeyEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      root : aliased C.Unsigned_Long;
      subwindow : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      x_root : aliased C.Int;
      y_root : aliased C.Int;
      state : aliased C.Unsigned;
      keycode : aliased C.Unsigned;
      same_screen : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XKeyEvent);

   subtype XKeyPressedEvent is XKeyEvent;

   subtype XKeyReleasedEvent is XKeyEvent;

   type XButtonEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      root : aliased C.Unsigned_Long;
      subwindow : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      x_root : aliased C.Int;
      y_root : aliased C.Int;
      state : aliased C.Unsigned;
      button : aliased C.Unsigned;
      same_screen : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XButtonEvent);

   subtype XButtonPressedEvent is XButtonEvent;

   subtype XButtonReleasedEvent is XButtonEvent;

   type XMotionEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      root : aliased C.Unsigned_Long;
      subwindow : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      x_root : aliased C.Int;
      y_root : aliased C.Int;
      state : aliased C.Unsigned;
      is_hint : aliased C.char;
      same_screen : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XMotionEvent);

   subtype XPointerMovedEvent is XMotionEvent;

   type XCrossingEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      root : aliased C.Unsigned_Long;
      subwindow : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      x_root : aliased C.Int;
      y_root : aliased C.Int;
      mode : aliased C.Int;
      detail : aliased C.Int;
      same_screen : aliased C.Int;
      focus : aliased C.Int;
      state : aliased C.Unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, XCrossingEvent);

   subtype XEnterWindowEvent is XCrossingEvent;

   subtype XLeaveWindowEvent is XCrossingEvent;

   type XFocusChangeEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      mode : aliased C.Int;
      detail : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XFocusChangeEvent);

   subtype XFocusInEvent is XFocusChangeEvent;

   subtype XFocusOutEvent is XFocusChangeEvent;

   subtype XKeymapEvent_key_vector_array is Interfaces.C.char_array (0 .. 31);
   type XKeymapEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      key_vector : aliased XKeymapEvent_key_vector_array;
   end record;
   pragma Convention (C_Pass_By_Copy, XKeymapEvent);

   type XExposeEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      width : aliased C.Int;
      height : aliased C.Int;
      count : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XExposeEvent);

   type XGraphicsExposeEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_drawable : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      width : aliased C.Int;
      height : aliased C.Int;
      count : aliased C.Int;
      major_code : aliased C.Int;
      minor_code : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XGraphicsExposeEvent);

   type XNoExposeEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_drawable : aliased C.Unsigned_Long;
      major_code : aliased C.Int;
      minor_code : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XNoExposeEvent);

   type XVisibilityEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      state : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XVisibilityEvent);

   type XCreateWindowEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      parent : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      width : aliased C.Int;
      height : aliased C.Int;
      border_width : aliased C.Int;
      override_redirect : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XCreateWindowEvent);

   type XDestroyWindowEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XDestroyWindowEvent);

   type XUnmapEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      from_configure : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XUnmapEvent);

   type XMapEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      override_redirect : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XMapEvent);

   type XMapRequestEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      parent : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XMapRequestEvent);

   type XReparentEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      parent : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      override_redirect : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XReparentEvent);

   type XConfigureEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      width : aliased C.Int;
      height : aliased C.Int;
      border_width : aliased C.Int;
      above : aliased C.Unsigned_Long;
      override_redirect : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XConfigureEvent);

   type XGravityEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XGravityEvent);

   type XResizeRequestEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      width : aliased C.Int;
      height : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XResizeRequestEvent);

   type XConfigureRequestEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      parent : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      x : aliased C.Int;
      y : aliased C.Int;
      width : aliased C.Int;
      height : aliased C.Int;
      border_width : aliased C.Int;
      above : aliased C.Unsigned_Long;
      detail : aliased C.Int;
      value_mask : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XConfigureRequestEvent);

   type XCirculateEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      event : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      place : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XCirculateEvent);

   type XCirculateRequestEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      parent : aliased C.Unsigned_Long;
      the_window : aliased C.Unsigned_Long;
      place : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XCirculateRequestEvent);

   type XPropertyEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      the_atom : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
      state : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XPropertyEvent);

   type XSelectionClearEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      selection : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XSelectionClearEvent);

   type XSelectionRequestEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      owner : aliased C.Unsigned_Long;
      requestor : aliased C.Unsigned_Long;
      selection : aliased C.Unsigned_Long;
      target : aliased C.Unsigned_Long;
      property : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XSelectionRequestEvent);

   type XSelectionEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      requestor : aliased C.Unsigned_Long;
      selection : aliased C.Unsigned_Long;
      target : aliased C.Unsigned_Long;
      property : aliased C.Unsigned_Long;
      the_time : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XSelectionEvent);

   type XColormapEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      the_colormap : aliased C.Unsigned_Long;
      c_new : aliased C.Int;
      state : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XColormapEvent);

   subtype anon1357_b_array is Interfaces.C.char_array (0 .. 19);
   type anon1357_s_array is array (0 .. 9) of aliased C.short;
   type anon1357_l_array is array (0 .. 4) of aliased C.long;
   type anon_1357 (discr : C.Unsigned := 0) is record
      case discr is
         when 0 =>
            b : aliased anon1357_b_array;
         when 1 =>
            s : aliased anon1357_s_array;
         when others =>
            l : aliased anon1357_l_array;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_1357);
   pragma Unchecked_Union (anon_1357);

   subtype XClientMessageEvent_b_array is Interfaces.C.char_array (0 .. 19);
   type XClientMessageEvent_s_array is array (0 .. 9) of aliased C.short;
   type XClientMessageEvent_l_array is array (0 .. 4) of aliased C.long;
   type XClientMessageEvent_data_union (discr : C.Unsigned := 0) is record
      case discr is
         when 0 =>
            b : aliased XClientMessageEvent_b_array;
         when 1 =>
            s : aliased XClientMessageEvent_s_array;
         when others =>
            l : aliased XClientMessageEvent_l_array;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, XClientMessageEvent_data_union);
   pragma Unchecked_Union (XClientMessageEvent_data_union);
   type XClientMessageEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      message_type : aliased C.Unsigned_Long;
      format : aliased C.Int;
      data : XClientMessageEvent_data_union;
   end record;
   pragma Convention (C_Pass_By_Copy, XClientMessageEvent);

   type XMappingEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
      request : aliased C.Int;
      first_keycode : aliased C.Int;
      count : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XMappingEvent);

   type XErrorEvent is record
      c_type : aliased C.Int;
      the_display : System.Address;
      resourceid : aliased C.Unsigned_Long;
      serial : aliased C.Unsigned_Long;
      error_code : aliased C.Unsigned_char;
      request_code : aliased C.Unsigned_char;
      minor_code : aliased C.Unsigned_char;
   end record;
   pragma Convention (C_Pass_By_Copy, XErrorEvent);

   type XAnyEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      the_window : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XAnyEvent);

   type XGenericEvent is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      extension : aliased C.Int;
      evtype : aliased C.Int;
   end record;
   pragma Convention (C_Pass_By_Copy, XGenericEvent);

   type XGenericEventCookie is record
      c_type : aliased C.Int;
      serial : aliased C.Unsigned_Long;
      send_event : aliased C.Int;
      the_display : System.Address;
      extension : aliased C.Int;
      evtype : aliased C.Int;
      cookie : aliased C.Unsigned;
      data : aliased System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, XGenericEventCookie);

   type anon1376_pad_array is array (0 .. 23) of aliased C.long;
   type u_XEvent (discr : C.Unsigned := 0) is record
      case discr is
         when 0 =>
            c_type : aliased C.Int;
         when 1 =>
            xany : aliased XAnyEvent;
         when 2 =>
            xkey : aliased XKeyEvent;
         when 3 =>
            xbutton : aliased XButtonEvent;
         when 4 =>
            xmotion : aliased XMotionEvent;
         when 5 =>
            xcrossing : aliased XCrossingEvent;
         when 6 =>
            xfocus : aliased XFocusChangeEvent;
         when 7 =>
            xexpose : aliased XExposeEvent;
         when 8 =>
            xgraphicsexpose : aliased XGraphicsExposeEvent;
         when 9 =>
            xnoexpose : aliased XNoExposeEvent;
         when 10 =>
            xvisibility : aliased XVisibilityEvent;
         when 11 =>
            xcreatewindow : aliased XCreateWindowEvent;
         when 12 =>
            xdestroywindow : aliased XDestroyWindowEvent;
         when 13 =>
            xunmap : aliased XUnmapEvent;
         when 14 =>
            xmap : aliased XMapEvent;
         when 15 =>
            xmaprequest : aliased XMapRequestEvent;
         when 16 =>
            xreparent : aliased XReparentEvent;
         when 17 =>
            xconfigure : aliased XConfigureEvent;
         when 18 =>
            xgravity : aliased XGravityEvent;
         when 19 =>
            xresizerequest : aliased XResizeRequestEvent;
         when 20 =>
            xconfigurerequest : aliased XConfigureRequestEvent;
         when 21 =>
            xcirculate : aliased XCirculateEvent;
         when 22 =>
            xcirculaterequest : aliased XCirculateRequestEvent;
         when 23 =>
            xproperty : aliased XPropertyEvent;
         when 24 =>
            xselectionclear : aliased XSelectionClearEvent;
         when 25 =>
            xselectionrequest : aliased XSelectionRequestEvent;
         when 26 =>
            xselection : aliased XSelectionEvent;
         when 27 =>
            xcolormap : aliased XColormapEvent;
         when 28 =>
            xclient : aliased XClientMessageEvent;
         when 29 =>
            xmapping : aliased XMappingEvent;
         when 30 =>
            xerror : aliased XErrorEvent;
         when 31 =>
            xkeymap : aliased XKeymapEvent;
         when 32 =>
            xgeneric : aliased XGenericEvent;
         when 33 =>
            xcookie : aliased XGenericEventCookie;
         when others =>
            pad : aliased anon1376_pad_array;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, u_XEvent);
   pragma Unchecked_Union (u_XEvent);

   subtype Event is U_XEvent;
   
   type XIEvent is record
      c_type : aliased C.int;
      serial : aliased C.unsigned_long;
      send_event : aliased C.int;
      the_display : System.Address;
      extension : aliased C.int;
      evtype : aliased C.int;
      the_time : aliased C.Unsigned_Long;
   end record;
   pragma Convention (C_Pass_By_Copy, XIEvent);
   
    type XIButtonState is record
      mask_len : aliased C.int;
      mask : access C.unsigned_char;
   end record;
   pragma Convention (C_Pass_By_Copy, XIButtonState);

    type XIValuatorState is record
      mask_len : aliased C.int;
      mask : access C.unsigned_char;
      values : access C.double;
   end record;
   pragma Convention (C_Pass_By_Copy, XIValuatorState);    

  type XIModifierState is record
      base : aliased C.int;
      latched : aliased C.int;
      locked : aliased C.int;
      effective : aliased C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, XIModifierState);

   type XIDeviceEvent is record
      c_type : aliased C.int;
      serial : aliased C.unsigned_long;
      send_event : aliased C.int;
      the_display : System.Address;
      extension : aliased C.int;
      evtype : aliased C.int;
      the_time : aliased C.Unsigned_Long;
      deviceid : aliased C.int;
      sourceid : aliased C.int;
      detail : aliased C.int;
      root : aliased C.Unsigned_Long;
      event : aliased C.Unsigned_Long;
      child : aliased C.Unsigned_Long;
      root_x : aliased C.double;
      root_y : aliased C.double;
      event_x : aliased C.double;
      event_y : aliased C.double;
      flags : aliased C.int;
      buttons : aliased XIButtonState;
      valuators : aliased XIValuatorState;
      mods : aliased XIModifierState;
      group : aliased XIModifierState;
   end record;
   pragma Convention (C_Pass_By_Copy, XIDeviceEvent);

   function XNextEvent (Display : System.Address; Any_Event : in out U_XEvent) return C.Int;
   pragma Import (C, XNextEvent, "XNextEvent");
   
   function XGetEventData (Display : System.Address; Cookie : in out XGenericEventCookie) return C.Int;
   pragma Import (C, XGetEventData, "XGetEventData");

   package CastDeviceEvent is new System.Address_To_Access_Conversions (Object => XIDeviceEvent);
   subtype XIDeviceEvent_Access is CastDeviceEvent.Object_Pointer;
end XEvent;
