with Ada.Text_IO;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Ordered_Maps;

package Qweyboard is
   type Softkey is
     (LZ, LF, LS, LP, LT, LC, LK, LJ, LR, LL, LI, LO, LE, LN,
      MU, MA, MY,
      RN, RE, RI, RO, RL, RR, RJ, RK, RC, RT, RP, RS, RF, RZ,
      MAPO, LCOM, RTIC,
      MSHI, CAPI, NOSP);
   
   type Language_Variant is (Standard, Swedish);
  
   type Key_Event_Variant_Type is (Key_Press, Key_Release);
   type Key_Event is record
      Key : Softkey;
      Key_Event_Variant : Key_Event_Variant_Type;
   end record;
   
   type Softboard is Private;

   function Make_Softboard (Language : Language_Variant) return Softboard;
   procedure Handle (Board : in out Softboard; Event : Key_Event);
   function Timeout (Board : in out Softboard) return String;
private
   package Key_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Softkey);
   package Key_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Character);
   
   type Key_Layer_Type is (Normal, Left, Middle, Right, J, R);
   type Key_Layer_Array is array (Key_Layer_Type) of Key_Maps.Map;

   Layer : Key_Layer_Array;

   type Softboard is record
      Pressed : Key_Sets.Set;
      Released : Key_Sets.Set;
   end record;
   
   function Union (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map;
   function Intersection (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map;
   function Difference (A : Key_Maps.Map; B : Key_Maps.Map) return Key_Maps.Map;

   function Mod_Layer (Final_Presses : Key_Maps.Map; Key : Softkey) return Key_Maps.Map;
   procedure Current_Mod_Map (Final_Presses : in out Key_Maps.Map; Key : Softkey);
   function Virtual_Layer (Pressed : Key_Sets.Set) return Key_Maps.Map;
   function Apply (Pressed : Key_Sets.Set) return String;
end Qweyboard;
