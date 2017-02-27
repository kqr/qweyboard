with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Logging;
private with Output_Backend;

package Qweyboard is
   --  Letter keys declared in the order they're likely to appear in words
   type Softkey is
     (LZ, LS, LF, LC, LT, LP, LL, LR, LJ, LK, LN, LE, LO, LI,
      MY, MA, MU, MAPO,
      RO, RI, RE, RN, RK, RJ, RR, RL, RP, RT, RC, RF, RS, RZ,
      --  Symbol keys declared afterward
      LCOM, RTIC,
      --  Special keys that exist on the real hardware
      MSHI, CAPI, NOSP,
      --  Special keys we want (backspace, suspend and "no modifier pressed")
      BS, SUSP, NOKEY);

   package Key_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Softkey);
   
   type Layout is private;
   procedure Add_Key (To_Layout : in out Layout; Modifier : Softkey; Key : Softkey; Letter : Character; Replace : Boolean := False);

   type Key_Event_Variant_Type is (Key_Press, Key_Release);
   type Key_Event is record
      Key : Softkey;
      Key_Event_Variant : Key_Event_Variant_Type;
   end record;
   
   type Output_Variant is (Nothing, Syllable, Erase);
   type Output (Variant : Output_Variant := Nothing) is record
      case Variant is
         when Nothing =>
            null;
         when Syllable =>
            Text : Unbounded_String;
            Completes_Word : Boolean;
         when Erase =>
            Amount : Positive;
      end case;
   end record;

   task Softboard is
      entry Ready_Wait;
      entry Set_Timeout (Timeout_Amount : Duration);
      entry Set_Layout (User_Layout : Layout);
      entry Handle (Event : Key_Event);
      entry Shut_Down;
   end Softboard;
   
private
   use Logging;

   procedure Log_Board (Pressed : Key_Sets.Set; Released : Key_Sets.Set);

   package Key_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Softkey);
   package Layer_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Character);
   package Layout_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Layer_Maps.Map, "=" => Layer_Maps."=");

   type Layout is record
      --  This is a vector because we want the order they're declared in to matter...
      --  (except, of course, NOKEY which gets last dibs always)
      Modifiers : Key_Vectors.Vector;
      Layers : Layout_Maps.Map;
   end record;

   function Virtual_Layer (User_Layout : Layout; Pressed : Key_Sets.Set) return Layer_Maps.Map;
   function Mod_Layer (User_Layout : Layout; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map;
end Qweyboard;
