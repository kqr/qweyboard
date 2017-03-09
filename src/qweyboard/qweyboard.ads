with String_Helpers; use String_Helpers;
with Ada.Containers.Ordered_Sets;

package Qweyboard is
   use Unbounded;

   --  Letter keys declared in the order they're likely to appear in words
   --
   --  Quoted from patent, although not strictly followed here...
   --
   --  >  For example, in English, the initial consonants of the syllable are
   --  >  ordered according to the following priority scheme or rule: "X, Z, S, B,
   --  >  P, G, QU, Q, C, D, T, V, W, F, J, H, K, L, M, R, N". The final consonants
   --  >  are sorted according to the following priority rules: "V, L, M, R, W, N,
   --  >  G, K, C, X, B, P, H, D, S, E, F, T, Y, Z", where the E is meant as a final
   --  >  mute "E" at the end of word

   type Softkey is
     (LZ, LS, LF, LC, LT, LP, LR, LJ, LK, LL, LN,
      LE, LO, LI, MY, MA, MU, MAPO, RO, RI, RE,
      RN, RL, RK, RJ, RR, RP, RT, RC, RF, RS, RZ,
      --  Special keys that exist on the real hardware
      MSHI, CAPI, NOSP,
      --  Special keys we want (backspace, suspend and "no modifier pressed")
      SUSP, NOKEY);

   package Key_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Softkey);

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
            Text : Unbounded_Wide_Wide_String;
            Continues_Word : Boolean;
         when Erase =>
            Amount : Positive;
      end case;
   end record;
end Qweyboard;
