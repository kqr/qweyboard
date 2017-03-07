with Ada.Containers.Ordered_Sets;

package Keys is
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
end Keys;
   
