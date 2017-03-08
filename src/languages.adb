package body Languages is
   protected body User_Language is
      function Decode (Released : Key_Sets.Set) return Unbounded_Wide_Wide_String is
         Init : Unbounded_Wide_Wide_String;
         Vowels : Unbounded_Wide_Wide_String;
         Tail : Unbounded_Wide_Wide_String;
      begin
         --  As if by accident, the declaration order of Softkey in conjunction with
         --  the implementation of Ordered_(Sets|Maps) means this iteration happens
         --  in precisely the order we need it to...

         for C in Virtual_Layer (Current_Layout, Released).Iterate loop
            if Layer_Maps.Key (C) < MY then
               Init := Init & Layer_maps.Element (C);
            elsif Layer_Maps.Key (C) < RN then
               Vowels := Vowels & Layer_maps.Element (C);
            elsif Layer_Maps.Key (C) < MSHI then
               Tail := Tail & Layer_maps.Element (C);
            end if;
         end loop;
         
         return
           Perform_Substitutions (Init, Substitutions (Left)) &
           Perform_Substitutions (Vowels, Substitutions (Middle)) &
           Perform_Substitutions (Tail, Substitutions (Right));
      end Decode;

      procedure Add_Key (Modifier : Softkey; Key : Softkey; Symbol : Wide_Wide_Character) is
         use Ada.Characters.Handling;
      begin
         if not Current_Layout.Layers.Contains (Modifier) then
            if Modifier /= NOKEY then
               Current_Layout.Modifiers.Append (Modifier);
            end if;
            Current_Layout.Layers.Insert (Modifier, Layer_Maps.Empty_Map);
         end if;
         Layer_Maps.Include (Current_Layout.Layers (Modifier), Key, Symbol);
      end Add_Key;

      procedure Add_Substitution (Position : Substitution_Type; Pattern : Unbounded_Wide_Wide_String; Replacement : Unbounded_Wide_Wide_String) is
      begin
         Substitution_Maps.Include (Substitutions (Position), Pattern, Replacement);
      end Add_Substitution;
   
      function Virtual_Layer (Layout : Layout_Type; Pressed : Key_Sets.Set) return Layer_Maps.Map is
         Final_Presses : Layer_Maps.Map;
         Already_Handled : Key_Sets.Set;
         
         procedure Handle_Modifier (Modifier : Softkey) is
            Layer : Layer_Maps.Map := Mod_Layer (Layout, Modifier, Key_Sets.Difference (Pressed, Already_Handled));
         begin
            if not Layer.Is_Empty then
               Already_Handled.Insert (Modifier);
            end if;
            for C in Layer.Iterate loop
               Already_Handled.Insert (Layer_Maps.Key (C));
               Final_Presses.Insert (Layer_Maps.Key (C), Layer_Maps.Element (C));
            end loop;
         end Handle_Modifier;
      begin
         for Modifier of Layout.Modifiers loop
            Handle_Modifier (Modifier);
         end loop;
         Handle_Modifier (NOKEY);
         
         return Final_Presses;
      end Virtual_Layer;

      function Mod_Layer (Layout : Layout_Type; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map is
         Layer : Layer_Maps.Map;
      begin
         if Modifier = NOKEY or Pressed.Contains (Modifier) then
            for Key of Pressed loop
               if Layout.Layers (Modifier).Contains (Key) then
                  Layer.Insert (Key, Layout.Layers (Modifier) (Key));
               end if;
            end loop;
         end if;
         return Layer;
      end Mod_Layer;

      function Perform_Substitutions (Text : Unbounded_Wide_Wide_String; From : Substitution_Maps.Map) return Unbounded_Wide_Wide_String is
         Ret : Unbounded_Wide_Wide_String := Text;

         function Substitute_Slice (Low, High : Positive) return Boolean is
            Pattern : Unbounded_Wide_Wide_String := Unbounded_Slice (Ret, Low, High);
         begin
            if From.Contains (Pattern) then
               Replace_Slice (Ret, Low, High, From_Unbounded (From (Pattern)));
               return True;
            else
               return False;
            end if;
         end Substitute_Slice;

         Substitution_Performed : Boolean;
      begin
         --  This entire thing is a bit iffy because it's not at all clear in
         --  which order substitutions should be performed. It seems from a
         --  grammatic argument "obvious" that substitutions should be
         --  performed iteratively, i.e. if one substitution results in a
         --  pattern for another substitution, they should both be performed.
         --  However, ensuring this happens in the correct order at all times
         --  is not a trivial problem, and much analysis is required. I defer
         --  this concern to a later time, and just pick an arbitrary order
         --  (left to right, long to short) for the time being.
         loop
            Substitution_Performed := False;
            for Low in 1 .. Length (Ret) loop
               for High in reverse Low .. Length (Ret) loop
                  if Substitute_Slice (Low, High) then
                     Substitution_Performed := True;
                  end if;
               end loop;
            end loop;
            exit when not Substitution_Performed;
         end loop;
         return Ret;
      end Perform_Substitutions;
   end User_Language;
begin
   User_Language.Add_Key (NOKEY, LZ, 'Z');
   User_Language.Add_Key (NOKEY, LF, 'F');
   User_Language.Add_Key (NOKEY, LS, 'S');
   User_Language.Add_Key (NOKEY, LP, 'P');
   User_Language.Add_Key (NOKEY, LT, 'T');
   User_Language.Add_Key (NOKEY, LC, 'C');
   User_Language.Add_Key (NOKEY, LK, 'K');
   User_Language.Add_Key (NOKEY, LJ, 'J');
   User_Language.Add_Key (NOKEY, LR, 'R');
   User_Language.Add_Key (NOKEY, LL, 'L');
   User_Language.Add_Key (NOKEY, LI, 'I');
   User_Language.Add_Key (NOKEY, LO, 'O');
   User_Language.Add_Key (NOKEY, LE, 'E');
   User_Language.Add_Key (NOKEY, LN, 'N');
   User_Language.Add_Key (NOKEY, MAPO, ''');
   User_Language.Add_Key (NOKEY, MU, 'U');
   User_Language.Add_Key (NOKEY, MA, 'A');
   User_Language.Add_Key (NOKEY, MY, 'Y');
   User_Language.Add_Key (NOKEY, RO, 'O');
   User_Language.Add_Key (NOKEY, RI, 'I');
   User_Language.Add_Key (NOKEY, RE, 'E');
   User_Language.Add_Key (NOKEY, RN, 'N');
   User_Language.Add_Key (NOKEY, RK, 'K');
   User_Language.Add_Key (NOKEY, RJ, 'J');
   User_Language.Add_Key (NOKEY, RR, 'R');
   User_Language.Add_Key (NOKEY, RL, 'L');
   User_Language.Add_Key (NOKEY, RP, 'P');
   User_Language.Add_Key (NOKEY, RT, 'T');
   User_Language.Add_Key (NOKEY, RC, 'C');
   User_Language.Add_Key (NOKEY, RF, 'F');
   User_Language.Add_Key (NOKEY, RS, 'S');
   User_Language.Add_Key (NOKEY, RZ, 'Z');
   User_Language.Add_Key (LO, RI, 'A');
   User_Language.Add_Key (LJ, LF, 'V');
   User_Language.Add_Key (LJ, LP, 'B');
   User_Language.Add_Key (LJ, LT, 'D');
   User_Language.Add_Key (LJ, LC, 'G');
   User_Language.Add_Key (LJ, LL, 'H');
   User_Language.Add_Key (LJ, LN, 'W');
   User_Language.Add_Key (RJ, RF, 'V');
   User_Language.Add_Key (RJ, RN, 'W');
   User_Language.Add_Key (RJ, RL, 'H');
   User_Language.Add_Key (RJ, RP, 'B');
   User_Language.Add_Key (RJ, RT, 'D');
   User_Language.Add_Key (RJ, RC, 'G');
   User_Language.Add_Key (LR, LL, 'V');
   User_Language.Add_Key (LR, LN, 'M');
   User_Language.Add_Key (RR, RN, 'M');
   User_Language.Add_Key (RR, RL, 'V');
   User_Language.Add_Key (LC, LF, 'Q');
   User_Language.Add_Key (RC, RF, 'Q');
   User_Language.Add_Key (LK, LZ, 'X');
   User_Language.Add_Key (RK, RZ, 'X');
   User_Language.Add_Key (MSHI, LS, '$');
   User_Language.Add_Key (MSHI, LP, '%');
   User_Language.Add_Key (MSHI, LT, '/');
   User_Language.Add_Key (MSHI, LC, '(');
   User_Language.Add_Key (MSHI, LK, '&');
   User_Language.Add_Key (MSHI, LJ, '*');
   User_Language.Add_Key (MSHI, LR, '+');
   User_Language.Add_Key (MSHI, LI, '7');
   User_Language.Add_Key (MSHI, LO, '4');
   User_Language.Add_Key (MSHI, LE, '1');
   User_Language.Add_Key (MSHI, MAPO, '8');
   User_Language.Add_Key (MSHI, MU, '5');
   User_Language.Add_Key (MSHI, MA, '2');
   User_Language.Add_Key (MSHI, MY, '0');
   User_Language.Add_Key (MSHI, RO, '9');
   User_Language.Add_Key (MSHI, RI, '6');
   User_Language.Add_Key (MSHI, RE, '3');
   User_Language.Add_Key (MSHI, RK, '?');
   User_Language.Add_Key (MSHI, RJ, '=');
   User_Language.Add_Key (MSHI, RR, '-');
   User_Language.Add_Key (MSHI, RP, '!');
   User_Language.Add_Key (MSHI, RT, ';');
   User_Language.Add_Key (MSHI, RC, ')');
   User_Language.Add_Key (MSHI, RF, '"');
   User_Language.Add_Key (MSHI, RS, ':');
   User_Language.Add_Substitution (Left, To_Unbounded ("BN"), To_Unbounded ("BR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("GN"), To_Unbounded ("GR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("DN"), To_Unbounded ("DR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("VN"), To_Unbounded ("VR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("PK"), To_Unbounded ("PR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("TK"), To_Unbounded ("TR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("FK"), To_Unbounded ("FR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("CK"), To_Unbounded ("KR"));
   User_Language.Add_Substitution (Left, To_Unbounded ("ZP"), To_Unbounded ("PS"));
   User_Language.Add_Substitution (Left, To_Unbounded ("ZT"), To_Unbounded ("TS"));
   User_Language.Add_Substitution (Left, To_Unbounded ("CW"), To_Unbounded ("QU"));
   User_Language.Add_Substitution (Left, To_Unbounded ("DH"), To_Unbounded ("TH"));
   User_Language.Add_Substitution (Left, To_Unbounded ("GH"), To_Unbounded ("CH"));
   User_Language.Add_Substitution (Right, To_Unbounded ("SZ"), To_Unbounded ("STS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("YZ"), To_Unbounded ("YS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("NZ"), To_Unbounded ("NS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("DZ"), To_Unbounded ("DS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("MZ"), To_Unbounded ("MS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("TZ"), To_Unbounded ("TS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("SJ"), To_Unbounded ("SS"));
   User_Language.Add_Substitution (Right, To_Unbounded ("CZ"), To_Unbounded ("CH"));
   User_Language.Add_Substitution (Right, To_Unbounded ("CS"), To_Unbounded ("SH"));
   User_Language.Add_Substitution (Right, To_Unbounded ("DZ"), To_Unbounded ("TH"));
   User_Language.Add_Substitution (Right, To_Unbounded ("BF"), To_Unbounded ("BE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("CF"), To_Unbounded ("CE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("DF"), To_Unbounded ("DE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("GF"), To_Unbounded ("GE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("KF"), To_Unbounded ("KE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("LF"), To_Unbounded ("LE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("MF"), To_Unbounded ("ME"));
   User_Language.Add_Substitution (Right, To_Unbounded ("NF"), To_Unbounded ("NE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("PF"), To_Unbounded ("PE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("RF"), To_Unbounded ("RE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("SF"), To_Unbounded ("SE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("TF"), To_Unbounded ("TE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("VF"), To_Unbounded ("VE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("WF"), To_Unbounded ("WE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("XF"), To_Unbounded ("XE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("ZF"), To_Unbounded ("ZE"));
   User_Language.Add_Substitution (Right, To_Unbounded ("KP"), To_Unbounded ("RP"));
   User_Language.Add_Substitution (Right, To_Unbounded ("KC"), To_Unbounded ("CK"));
   User_Language.Add_Substitution (Right, To_Unbounded ("GZ"), To_Unbounded ("GT"));
   User_Language.Add_Substitution (Right, To_Unbounded ("PZ"), To_Unbounded ("PT"));
   User_Language.Add_Substitution (Right, To_Unbounded ("FZ"), To_Unbounded ("FT"));
   User_Language.Add_Substitution (Right, To_Unbounded ("NC"), To_Unbounded ("GN"));
   User_Language.Add_Substitution (Right, To_Unbounded ("WD"), To_Unbounded ("ND"));
   User_Language.Add_Substitution (Right, To_Unbounded ("MV"), To_Unbounded ("LM"));
end Languages;
