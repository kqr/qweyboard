package body Languages is
   Standard_Language : Language;

   function Decode (User_Language : Language; Released : Key_Sets.Set) return Unbounded_String is
      Ret : Unbounded_String;
   begin
      --  As if by accident, the declaration order of Softkey in conjunction with
      --  the implementation of Ordered_(Sets|Maps) means this iteration happens
      --  in precisely the order we need it to...
      for C in Virtual_Layer (User_Language.Layout, Released).Iterate loop
         Ret := Ret & Layer_Maps.Element (C);
      end loop;
      return Ret;
   end Decode;

   function Parse (Source : Ada.Text_IO.File_Type) return Language is
   begin
      --  check if null string, then standard language
      --  if To_String (Config.Layout_File_Name) = "swedish.layout" then
      --     Add_Key (Config.Layout, NOKEY, LI, 'Å', Replace => True);
      --     Add_Key (Config.Layout, NOKEY, LO, 'Ä', Replace => True);
      --     Add_Key (Config.Layout, NOKEY, LE, 'Ö', Replace => True);
      --  end if;
      return Standard_Language;
   end Parse;

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

   procedure Add_Key (Layout : in out Layout_Type; Modifier : Softkey; Key : Softkey; Letter : Character; Replace : Boolean := False) is
      use Ada.Characters.Handling;
   begin
      if not Layout.Layers.Contains (Modifier) then
         if Modifier /= NOKEY then
            Layout.Modifiers.Append (Modifier);
         end if;
         Layout.Layers.Insert (Modifier, Layer_Maps.Empty_Map);
      end if;
      if Replace then
         Layer_Maps.Replace (Layout.Layers (Modifier), Key, To_Lower (Letter));
      else
         Layout.Layers (Modifier).Insert (Key, To_Lower (Letter));
      end if;
   end Add_Key;
begin
   Add_Key (Standard_Language.Layout, NOKEY, LZ, 'Z');
   Add_Key (Standard_Language.Layout, NOKEY, LF, 'F');
   Add_Key (Standard_Language.Layout, NOKEY, LS, 'S');
   Add_Key (Standard_Language.Layout, NOKEY, LP, 'P');
   Add_Key (Standard_Language.Layout, NOKEY, LT, 'T');
   Add_Key (Standard_Language.Layout, NOKEY, LC, 'C');
   Add_Key (Standard_Language.Layout, NOKEY, LK, 'K');
   Add_Key (Standard_Language.Layout, NOKEY, LJ, 'J');
   Add_Key (Standard_Language.Layout, NOKEY, LR, 'R');
   Add_Key (Standard_Language.Layout, NOKEY, LL, 'L');
   Add_Key (Standard_Language.Layout, NOKEY, LI, 'I');
   Add_Key (Standard_Language.Layout, NOKEY, LO, 'O');
   Add_Key (Standard_Language.Layout, NOKEY, LE, 'E');
   Add_Key (Standard_Language.Layout, NOKEY, LN, 'N');
   Add_Key (Standard_Language.Layout, NOKEY, MAPO, ''');
   Add_Key (Standard_Language.Layout, NOKEY, MU, 'U');
   Add_Key (Standard_Language.Layout, NOKEY, MA, 'A');
   Add_Key (Standard_Language.Layout, NOKEY, MY, 'Y');
   Add_Key (Standard_Language.Layout, NOKEY, RO, 'O');
   Add_Key (Standard_Language.Layout, NOKEY, RI, 'I');
   Add_Key (Standard_Language.Layout, NOKEY, RE, 'E');
   Add_Key (Standard_Language.Layout, NOKEY, RN, 'N');
   Add_Key (Standard_Language.Layout, NOKEY, RK, 'K');
   Add_Key (Standard_Language.Layout, NOKEY, RJ, 'J');
   Add_Key (Standard_Language.Layout, NOKEY, RR, 'R');
   Add_Key (Standard_Language.Layout, NOKEY, RL, 'L');
   Add_Key (Standard_Language.Layout, NOKEY, RP, 'P');
   Add_Key (Standard_Language.Layout, NOKEY, RT, 'T');
   Add_Key (Standard_Language.Layout, NOKEY, RC, 'C');
   Add_Key (Standard_Language.Layout, NOKEY, RF, 'F');
   Add_Key (Standard_Language.Layout, NOKEY, RS, 'S');
   Add_Key (Standard_Language.Layout, NOKEY, RZ, 'Z');
   Add_Key (Standard_Language.Layout, LO, RI, 'A');
   Add_Key (Standard_Language.Layout, LJ, LF, 'V');
   Add_Key (Standard_Language.Layout, LJ, LP, 'B');
   Add_Key (Standard_Language.Layout, LJ, LT, 'D');
   Add_Key (Standard_Language.Layout, LJ, LC, 'G');
   Add_Key (Standard_Language.Layout, LJ, LL, 'H');
   Add_Key (Standard_Language.Layout, LJ, LN, 'W');
   Add_Key (Standard_Language.Layout, RJ, RF, 'V');
   Add_Key (Standard_Language.Layout, RJ, RN, 'W');
   Add_Key (Standard_Language.Layout, RJ, RL, 'H');
   Add_Key (Standard_Language.Layout, RJ, RP, 'B');
   Add_Key (Standard_Language.Layout, RJ, RT, 'D');
   Add_Key (Standard_Language.Layout, RJ, RC, 'G');
   Add_Key (Standard_Language.Layout, LR, LL, 'V');
   Add_Key (Standard_Language.Layout, LR, LN, 'M');
   Add_Key (Standard_Language.Layout, RR, RN, 'M');
   Add_Key (Standard_Language.Layout, RR, RL, 'V');
   Add_Key (Standard_Language.Layout, LC, LF, 'Q');
   Add_Key (Standard_Language.Layout, RC, RF, 'Q');
   Add_Key (Standard_Language.Layout, LK, LZ, 'X');
   Add_Key (Standard_Language.Layout, RK, RZ, 'X');
   Add_Key (Standard_Language.Layout, MSHI, LS, '$');
   Add_Key (Standard_Language.Layout, MSHI, LP, '%');
   Add_Key (Standard_Language.Layout, MSHI, LT, '/');
   Add_Key (Standard_Language.Layout, MSHI, LC, '(');
   Add_Key (Standard_Language.Layout, MSHI, LK, '&');
   Add_Key (Standard_Language.Layout, MSHI, LJ, '*');
   Add_Key (Standard_Language.Layout, MSHI, LR, '+');
   Add_Key (Standard_Language.Layout, MSHI, LI, '7');
   Add_Key (Standard_Language.Layout, MSHI, LO, '4');
   Add_Key (Standard_Language.Layout, MSHI, LE, '1');
   Add_Key (Standard_Language.Layout, MSHI, MAPO, '8');
   Add_Key (Standard_Language.Layout, MSHI, MU, '5');
   Add_Key (Standard_Language.Layout, MSHI, MA, '2');
   Add_Key (Standard_Language.Layout, MSHI, MY, '0');
   Add_Key (Standard_Language.Layout, MSHI, RO, '9');
   Add_Key (Standard_Language.Layout, MSHI, RI, '6');
   Add_Key (Standard_Language.Layout, MSHI, RE, '3');
   Add_Key (Standard_Language.Layout, MSHI, RK, '?');
   Add_Key (Standard_Language.Layout, MSHI, RJ, '=');
   Add_Key (Standard_Language.Layout, MSHI, RR, '-');
   Add_Key (Standard_Language.Layout, MSHI, RP, '!');
   Add_Key (Standard_Language.Layout, MSHI, RT, ';');
   Add_Key (Standard_Language.Layout, MSHI, RC, ')');
   Add_Key (Standard_Language.Layout, MSHI, RF, '"');
   Add_Key (Standard_Language.Layout, MSHI, RS, ':');
end Languages;
