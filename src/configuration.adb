package body Configuration is
   procedure Get_Settings (Config : out Settings) is
      I : Positive := 1;
   begin
      while I <= CLI.Argument_Count loop
         if Get_Argument (I, "-t", Config.Timeout) then null;
         elsif Get_Argument (I, "-l", Config.Layout_File_Name) then null;
         elsif Get_Argument (I, "-d", Config.Dictionary_File_Name) then null;
         elsif Get_Argument (I, "-v", Config.Log_Level) then null;
         elsif Get_Argument (I, "-vv", Config.Log_Level) then null;
         elsif Get_Argument (I, "-vvv", Config.Log_Level) then null;
         else
            raise ARGUMENTS_ERROR;
         end if;
      end loop;
   end Get_Settings;


   procedure Load_Layout (Config : in out Settings) is
      use Qweyboard;
   begin
      --  This is somewhat temporary. We want to load the layout from a file soon...
      Add_Key (Config.Layout, NOKEY, LZ, 'Z');
      Add_Key (Config.Layout, NOKEY, LF, 'F');
      Add_Key (Config.Layout, NOKEY, LS, 'S');
      Add_Key (Config.Layout, NOKEY, LP, 'P');
      Add_Key (Config.Layout, NOKEY, LT, 'T');
      Add_Key (Config.Layout, NOKEY, LC, 'C');
      Add_Key (Config.Layout, NOKEY, LK, 'K');
      Add_Key (Config.Layout, NOKEY, LJ, 'J');
      Add_Key (Config.Layout, NOKEY, LR, 'R');
      Add_Key (Config.Layout, NOKEY, LL, 'L');
      Add_Key (Config.Layout, NOKEY, LI, 'I');
      Add_Key (Config.Layout, NOKEY, LO, 'O');
      Add_Key (Config.Layout, NOKEY, LE, 'E');
      Add_Key (Config.Layout, NOKEY, LN, 'N');
      Add_Key (Config.Layout, NOKEY, MAPO, ''');
      Add_Key (Config.Layout, NOKEY, MU, 'U');
      Add_Key (Config.Layout, NOKEY, MA, 'A');
      Add_Key (Config.Layout, NOKEY, MY, 'Y');
      Add_Key (Config.Layout, NOKEY, RO, 'O');
      Add_Key (Config.Layout, NOKEY, RI, 'I');
      Add_Key (Config.Layout, NOKEY, RE, 'E');
      Add_Key (Config.Layout, NOKEY, RN, 'N');
      Add_Key (Config.Layout, NOKEY, RK, 'K');
      Add_Key (Config.Layout, NOKEY, RJ, 'J');
      Add_Key (Config.Layout, NOKEY, RR, 'R');
      Add_Key (Config.Layout, NOKEY, RL, 'L');
      Add_Key (Config.Layout, NOKEY, RP, 'P');
      Add_Key (Config.Layout, NOKEY, RT, 'T');
      Add_Key (Config.Layout, NOKEY, RC, 'C');
      Add_Key (Config.Layout, NOKEY, RF, 'F');
      Add_Key (Config.Layout, NOKEY, RS, 'S');
      Add_Key (Config.Layout, NOKEY, RZ, 'Z');
      Add_Key (Config.Layout, LJ, LP, 'B');
      Add_Key (Config.Layout, LJ, LT, 'D');
      Add_Key (Config.Layout, LJ, LC, 'G');
      Add_Key (Config.Layout, LJ, LL, 'H');
      Add_Key (Config.Layout, LJ, LN, 'W');
      Add_Key (Config.Layout, RJ, RN, 'W');
      Add_Key (Config.Layout, RJ, RL, 'H');
      Add_Key (Config.Layout, RJ, RP, 'B');
      Add_Key (Config.Layout, RJ, RT, 'D');
      Add_Key (Config.Layout, RJ, RC, 'G');
      Add_Key (Config.Layout, LR, LL, 'V');
      Add_Key (Config.Layout, LR, LN, 'M');
      Add_Key (Config.Layout, RR, RN, 'M');
      Add_Key (Config.Layout, RR, RL, 'V');
      Add_Key (Config.Layout, LC, LF, 'Q');
      Add_Key (Config.Layout, RC, RF, 'Q');
      Add_Key (Config.Layout, LK, LZ, 'X');
      Add_Key (Config.Layout, RK, RZ, 'X');
      Add_Key (Config.Layout, MSHI, LS, '$');
      Add_Key (Config.Layout, MSHI, LP, '%');
      Add_Key (Config.Layout, MSHI, LT, '/');
      Add_Key (Config.Layout, MSHI, LC, '(');
      Add_Key (Config.Layout, MSHI, LK, '&');
      Add_Key (Config.Layout, MSHI, LJ, '*');
      Add_Key (Config.Layout, MSHI, LR, '+');
      Add_Key (Config.Layout, MSHI, LI, '7');
      Add_Key (Config.Layout, MSHI, LO, '4');
      Add_Key (Config.Layout, MSHI, LE, '1');
      Add_Key (Config.Layout, MSHI, MAPO, '8');
      Add_Key (Config.Layout, MSHI, MU, '5');
      Add_Key (Config.Layout, MSHI, MA, '2');
      Add_Key (Config.Layout, MSHI, MY, '0');
      Add_Key (Config.Layout, MSHI, RO, '9');
      Add_Key (Config.Layout, MSHI, RI, '6');
      Add_Key (Config.Layout, MSHI, RE, '3');
      Add_Key (Config.Layout, MSHI, RK, '?');
      Add_Key (Config.Layout, MSHI, RJ, '=');
      Add_Key (Config.Layout, MSHI, RR, '-');
      Add_Key (Config.Layout, MSHI, RP, '!');
      Add_Key (Config.Layout, MSHI, RT, ';');
      Add_Key (Config.Layout, MSHI, RC, ')');
      Add_Key (Config.Layout, MSHI, RF, '"');
      Add_Key (Config.Layout, MSHI, RS, ':');
      
      if To_String (Config.Layout_File_Name) = "swedish.layout" then
         Add_Key (Config.Layout, NOKEY, LI, 'Å', Replace => True);
         Add_Key (Config.Layout, NOKEY, LO, 'Ä', Replace => True);
         Add_Key (Config.Layout, NOKEY, LE, 'Ö', Replace => True);
      end if;
   end;


   function Get_Argument (Count : in out Positive; Flag : String; File_Name : out Unbounded_String) return Boolean is
   begin
      if CLI.Argument (Count) /= Flag then
         return False;
      end if;
      File_Name := To_Unbounded_String (CLI.Argument (Count + 1));
      Count := Count + 2;
      return True;
   end Get_Argument;
   
   function Get_Argument (Count : in out Positive; Flag : String; Timeout : out Duration) return Boolean is
   begin
      if CLI.Argument (Count) /= Flag then
         return False;
      end if;
      Timeout := Duration (Natural'Value (CLI.Argument (Count + 1))) / 1000.0;
      Count := Count + 2;
      return True;
   exception
      when CONSTRAINT_ERROR =>
         return False;
   end Get_Argument;

   function Get_Argument (Count : in out Positive; Flag : String; Verbosity : out Logging.Verbosity_Level) return Boolean is
   begin
      if CLI.Argument (Count) /= Flag then
         return False;
      end if;
      if Flag = "-v" then
         Verbosity := Logging.Log_Warning;
      elsif Flag = "-vv" then
         Verbosity := Logging.Log_Info;
      elsif Flag = "-vvv" then
         Verbosity := Logging.Log_Chatty;
      else
         return False;
      end if;
      Count := Count + 1;
      return True;
   end;

end Configuration;
