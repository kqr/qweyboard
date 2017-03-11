package body Configuration is
   procedure Get_Settings (Config : in out Settings) is
      I : Positive := 1;
   begin
      while I <= CLI.Argument_Count loop
         if Get_Argument (I, "-t", Config.Timeout) then null;
         elsif Get_Argument (I, "-l", Config.Language_File_Name) then null;
         elsif Get_Argument (I, "-v", Config.Log_Level) then null;
         elsif Get_Argument (I, "-vv", Config.Log_Level) then null;
         elsif Get_Argument (I, "-vvv", Config.Log_Level) then null;
         else
            raise ARGUMENTS_ERROR;
         end if;
      end loop;
   end Get_Settings;

   procedure Load_Language (Config : in out Settings) is
   begin
      if Length (Config.Language_File_Name) > 0 then
         Qweyboard.Languages.Parser.Parse (To_String (Config.Language_File_Name));
      end if;
   end;

   function Get_Argument (Count : in out Positive; Flag : String; File_Name : in out Unbounded_String) return Boolean is
   begin
      if CLI.Argument (Count) /= Flag then
         return False;
      end if;
      File_Name := To_Unbounded_String (CLI.Argument (Count + 1));
      Count := Count + 2;
      return True;
   end Get_Argument;
   
   function Get_Argument (Count : in out Positive; Flag : String; Timeout : in out Ada.Real_Time.Time_Span) return Boolean is
   begin
      if CLI.Argument (Count) /= Flag then
         return False;
      end if;
      Timeout := Ada.Real_Time.Milliseconds
        (Natural'Value (CLI.Argument (Count + 1)));
      Count := Count + 2;
      return True;
   exception
      when CONSTRAINT_ERROR =>
         return False;
   end Get_Argument;

   function Get_Argument (Count : in out Positive; Flag : String; Verbosity : in out Logging.Verbosity_Level) return Boolean is
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
