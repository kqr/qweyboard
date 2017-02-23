package body Configuration is
   procedure Get_Settings (Config : out Settings) is
   begin
      if CLI.Argument_Count = 0 then
         null;
      elsif CLI.Argument_Count = 2 and (Get_Argument (1, Config.Language) or Get_Argument (1, Config.Timeout)) then
         null;
      elsif CLI.Argument_Count = 4 and Get_Argument (1, Config.Language) and Get_Argument (3, Config.Timeout) then
         null;
      elsif CLI.Argument_Count = 4 and Get_Argument (3, Config.Language) and Get_Argument (1, Config.Timeout) then
         null;
      else
         raise ARGUMENTS_ERROR; 
      end if;
   end Get_Settings;

   function Get_Argument (Count : Natural; Language : out Qweyboard.Language_Variant) return Boolean is
   begin
      if CLI.Argument (Count) /= "-l" then
         return False;
      end if;
      if CLI.Argument (Count + 1) = "standard" then
         Language := Qweyboard.Standard;
      elsif CLI.Argument (Count + 1) = "swedish" then
         Language := Qweyboard.Swedish;
      else
         return False;
      end if;
      return True;
   end Get_Argument;
   
   function Get_Argument (Count : Natural; Timeout : out Duration) return Boolean is
   begin
      if CLI.Argument (Count) /= "-t" then
         return False;
      end if;
      Timeout := Duration'Value (CLI.Argument (Count + 1));
      return True;
   exception
      when CONSTRAINT_ERROR =>
         return False;
   end Get_Argument;
end Configuration;
