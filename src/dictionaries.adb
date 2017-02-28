package body Dictionaries is
   procedure Add_Definition (To_Dictionary : in out Dictionary; From : Unbounded_String; To : Unbounded_String) is
   begin
      To_Dictionary.Syllables.Insert (From, To);
   end Add_Definition;

   function Apply_To (Using_Dictionary : in out Dictionary; Text : Unbounded_String) return Unbounded_String is
   begin
      if Using_Dictionary.Syllables.Contains (Text) then
         Log.Info ("[Qweyboard] """ & To_String (Text) & """ found in dictionary, replacing with """ & To_String (Using_Dictionary.Syllables (Text)) & """");
         return Using_Dictionary.Syllables (Text);
      else
         return Text;
      end if;
   end Apply_To;

   function Parse (Source : Ada.Text_IO.File_Type) return Dictionary is
      use Ada.Text_IO;

      Result : Dictionary;
      
      procedure Panic is
      begin
         raise PARSE_ERROR with "Dictionary parser encountered unexpected input on line" & Positive_Count'Image (Line) & ".";
      end;
   begin
      --  <dictionary> ::= <entry> <rest>
      --  <rest> ::= e | "\n" <dictionary>
      --  <entry> ::= <word> "=" <replacement>
      while not End_Of_File (Source) loop
         Log.Chat ("[Dictionaries] Loading dictionary line" & Positive_Count'Image (Line (Source)));
         declare
            use Ada.Characters.Latin_1;
            use Ada.Characters.Handling;
            Word : Unbounded_String;
            Replacement : Unbounded_String;
            C : Character;
         begin
            Get_Immediate (Source, C);
            while Is_Lower (C) loop
               Word := Word & C;
               if End_Of_File (Source) then Panic; else Get_Immediate (Source, C); end if;
            end loop;
            if Length (Word) = 0 then
               Panic;
            end if;
            if C /= '=' then
               Panic;
            end if;
            if End_Of_File (Source) then Panic; else Get_Immediate (Source, C); end if;
            while C /= LF loop
               Replacement := Replacement & C;
               if End_Of_File (Source) then exit; else Get_Immediate (Source, C); end if;
            end loop;
            Log.Info ("[Dictionaries] Adding definition """ & To_String (Word) & """=""" & To_String (Replacement) & """");
            Add_Definition (Result, Word, Replacement);
         end;
      end loop;
      return Result;
   end Parse;
end Dictionaries;
