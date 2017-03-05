with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Keys; use Keys;

with Logging;

package Languages is
   use Logging;
   use Ada.Strings.Unbounded;
   
   PARSE_ERROR : Exception;

   package Key_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Softkey);
   package Layer_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Character);
   package Layout_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Layer_Maps.Map, "=" => Layer_Maps."=");
   package Substitution_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String, Element_Type => Unbounded_String);

   type Layout_Type is record
      --  This is a vector because we want the order they're declared in to matter...
      --  (except, of course, NOKEY which gets last dibs always)
      Modifiers : Key_Vectors.Vector;
      Layers : Layout_Maps.Map;
   end record;
   
   type Substitution_Type is (Left, Middle, Right);
   type Substitution_Dictionary is array (Substitution_Type) of Substitution_Maps.Map;

   protected User_Language is
      function Decode (Released : Key_Sets.Set) return Unbounded_String;
      procedure Add_Key (Modifier : Softkey; Key : Softkey; Symbol : Character);
      procedure Add_Substitution (Position : Substitution_Type; Pattern : Unbounded_String; Replacement : Unbounded_String);
   private
      Current_Layout : Layout_Type;
      Substitutions : Substitution_Dictionary;

      function Virtual_Layer (Layout : Layout_Type; Pressed : Key_Sets.Set) return Layer_Maps.Map;
      function Mod_Layer (Layout : Layout_Type; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map;
   end User_Language;
end Languages;
