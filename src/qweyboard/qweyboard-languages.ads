with Unicode_Strings; use Unicode_Strings;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
private with Logging;

package Qweyboard.Languages is
   use Unbounded;
   
   package Key_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Softkey);
   package Layer_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Wide_Wide_Character);
   package Layout_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Layer_Maps.Map, "=" => Layer_Maps."=");
   package Substitution_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_Wide_Wide_String, Element_Type => Unbounded_Wide_Wide_String);

   type Layout_Type is record
      --  This is a vector because we want the order they're declared in to matter...
      --  (except, of course, NOKEY which gets last dibs always)
      Modifiers : Key_Vectors.Vector;
      Layers : Layout_Maps.Map;
   end record;
   
   type Substitution_Type is (Left, Middle, Right);
   type Substitution_Dictionary is array (Substitution_Type) of Substitution_Maps.Map;

   --  TODO: is a protected object really the right abstraction here?
   protected User_Language is
      function Decode (Released : Key_Sets.Set) return Unbounded_Wide_Wide_String;
      procedure Add_Key (Modifier : Softkey; Key : Softkey; Symbol : Wide_Wide_Character);
      procedure Add_Substitution (Position : Substitution_Type; Pattern : Unbounded_Wide_Wide_String; Replacement : Unbounded_Wide_Wide_String);
   private
      Current_Layout : Layout_Type;
      Substitutions : Substitution_Dictionary;

      function Virtual_Layer (Layout : Layout_Type; Pressed : Key_Sets.Set) return Layer_Maps.Map;
      function Mod_Layer (Layout : Layout_Type; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map;
      function Perform_Substitutions (Text : Unbounded_Wide_Wide_String; From : Substitution_Maps.Map) return Unbounded_Wide_Wide_String;
   end User_Language;
private
   use Logging;
end Qweyboard.Languages;
