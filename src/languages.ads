with Ada.Strings.Unbounded;
with Ada.Text_IO;
private with Ada.Containers.Vectors;
private with Ada.Containers.Ordered_Maps;
private with Ada.Characters.Latin_1;
private with Ada.Characters.Handling;
with Keys; use Keys;

with Logging;

package Languages is
   use Logging;
   use Ada.Strings.Unbounded;
   
   PARSE_ERROR : Exception;

   type Language is private;
   function Decode (User_Language : Language; Released : Key_Sets.Set) return Unbounded_String;
   function Get_Standard return Language;
private
   package Key_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Softkey);
   package Layer_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Character);
   package Layout_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Softkey, Element_Type => Layer_Maps.Map, "=" => Layer_Maps."=");

   type Layout_Type is record
      --  This is a vector because we want the order they're declared in to matter...
      --  (except, of course, NOKEY which gets last dibs always)
      Modifiers : Key_Vectors.Vector;
      Layers : Layout_Maps.Map;
   end record;

   function Virtual_Layer (Layout : Layout_Type; Pressed : Key_Sets.Set) return Layer_Maps.Map;
   function Mod_Layer (Layout : Layout_Type; Modifier : Softkey; Pressed : Key_Sets.Set) return Layer_Maps.Map;
   procedure Add_Key (Layout : in out Layout_Type; Modifier : Softkey; Key : Softkey; Letter : Character; Replace : Boolean := False);

   type Language is record
      Layout : Layout_Type;
   end record;
end Languages;
