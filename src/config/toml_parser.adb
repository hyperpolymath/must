-- toml_parser.adb
-- TOML parser for Must
-- Copyright (C) 2025 Jonathan D.A. Jewell
-- SPDX-License-Identifier: AGPL-3.0-or-later

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

package body TOML_Parser is

   --  Character sets
   Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (" " & ASCII.HT);

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Whitespace, Whitespace);
   end Trim;

   function Make_String (S : String) return TOML_Value_Access is
   begin
      return new TOML_Value'(Kind => Val_String,
                             Str_Val => Must_Types.To_Unbounded (S));
   end Make_String;

   function Make_Boolean (B : Boolean) return TOML_Value_Access is
   begin
      return new TOML_Value'(Kind => Val_Boolean, Bool_Val => B);
   end Make_Boolean;

   function Make_Integer (I : Long_Integer) return TOML_Value_Access is
   begin
      return new TOML_Value'(Kind => Val_Integer, Int_Val => I);
   end Make_Integer;

   function Make_Table return TOML_Value_Access is
   begin
      return new TOML_Value'(Kind => Val_Table, Table_Val => Value_Maps.Empty_Map);
   end Make_Table;

   function Make_Array return TOML_Value_Access is
   begin
      return new TOML_Value'(Kind => Val_Array, Arr_Val => Value_Vectors.Empty_Vector);
   end Make_Array;

   --  Parse a quoted string value
   function Parse_String_Value (S : String) return String is
      Result : Unbounded_String;
      I      : Positive := S'First;
      Quote  : Character;
   begin
      if S'Length < 2 then
         raise Parse_Error with "Invalid string: " & S;
      end if;

      Quote := S (I);
      if Quote /= '"' and then Quote /= ''' then
         raise Parse_Error with "String must start with quote: " & S;
      end if;

      I := I + 1;
      while I <= S'Last loop
         if S (I) = Quote then
            return Must_Types.To_String (Result);
         elsif S (I) = '\' and then I < S'Last then
            I := I + 1;
            case S (I) is
               when 'n' => Append (Result, ASCII.LF);
               when 't' => Append (Result, ASCII.HT);
               when 'r' => Append (Result, ASCII.CR);
               when '\' => Append (Result, '\');
               when '"' => Append (Result, '"');
               when ''' => Append (Result, ''');
               when others => Append (Result, S (I));
            end case;
         else
            Append (Result, S (I));
         end if;
         I := I + 1;
      end loop;

      raise Parse_Error with "Unterminated string: " & S;
   end Parse_String_Value;

   --  Parse a value (string, integer, boolean, array)
   function Parse_Value (S : String) return TOML_Value_Access is
      Trimmed : constant String := Trim (S);
   begin
      if Trimmed'Length = 0 then
         return Make_String ("");
      end if;

      --  Boolean
      if Trimmed = "true" then
         return Make_Boolean (True);
      elsif Trimmed = "false" then
         return Make_Boolean (False);
      end if;

      --  String
      if Trimmed (Trimmed'First) = '"' or else Trimmed (Trimmed'First) = ''' then
         return Make_String (Parse_String_Value (Trimmed));
      end if;

      --  Array
      if Trimmed (Trimmed'First) = '[' then
         declare
            Arr   : constant TOML_Value_Access := Make_Array;
            Inner : constant String :=
              Trim (Trimmed (Trimmed'First + 1 .. Trimmed'Last - 1));
            Start : Positive := Inner'First;
            I     : Positive := Inner'First;
            Depth : Natural := 0;
            In_Str : Boolean := False;
         begin
            if Inner'Length = 0 then
               return Arr;
            end if;

            while I <= Inner'Last loop
               if not In_Str then
                  if Inner (I) = '"' then
                     In_Str := True;
                  elsif Inner (I) = '[' then
                     Depth := Depth + 1;
                  elsif Inner (I) = ']' then
                     Depth := Depth - 1;
                  elsif Inner (I) = ',' and Depth = 0 then
                     Arr.Arr_Val.Append
                       (Parse_Value (Inner (Start .. I - 1)));
                     Start := I + 1;
                  end if;
               else
                  if Inner (I) = '"' and then
                     (I = Inner'First or else Inner (I - 1) /= '\')
                  then
                     In_Str := False;
                  end if;
               end if;
               I := I + 1;
            end loop;

            --  Last element
            if Start <= Inner'Last then
               Arr.Arr_Val.Append (Parse_Value (Inner (Start .. Inner'Last)));
            end if;

            return Arr;
         end;
      end if;

      --  Integer
      declare
         Val : Long_Integer;
      begin
         Val := Long_Integer'Value (Trimmed);
         return Make_Integer (Val);
      exception
         when others =>
            --  Not a valid integer, treat as bareword/string
            return Make_String (Trimmed);
      end;
   end Parse_Value;

   function Split_Path (Path : String) return String_Vector is
      Segments  : String_Vector;
      Current   : Unbounded_String := Must_Types.To_Unbounded ("");
      In_Quotes : Boolean := False;
      I         : Positive := Path'First;
   begin
      while I <= Path'Last loop
         declare
            C : constant Character := Path (I);
         begin
            if C = '"' then
               In_Quotes := not In_Quotes;
            elsif C = '.' and then not In_Quotes then
               Segments.Append (Must_Types.To_String (Current));
               Current := Must_Types.To_Unbounded ("");
            elsif C = '\' and then In_Quotes and then I < Path'Last then
               I := I + 1;
               Ada.Strings.Unbounded.Append (Current, Path (I));
            else
               Ada.Strings.Unbounded.Append (Current, C);
            end if;
         end;
         I := I + 1;
      end loop;

      if Ada.Strings.Unbounded.Length (Current) > 0 or else Segments.Is_Empty then
         Segments.Append (Must_Types.To_String (Current));
      end if;

      return Segments;
   end Split_Path;

   --  Navigate to or create path in document
   procedure Navigate_Or_Create
     (Doc     : in out TOML_Document;
      Path    : String;
      Target  : out TOML_Value_Access;
      Parent  : out TOML_Value_Access;
      Last_Key : out Unbounded_String)
   is
      Current : TOML_Value_Access := null;
      Segments : constant String_Vector := Split_Path (Path);
      Key      : Unbounded_String;
   begin
      Parent := null;
      Last_Key := Must_Types.To_Unbounded ("");

      --  Start with doc root as implicit table
      for Segment of Segments loop
         Key := Must_Types.To_Unbounded (Segment);
         if Current = null then
            --  At root level
            if not Doc.Contains (Must_Types.To_String (Key)) then
               Doc.Insert (Must_Types.To_String (Key), Make_Table);
            end if;
            Parent := null;
            Current := Doc (Must_Types.To_String (Key));
         else
            --  Inside a table
            if Current.Kind /= Val_Table then
               raise Parse_Error with "Cannot navigate into non-table: " & Path;
            end if;
            if not Current.Table_Val.Contains (Must_Types.To_String (Key)) then
               Current.Table_Val.Insert (Must_Types.To_String (Key), Make_Table);
            end if;
            Parent := Current;
            Current := Current.Table_Val (Must_Types.To_String (Key));
         end if;

         Last_Key := Key;
      end loop;

      Target := Current;
   end Navigate_Or_Create;

   function Parse_String (Content : String) return TOML_Document is
      Doc           : TOML_Document;
      Current_Table : Unbounded_String := Must_Types.To_Unbounded ("");
      Lines         : String_Vector;
      Line_Start    : Positive := Content'First;
      I             : Positive := Content'First;
   begin
      --  Split into lines
      while I <= Content'Last loop
         if Content (I) = ASCII.LF then
            if I > Line_Start then
               Lines.Append (Content (Line_Start .. I - 1));
            else
               Lines.Append ("");
            end if;
            Line_Start := I + 1;
         end if;
         I := I + 1;
      end loop;
      if Line_Start <= Content'Last then
         Lines.Append (Content (Line_Start .. Content'Last));
      end if;

      --  Process each line
      declare
         Idx : Natural := Lines.First_Index;
      begin
         while Idx <= Lines.Last_Index loop
            declare
               Line    : constant String := Lines (Idx);
               Trimmed : constant String := Trim (Line);
               Target  : TOML_Value_Access;
               Parent  : TOML_Value_Access;
               Last_Key : Unbounded_String;
            begin
            --  Skip empty lines and comments
            if Trimmed'Length = 0 or else Trimmed (Trimmed'First) = '#' then
               null;

            --  Table header [table.name]
            elsif Trimmed (Trimmed'First) = '[' then
               if Trimmed'Length > 1 and then
                  Trimmed (Trimmed'First + 1) = '['
               then
                  --  Array of tables [[table.name]]
                  Current_Table := To_Unbounded
                    (Trim (Trimmed (Trimmed'First + 2 .. Trimmed'Last - 2)));
                  Navigate_Or_Create (Doc, Must_Types.To_String (Current_Table),
                                      Target, Parent, Last_Key);
                  if Target.Kind /= Val_Array then
                     --  Convert to array
                     declare
                        Arr : constant TOML_Value_Access := Make_Array;
                     begin
                        Arr.Arr_Val.Append (Make_Table);
                        if Parent = null then
                           Doc.Include (Must_Types.To_String (Last_Key), Arr);
                        else
                           Parent.Table_Val.Include (Must_Types.To_String (Last_Key), Arr);
                        end if;
                     end;
                  else
                     Target.Arr_Val.Append (Make_Table);
                  end if;
               else
                  --  Regular table [table.name]
                  Current_Table := To_Unbounded
                    (Trim (Trimmed (Trimmed'First + 1 .. Trimmed'Last - 1)));
                  Navigate_Or_Create (Doc, Must_Types.To_String (Current_Table),
                                      Target, Parent, Last_Key);
               end if;

            --  Key = value
            else
               declare
                  Eq_Pos : constant Natural :=
                    Ada.Strings.Fixed.Index (Trimmed, "=");
               begin
                  if Eq_Pos > 0 then
                     declare
                        Key   : constant String :=
                          Trim (Trimmed (Trimmed'First .. Eq_Pos - 1));
                        Value : Unbounded_String :=
                          Must_Types.To_Unbounded
                            (Trim (Trimmed (Eq_Pos + 1 .. Trimmed'Last)));
                        Full_Path : constant String :=
                          (if Length (Current_Table) > 0
                           then Must_Types.To_String (Current_Table) & "." & Key
                           else Key);
                        Val : TOML_Value_Access;
                     begin
                        --  Support multi-line arrays (collect until closing ])
                        if Ada.Strings.Unbounded.Length (Value) > 0 and then
                          Must_Types.To_String (Value) (1) = '[' and then
                          Ada.Strings.Fixed.Index (Must_Types.To_String (Value), "]") = 0
                        then
                           declare
                              Acc : Unbounded_String := Value;
                           begin
                              while Ada.Strings.Fixed.Index
                                (Must_Types.To_String (Acc), "]") = 0
                                and then Idx < Lines.Last_Index
                              loop
                                 Idx := Idx + 1;
                                 Acc := Acc & " " & Trim (Lines (Idx));
                              end loop;
                              Value := Acc;
                           end;
                        end if;

                        Val := Parse_Value (Must_Types.To_String (Value));
                        Navigate_Or_Create (Doc, Full_Path, Target, Parent, Last_Key);
                        if Parent = null then
                           Doc.Include (Must_Types.To_String (Last_Key), Val);
                        else
                           Parent.Table_Val.Include (Must_Types.To_String (Last_Key), Val);
                        end if;
                     end;
                  end if;
               end;
            end if;
            end;
            Idx := Idx + 1;
         end loop;
      end;

      return Doc;
   end Parse_String;

   function Parse_File (Filename : String) return TOML_Document is
      use Ada.Text_IO;
      File    : File_Type;
      Content : Unbounded_String;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Append (Content, Get_Line (File));
         Append (Content, ASCII.LF);
      end loop;
      Close (File);

      return Parse_String (Must_Types.To_String (Content));
   exception
      when Name_Error =>
         raise Parse_Error with "File not found: " & Filename;
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Parse_File;

   function Get (Doc : TOML_Document; Path : String) return TOML_Value_Access is
      Current : TOML_Value_Access := null;
      Segments : constant String_Vector := Split_Path (Path);
      Key      : Unbounded_String;
   begin
      for Segment of Segments loop
         Key := Must_Types.To_Unbounded (Segment);
         if Current = null then
            if not Doc.Contains (Must_Types.To_String (Key)) then
               return null;
            end if;
            Current := Doc (Must_Types.To_String (Key));
         else
            if Current.Kind /= Val_Table then
               return null;
            end if;
            if not Current.Table_Val.Contains (Must_Types.To_String (Key)) then
               return null;
            end if;
            Current := Current.Table_Val (Must_Types.To_String (Key));
         end if;
      end loop;

      return Current;
   end Get;

   function Has (Doc : TOML_Document; Path : String) return Boolean is
   begin
      return Get (Doc, Path) /= null;
   end Has;

   function Get_String (Doc : TOML_Document; Path : String;
                        Default : String := "") return String is
      Val : constant TOML_Value_Access := Get (Doc, Path);
   begin
      if Val = null then
         return Default;
      end if;
      if Val.Kind /= Val_String then
         return Default;
      end if;
      return Must_Types.To_String (Val.Str_Val);
   end Get_String;

   function Get_Boolean (Doc : TOML_Document; Path : String;
                         Default : Boolean := False) return Boolean is
      Val : constant TOML_Value_Access := Get (Doc, Path);
   begin
      if Val = null then
         return Default;
      end if;
      if Val.Kind /= Val_Boolean then
         return Default;
      end if;
      return Val.Bool_Val;
   end Get_Boolean;

   function Get_Integer (Doc : TOML_Document; Path : String;
                         Default : Long_Integer := 0) return Long_Integer is
      Val : constant TOML_Value_Access := Get (Doc, Path);
   begin
      if Val = null then
         return Default;
      end if;
      if Val.Kind /= Val_Integer then
         return Default;
      end if;
      return Val.Int_Val;
   end Get_Integer;

   function Get_String_Array (Doc : TOML_Document; Path : String)
     return String_Vector
   is
      Result : String_Vector;
      Val    : constant TOML_Value_Access := Get (Doc, Path);
   begin
      if Val = null or else Val.Kind /= Val_Array then
         return Result;
      end if;

      declare
         Arr : constant Value_Vectors.Vector := Val.Arr_Val;
      begin
         for Item of Arr loop
            if Item.all.Kind = Val_String then
               Result.Append (Must_Types.To_String (Item.all.Str_Val));
            end if;
         end loop;
      end;

      return Result;
   end Get_String_Array;

   function Get_Table_Keys (Doc : TOML_Document; Path : String)
     return String_Vector
   is
      Result : String_Vector;
      Val    : constant TOML_Value_Access := Get (Doc, Path);
   begin
      if Val = null or else Val.Kind /= Val_Table then
         return Result;
      end if;

      declare
         Tbl : constant Value_Maps.Map := Val.Table_Val;
      begin
         for C in Tbl.Iterate loop
            Result.Append (Value_Maps.Key (C));
         end loop;
      end;

      return Result;
   end Get_Table_Keys;

end TOML_Parser;
