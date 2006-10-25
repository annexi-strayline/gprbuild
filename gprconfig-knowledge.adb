------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with Glib.XML;                  use Glib;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with Sdefault;                  use Sdefault;

package body GprConfig.Knowledge is

   package XML_Int is new Glib.XML (Integer);
   use XML_Int;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Unbounded_String, Ada.Strings.Hash_Case_Insensitive, "=");

   use Compiler_Lists, Compiler_Description_Maps, String_Sets;
   use Configuration_Lists, Compilers_Filter_Lists, String_Maps;
   use Compiler_Filter_Lists;

   Ignore_Compiler : exception;
   --  Raised when the compiler should be ignored

   function TU (Str : String) return Unbounded_String;
   --  returns an unbounded string for Str (or Null_Unbounded_String if
   --  Str is the empty string)

   procedure Parse_Compiler_Description
     (Append_To   : in out Compiler_Description_Maps.Map;
      File        : String;
      Description : Node_Ptr);
   --  Parse a compiler description described by N

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node_Ptr);
   --  Parse a configuration node

   procedure Parse_External_Value
     (Value       : out External_Value;
      File        : String;
      Node        : Node_Ptr);
   --  Parse an XML node that describes an external value

   procedure Find_Compilers_In_Dir
     (Append_To : in out Compiler_Lists.List;
      Base      : Knowledge_Base;
      Directory : String);
   --  Find all known compilers in a specific directory

   function Get_Unfiltered_External_Value
     (Value  : External_Value;
      Path   : String;
      Target : String) return String;
   --  Computes the value of Value, depending on its type. When an external
   --  command needs to be executed, Path is put first on the PATH environment
   --  variable.
   --  Raises Ignore_Compiler if the value doesn't match its required regexp.

   procedure Get_Words
     (Words  : String;
      Filter : Unbounded_String;
      Map    : out String_Sets.Set);
   --  Return the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes

   procedure For_Each_Language_Runtime
     (Append_To  : in out Compiler_Lists.List;
      Name       : String;
      Path       : String;
      Target     : External_Value;
      Directory  : String;
      Version    : External_Value;
      Languages  : External_Value;
      Runtimes   : External_Value;
      Extra_Tool : Unbounded_String);
   --  For each language/runtime parsed in Languages/Runtimes, create a new
   --  compiler in the list.

   procedure Parse_All_Dirs
     (Append_To         : in out Unbounded_String;
      Current_Dir       : String;
      Path_To_Check     : String;
      Regexp            : Pattern_Matcher;
      Group             : Natural);
   --  Parse all subdirectories of Current_Dir for those that match
   --  Path_To_Check (see description of <directory>). When a match is found,
   --  the regexp is evaluated against the current directory, and the matching
   --  parenthesis group is appended to Append_To (comma-separated)

   function Substitute_Special_Dirs
     (Directory : String; Target : String) return String;
   --  Substitute the special "$..." names in Directory, as described in the
   --  <directory> tag

   function Match
     (Filter   : Compilers_Filter_Lists.List;
      Selected : Compiler_Lists.List) return Boolean;
   function Match
     (Filter   : Compilers_Filter;
      Selected : Compiler_Lists.List) return Boolean;
   function Match
     (Filter   : Compiler_Filter;
      Selected : Compiler_Lists.List) return Boolean;
   function Match
     (Target_Filter : String_Sets.Set;
      Selected      : Compiler_Lists.List) return Boolean;
   --  Return True if Filter matches the list of selected configurations

   procedure Merge_Config
     (Packages : in out String_Maps.Map;
      Config   : String);
   --  Merge the contents of Config into Packages, so that each attributes ends
   --  up in the right package, and the packages are not duplicated.

   procedure Skip_Spaces (Str : String; Index : in out Integer);
   --  Move Index from its current position to the next non-whitespace
   --  character in Str

   procedure Skip_Spaces_Backward (Str : String; Index : in out Integer);
   --  Same as Skip_Spaces, but goes backward

   --------
   -- TU --
   --------

   function TU (Str : String) return Unbounded_String is
   begin
      if Str = "" then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (Str);
      end if;
   end TU;

   --------------------------
   -- Parse_External_Value --
   --------------------------

   procedure Parse_External_Value
     (Value       : out External_Value;
      File        : String;
      Node        : Node_Ptr)
   is
      use type Glib.String_Ptr;
      Tmp : Node_Ptr := Node.Child;
   begin
      Value.Filter     := Null_Unbounded_String;
      Value.Must_Match := Null_Unbounded_String;

      if Node.Value /= null then
         Value := (Typ        => Value_Constant,
                   Filter     => Null_Unbounded_String,
                   Must_Match => Value.Must_Match,
                   Value      => To_Unbounded_String (Node.Value.all));
      end if;

      while Tmp /= null loop
         if Tmp.Tag.all = "external" then
            Value := (Typ        => Value_Shell,
                      Filter     => Value.Filter,
                      Must_Match => Value.Must_Match,
                      Command    => To_Unbounded_String (Tmp.Value.all),
                      Regexp     => To_Unbounded_String
                        (Get_Attribute (Tmp, "regexp", ".*")),
                      Group      => Integer'Value
                        (Get_Attribute (Tmp, "group", "0")));
         elsif Tmp.Tag.all = "directory" then
            Value := (Typ             => Value_Directory,
                      Filter          => Value.Filter,
                      Must_Match      => Value.Must_Match,
                      Directory       => To_Unbounded_String (Tmp.Value.all),
                      Directory_Group => Integer'Value
                        (Get_Attribute (Tmp, "group", "0")));
         elsif Tmp.Tag.all = "filter" then
            Value.Filter := To_Unbounded_String (Tmp.Value.all);
         elsif Tmp.Tag.all = "must_match" then
            Value.Must_Match := To_Unbounded_String (Tmp.Value.all);
         else
            Put_Line (Standard_Error, "Invalid XML description for "
                      & Node.Tag.all & " in file " & File);
            Put_Line (Standard_Error, "    Invalid tag: " & Tmp.Tag.all);
            Value := Null_External_Value;
         end if;

         Tmp := Tmp.Next;
      end  loop;


   exception
      when Constraint_Error =>
      Put_Line (Standard_Error, "Invalid group number for " & Node.Tag.all
                   & " in file " & File);
         Value := Null_External_Value;
   end Parse_External_Value;

   --------------------------------
   -- Parse_Compiler_Description --
   --------------------------------

   procedure Parse_Compiler_Description
     (Append_To   : in out Compiler_Description_Maps.Map;
      File        : String;
      Description : Node_Ptr)
   is
      Compiler : Compiler_Description;
      N        : Node_Ptr := Description.Child;
      Name     : String_Ptr;
   begin
      while N /= null loop
         if N.Tag.all = "executable" then
            Compiler.Executable := To_Unbounded_String (N.Value.all);
         elsif N.Tag.all = "name" then
            Name := N.Value;
         elsif N.Tag.all = "version" then
            Parse_External_Value
              (Value => Compiler.Version,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "languages" then
            Parse_External_Value
              (Value => Compiler.Languages,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "runtimes" then
            Parse_External_Value
              (Value => Compiler.Runtimes,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "target" then
            Parse_External_Value
              (Value => Compiler.Target,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "extra_tool" then
            Compiler.Extra_Tool := To_Unbounded_String (N.Value.all);
         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & N.Tag.all);
         end if;

         N := N.Next;
      end loop;

      if Name /= null then
         Include (Append_To, Name.all, Compiler);
      end if;
   end Parse_Compiler_Description;

   -------------------------
   -- Parse_Configuration --
   -------------------------

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node_Ptr)
   is
      Config    : Configuration;
      N         : Node_Ptr := Description.Child;
      N2        : Node_Ptr;
      Compilers : Compilers_Filter;
      Ignore_Config : Boolean := False;
   begin
      while N /= null loop
         if N.Tag.all = "compilers" then
            Compilers := No_Compilers_Filter;
            N2 := N.Child;
            while N2 /= null loop
               if N2.Tag.all = "compiler" then
                  Append
                    (Compilers.Compiler,
                     Compiler_Filter'
                       (Name     => TU (Get_Attribute (N2, "name", "")),
                        Version  => TU (Get_Attribute (N2, "version", "")),
                        Runtime  => TU (Get_Attribute (N2, "runtime", "")),
                        Language => TU (Get_Attribute (N2, "language", ""))));
               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & N2.Tag.all);
               end if;
               N2 := N2.Next;
            end loop;
            Append (Config.Compilers_Filters, Compilers);

         elsif N.Tag.all = "targets" then
            if not Is_Empty (Config.Targets_Filters) then
               Put_Line (Standard_Error,
                         "Can have a single <targets> filter in " & File);
            else
               N2 := N.Child;
               while N2 /= null loop
                  if N2.Tag.all = "target" then
                     Include (Config.Targets_Filters,
                              Get_Attribute (N2, "name", ""));
                  else
                     Put_Line
                       (Standard_Error, "Unknown XML tag in " & File & ": "
                        & N2.Tag.all);
                  end if;
                  N2 := N2.Next;
               end loop;
            end if;

         elsif N.Tag.all = "hosts" then
            --  Resolve this filter immediately. This saves memory, since we
            --  don't need to store it in memory if we know it won't apply.
            N2 := N.Child;
            Ignore_Config := True;
            while N2 /= null loop
               if N2.Tag.all = "host" then
                  if Match
                    (Get_Attribute (N2, "name", ""), Sdefault.Hostname)
                  then
                     Ignore_Config := False;
                     exit;
                  end if;

               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & N2.Tag.all);
               end if;

               N2 := N2.Next;
            end loop;
            exit when Ignore_Config;

         elsif N.Tag.all = "config" then
            Append (Config.Config, N.Value.all);

         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & N.Tag.all);
         end if;

         N := N.Next;
      end loop;

      if not Ignore_Config then
         Append (Append_To, Config);
      end if;
   end Parse_Configuration;

   --------------------------
   -- Parse_Knowledge_Base --
   --------------------------

   procedure Parse_Knowledge_Base
     (Base : out Knowledge_Base; Directory : String)
   is
      Search    : Search_Type;
      File      : Directory_Entry_Type;
      File_Node : Node_Ptr;
      N         : Node_Ptr;
   begin
      Start_Search
        (Search,
         Directory => Directory,
         Pattern   => "*.xml",
         Filter    => (Ordinary_File => True, others => False));

      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);

         File_Node := Parse (Full_Name (File));

         if File_Node = null then
            Put_Line (Standard_Error, "Could not parse " & Simple_Name (File));
         elsif File_Node.Tag.all = "gprconfig" then
            N := File_Node.Child;
            while N /= null loop
               if N.Tag.all = "compiler_description" then
                  Parse_Compiler_Description
                    (Append_To   => Base.Compilers,
                     File        => Simple_Name (File),
                     Description => N);
               elsif N.Tag.all = "configuration" then
                  Parse_Configuration
                    (Append_To   => Base.Configurations,
                     File        => Simple_Name (File),
                     Description => N);
               else
                  Put_Line (Standard_Error,
                            "Unknown XML tag in " & Simple_Name (File) & ": "
                            & N.Tag.all);
               end if;

               N := N.Next;
            end loop;
         else
            Put_Line (Standard_Error,
                      "Invalid toplevel XML tag in " & Simple_Name (File));
         end if;
         Free (File_Node);
      end loop;

      End_Search (Search);

   exception
      when Ada.Directories.Name_Error =>
         Put_Line (Standard_Error, "Directory not found: " & Directory);
   end Parse_Knowledge_Base;

   -----------------------------
   -- Substitute_Special_Dirs --
   -----------------------------

   function Substitute_Special_Dirs
     (Directory : String; Target : String) return String
   is
      Pos : Natural := Directory'First;
   begin
      while Pos <= Directory'Last loop
         if Directory (Pos) = '$' then
            if Pos + 4 <= Directory'Last
              and then Directory (Pos .. Pos + 4) = "$HOST"
            then
               return Directory (Directory'First .. Pos - 1)
                 & Sdefault.Hostname
                 & Substitute_Special_Dirs
                 (Directory (Pos + 5 .. Directory'Last), Target);

            elsif Pos + 6 <= Directory'Last
              and then Directory (Pos .. Pos + 6) = "$TARGET"
            then
               return Directory (Directory'First .. Pos - 1)
                 & Target
                 & Substitute_Special_Dirs
                 (Directory (Pos + 7 .. Directory'Last), Target);
            end if;
         end if;
         Pos := Pos + 1;
      end loop;
      return Directory;
   end Substitute_Special_Dirs;

   --------------------
   -- Parse_All_Dirs --
   --------------------

   procedure Parse_All_Dirs
     (Append_To         : in out Unbounded_String;
      Current_Dir       : String;
      Path_To_Check     : String;
      Regexp            : Pattern_Matcher;
      Group             : Natural)
   is
      First : constant Integer := Path_To_Check'First;
      Last  : Integer;
   begin
      if Path_To_Check'Length = 0 then
         declare
            Matched : Match_Array (0 .. Group);
         begin
            --  We matched, so insert the relevant path
            Match (Regexp, Current_Dir, Matched);
            if Matched (Group) /= No_Match then
               Append (Append_To, ",");
               Append
                 (Append_To,
                  Current_Dir (Matched (Group).First .. Matched (Group).Last));
            end if;
         end;

      else
         Last := First + 1;
         while Last <= Path_To_Check'Last
           and then Path_To_Check (Last) /= '/'
           and then Path_To_Check (Last) /= Directory_Separator
         loop
            Last := Last + 1;
         end loop;

         --  If we do not have a regexp
         if GNAT.Regpat.Quote (Path_To_Check (First .. Last - 1)) =
           Path_To_Check (First .. Last - 1)
         then
            if Ada.Directories.Exists
              (Current_Dir & Directory_Separator
               & Path_To_Check (First .. Last - 1))
            then
               --  If there is such a subdir, keep checking
               Parse_All_Dirs
                 (Append_To     => Append_To,
                  Current_Dir   =>
                    Current_Dir & Directory_Separator
                      & Path_To_Check (First .. Last - 1),
                  Path_To_Check =>
                    Path_To_Check (Last + 1 .. Path_To_Check'Last),
                  Regexp        => Regexp,
                  Group         => Group);
            end if;

         --  Else we have a regexp, check all files
         else
            declare
               File_Regexp : constant Pattern_Matcher :=
                 Compile (Path_To_Check (First .. Last - 1));
               Search : Search_Type;
               File   : Directory_Entry_Type;
            begin
               Start_Search
                 (Search    => Search,
                  Directory => Current_Dir,
                  Pattern   => "");
               while More_Entries (Search) loop
                  Get_Next_Entry (Search, File);
                  if Simple_Name (File) /= "."
                    and then Simple_Name (File) /= ".."
                    and then Match (File_Regexp, Simple_Name (File))
                  then
                     Parse_All_Dirs
                       (Append_To     => Append_To,
                        Current_Dir   => Full_Name (File),
                        Path_To_Check =>
                          Path_To_Check (Last + 1 .. Path_To_Check'Last),
                        Regexp        => Regexp,
                        Group         => Group);
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Parse_All_Dirs;

   -----------------------------------
   -- Get_Unfiltered_External_Value --
   -----------------------------------

   function Get_Unfiltered_External_Value
     (Value  : External_Value;
      Path   : String;
      Target : String) return String
   is
      Saved_Path : constant String := Ada.Environment_Variables.Value ("PATH");
      Status     : aliased Integer;
      Result     : Unbounded_String;
   begin
      case Value.Typ is
         when Value_Constant =>
            Result := Value.Value;

         when Value_Shell =>
            Ada.Environment_Variables.Set
              ("PATH", Path & Path_Separator & Saved_Path);
            declare
               Args   : Argument_List_Access := Argument_String_To_List
                 (To_String (Value.Command));
               Output : constant String := Get_Command_Output
                 (Command     => Args (Args'First).all,
                  Arguments   => Args (Args'First + 1 .. Args'Last),
                  Input       => "",
                  Status      => Status'Unchecked_Access,
                  Err_To_Out  => True);
               Regexp : constant Pattern_Matcher := Compile
                 (To_String (Value.Regexp), Multiple_Lines);
               Matched : Match_Array (0 .. Value.Group);
            begin
               GNAT.Strings.Free (Args);
               Ada.Environment_Variables.Set ("PATH", Saved_Path);

               Match (Regexp, Output, Matched);
               if Matched (Value.Group) /= No_Match then
                  Result := To_Unbounded_String
                    (Output (Matched (Value.Group).First ..
                             Matched (Value.Group).Last));
               else
                  Result := Null_Unbounded_String;
               end if;
            end;

         when Value_Directory =>
            declare
               Search : constant String := Substitute_Special_Dirs
                 (To_String (Value.Directory), Target);
            begin
               Parse_All_Dirs
                 (Append_To     => Result,
                  Current_Dir   => Path,
                  Path_To_Check => Search,
                  Regexp        => Compile (Search),
                  Group         => Value.Directory_Group);
            end;
      end case;

      if Value.Must_Match /= Null_Unbounded_String
        and then not Match (Expression => To_String (Value.Must_Match),
                            Data       => To_String (Result))
      then
         raise Ignore_Compiler;
      end if;

      return To_String (Result);
   end Get_Unfiltered_External_Value;

   ---------------
   -- Get_Words --
   ---------------

   procedure Get_Words
     (Words  : String;
      Filter : Unbounded_String;
      Map    : out String_Sets.Set)
   is
      First      : Integer := Words'First;
      Last       : Integer;
      Filter_Set : String_Sets.Set;
   begin
      if Filter /= Null_Unbounded_String then
         Get_Words (To_String (Filter), Null_Unbounded_String, Filter_Set);
      end if;

      while First <= Words'Last loop
         while First <= Words'Last
           and then (Words (First) = ' '
                     or else Words (First) = ',')
         loop
            First := First + 1;
         end loop;

         if First <= Words'Last then
            Last := First + 1;
            while Last <= Words'Last
              and then Words (Last) /= ' '
              and then Words (Last) /= ','
            loop
               Last := Last + 1;
            end loop;

            if Is_Empty (Filter_Set)
              or else Contains (Filter_Set, Words (First .. Last - 1))
            then
               Include (Map, Words (First .. Last - 1));
            end if;
         end if;

         First := Last + 1;
      end loop;
   end Get_Words;

   -------------------------------
   -- For_Each_Language_Runtime --
   -------------------------------

   procedure For_Each_Language_Runtime
     (Append_To  : in out Compiler_Lists.List;
      Name       : String;
      Path       : String;
      Target     : External_Value;
      Directory  : String;
      Version    : External_Value;
      Languages  : External_Value;
      Runtimes   : External_Value;
      Extra_Tool : Unbounded_String)
   is
      Unfiltered_Target    : constant String := Get_Unfiltered_External_Value
        (Target, Directory, "");
      Unfiltered_Version   : constant String := Get_Unfiltered_External_Value
        (Version, Directory, Unfiltered_Target);
      Unfiltered_Languages : constant String := Get_Unfiltered_External_Value
        (Languages, Directory, Unfiltered_Target);
      Unfiltered_Runtimes  : constant String := Get_Unfiltered_External_Value
        (Runtimes, Directory, Unfiltered_Target);

      Langs : String_Sets.Set;
      Runs  : String_Sets.Set;
      C     : String_Sets.Cursor;
      C2    : String_Sets.Cursor;
   begin
      --  If we can't find version, ignore this compiler
      if Unfiltered_Version = "" then
         raise Ignore_Compiler;
      end if;

      Get_Words (Words  => Unfiltered_Languages,
                 Filter => Languages.Filter,
                 Map    => Langs);
      Get_Words (Words  => Unfiltered_Runtimes,
                 Filter => Runtimes.Filter,
                 Map    => Runs);

      C := First (Langs);
      while Has_Element (C) loop
         declare
            L : String := Element (C);
         begin
            To_Mixed (L);

            if Is_Empty (Runs) then
               Append
                 (Append_To,
                  Compiler'
                    (Name       => To_Unbounded_String (Name),
                     Target     => To_Unbounded_String (Unfiltered_Target),
                     Path       => To_Unbounded_String (Path),
                     Version    => To_Unbounded_String (Unfiltered_Version),
                     Language   => To_Unbounded_String (L),
                     Runtime    => Null_Unbounded_String,
                     Extra_Tool => Extra_Tool));

            else
               C2 := First (Runs);
               while Has_Element (C2) loop
                  Append
                    (Append_To,
                     Compiler'
                       (Name       => To_Unbounded_String (Name),
                        Path       => To_Unbounded_String (Path),
                        Target     => To_Unbounded_String (Unfiltered_Target),
                        Version    => To_Unbounded_String (Unfiltered_Version),
                        Language   => To_Unbounded_String (L),
                        Runtime    => To_Unbounded_String (Element (C2)),
                        Extra_Tool => Extra_Tool));
                  Next (C2);
               end loop;
            end if;
         end;

         Next (C);
      end loop;
   end For_Each_Language_Runtime;

   ---------------------------
   -- Find_Compilers_In_Dir --
   ---------------------------

   procedure Find_Compilers_In_Dir
     (Append_To : in out Compiler_Lists.List;
      Base      : Knowledge_Base;
      Directory : String)
   is
      C      : Compiler_Description_Maps.Cursor;
   begin
      --  Do not search all entries in the directory, but check explictly for
      --  the compilers. This results in a lot less system calls, and thus is
      --  faster

      C := First (Base.Compilers);
      while Has_Element (C) loop
         declare
            F : constant String := Normalize_Pathname
              (Name           => To_String (Element (C).Executable),
               Directory      => Directory,
               Resolve_Links  => False,
               Case_Sensitive => True);
         begin
            if Ada.Directories.Exists (F) then
               For_Each_Language_Runtime
                 (Append_To  => Append_To,
                  Name       => Key (C),
                  Path       => Directory,
                  Directory  => Directory,
                  Target     => Element (C).Target,
                  Version    => Element (C).Version,
                  Languages  => Element (C).Languages,
                  Runtimes   => Element (C).Runtimes,
                  Extra_Tool => Element (C).Extra_Tool);
            end if;
         exception
            when Ignore_Compiler =>
               null;  --  Nothing to do, the compiler has not been inserted
         end;

         Next (C);
      end loop;
   end Find_Compilers_In_Dir;

   ----------------------------
   -- Find_Compilers_In_Path --
   ----------------------------

   procedure Find_Compilers_In_Path
     (Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List)
   is
      Map : String_Sets.Set;
   begin
      if Ada.Environment_Variables.Exists ("PATH") then
         declare
            Path : constant String := Ada.Environment_Variables.Value ("PATH");
            First, Last : Natural;
         begin
            First := Path'First;
            while First <= Path'Last loop
               Last := First + 1;
               while Last <= Path'Last
                 and then Path (Last) /= GNAT.OS_Lib.Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               --  Use a hash to make sure we do not parse the same directory
               --  twice. This is both more efficient and avoids duplicates in
               --  the final result list
               if not Contains (Map, Path (First .. Last - 1)) then
                  Include (Map, Path (First .. Last - 1));
                  Find_Compilers_In_Dir
                    (Append_To => Compilers,
                     Base      => Base,
                     Directory => Path (First .. Last - 1));
               end if;

               First := Last + 1;
            end loop;
         end;
      end if;
   end Find_Compilers_In_Path;

   -----------
   -- Match --
   -----------

   function Match
     (Filter   : Compilers_Filter;
      Selected : Compiler_Lists.List) return Boolean
   is
      C : Compiler_Filter_Lists.Cursor := First (Filter.Compiler);
   begin
      while Has_Element (C) loop
         if Match (Element (C), Selected) then
            return True;
         end if;
         Next (C);
      end loop;
      return False;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Filter   : Compiler_Filter;
      Selected : Compiler_Lists.List) return Boolean
   is
      C    : Compiler_Lists.Cursor := First (Selected);
      Comp : Compiler;
   begin
      while Has_Element (C) loop
         Comp := Element (C);
         if Filter.Name = Comp.Name
           and then
             (Filter.Version = Null_Unbounded_String
              or else Match
                (To_String (Filter.Version), To_String (Comp.Version)))
           and then (Filter.Runtime = Null_Unbounded_String
              or else Match
                (To_String (Filter.Runtime), To_String (Comp.Runtime)))
           and then (Filter.Language = Null_Unbounded_String
              or else Match
                (To_String (Filter.Language), To_String (Comp.Language)))
         then
            return True;
         end if;
         Next (C);
      end loop;
      return False;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Filter   : Compilers_Filter_Lists.List;
      Selected : Compiler_Lists.List) return Boolean
   is
      C : Compilers_Filter_Lists.Cursor := First (Filter);
   begin
      while Has_Element (C) loop
         if not Match (Element (C), Selected) then
            return False;
         end if;
         Next (C);
      end loop;
      return True;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Target_Filter : String_Sets.Set;
      Selected      : Compiler_Lists.List) return Boolean
   is
      Target : String_Sets.Cursor := First (Target_Filter);
      Comp   : Compiler_Lists.Cursor;
   begin
      if Is_Empty (Target_Filter) then
         return True;

      else
         while Has_Element (Target) loop
            Comp := First (Selected);
            while Has_Element (Comp) loop
               if Match
                 (Element (Target), To_String (Element (Comp).Target))
               then
                  return True;
               end if;
               Next (Comp);
            end loop;

            Next (Target);
         end loop;
         return False;
      end if;
   end Match;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces (Str : String; Index : in out Integer) is
   begin
      while Index <= Str'Last
        and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

   procedure Skip_Spaces_Backward (Str : String; Index : in out Integer) is
   begin
      while Index >= Str'First
        and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
      loop
         Index := Index - 1;
      end loop;
   end Skip_Spaces_Backward;

   ------------------
   -- Merge_Config --
   ------------------

   procedure Merge_Config
     (Packages : in out String_Maps.Map;
      Config   : String)
   is
      First : Integer := Index (Config, "package ");
      Pkg_Name_First, Pkg_Name_Last : Integer;
      Pkg_Content_First : Integer;
      Last  : Integer;
      C     : String_Maps.Cursor;
   begin
      while First /= 0 loop
         Pkg_Name_First := First + 8;
         Skip_Spaces (Config, Pkg_Name_First);

         Pkg_Name_Last := Pkg_Name_First + 1;
         while Pkg_Name_Last <= Config'Last
           and then Config (Pkg_Name_Last) /= ' '
           and then Config (Pkg_Name_Last) /= ASCII.LF
         loop
            Pkg_Name_Last := Pkg_Name_Last + 1;
         end loop;

         Pkg_Content_First := Pkg_Name_Last + 1;
         Skip_Spaces (Config, Pkg_Content_First);
         Pkg_Content_First := Pkg_Content_First + 2; --  skip "is"
         Skip_Spaces (Config, Pkg_Content_First);

         Last := Index (Config (Pkg_Content_First .. Config'Last),
                        "end " & Config (Pkg_Name_First .. Pkg_Name_Last - 1));
         if Last /= 0 then
            Last := Last - 1;
            Skip_Spaces_Backward (Config, Last);

            C := Find (Packages, Config (Pkg_Name_First .. Pkg_Name_Last - 1));
            if Has_Element (C) then
               Replace_Element
                 (Packages,
                  C,
                  Element (C) & ASCII.LF & "      " & To_Unbounded_String
                    (Config (Pkg_Content_First .. Last)));

            else
               Insert
                 (Packages,
                  Config (Pkg_Name_First .. Pkg_Name_Last - 1),
                  "      " &
                  To_Unbounded_String (Config (Pkg_Content_First .. Last)));
            end if;
         end if;

         First := Index (Config (First + 5 .. Config'Last), "package ");
      end loop;
   end Merge_Config;

   ----------------------------
   -- Generate_Configuration --
   ----------------------------

   procedure Generate_Configuration
     (Base        : Knowledge_Base;
      Selected    : Compiler_Lists.List;
      Output_File : String)
   is
      Output   : File_Type;
      Config   : Configuration_Lists.Cursor := First (Base.Configurations);
      Packages : String_Maps.Map;
      C        : String_Maps.Cursor;
   begin
      while Has_Element (Config) loop
         if Match (Element (Config).Compilers_Filters, Selected)
           and then Match (Element (Config).Targets_Filters, Selected)
         then
            Merge_Config (Packages, To_String (Element (Config).Config));
         end if;

         Next (Config);
      end loop;

      Create (Output, Out_File, Output_File);
      C := First (Packages);
      while Has_Element (C) loop
         Put_Line (Output, "   package " & Key (C) & " is");
         Put_Line (Output, To_String (Element (C)));
         Put_Line (Output, "   end " & Key (C) & ";");
         Next (C);
      end loop;
      Close (Output);
   end Generate_Configuration;

end GprConfig.Knowledge;
