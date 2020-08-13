------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the internal data structures used for representation
--  of Ada Library Information (ALI) acquired from the ALI files generated by
--  the front end.

with System.Rident;

with GNAT.Table;

with GPR.Osint; use GPR.Osint;

package GPR.ALI is

   package Rident is new System.Rident;
   use Rident;

   --------------
   -- Id Types --
   --------------

   --  The various entries are stored in tables with distinct subscript ranges.
   --  The following type definitions show the ranges used for the subscripts
   --  (Id values) for the various tables.

   type ALI_Id is range 0 .. 99_999_999;
   --  Id values used for ALIs table entries

   type Unit_Id is range 0 .. 99_999_999;
   --  Id values used for Unit table entries

   type With_Id is range 0 .. 99_999_999;
   --  Id values used for Withs table entries

   type Arg_Id is range 0 .. 99_999_999;
   --  Id values used for argument table entries

   type Sdep_Id is range 0 .. 99_999_999;
   --  Id values used for Sdep table entries

   --------------------
   -- ALI File Table --
   --------------------

   --  Each ALI file read generates an entry in the ALIs table

   No_ALI_Id : constant ALI_Id := ALI_Id'First;
   --  Special value indicating no ALI entry

   First_ALI_Entry : constant ALI_Id := No_ALI_Id + 1;
   --  Id of first actual entry in table

   GNAT_Version_Max_Len : constant Natural := 32;
   --  "Safe" length for GNAT Version

   type Main_Program_Type is (None, Proc, Func);
   --  Indicator of whether unit can be used as main program

   type ALIs_Record is record

      Afile : File_Name_Type;
      --  Name of ALI file

      Ofile_Full_Name : File_Name_Type;
      --  Full name of object file corresponding to the ALI file

      Sfile : File_Name_Type;
      --  Name of source file that generates this ALI file (which is equal
      --  to the name of the source file in the first unit table entry for
      --  this ALI file, since the body if present is always first).

      SAL_Interface : Boolean;
      --  Set True when this is an interface to a standalone library

      First_Unit : Unit_Id;
      --  Id of first Unit table entry for this file

      Last_Unit : Unit_Id;
      --  Id of last Unit table entry for this file

      First_Sdep : Sdep_Id;
      --  Id of first Sdep table entry for this file

      Last_Sdep : Sdep_Id;
      --  Id of last Sdep table entry for this file

      GNAT_Version : Name_Id;
      --  GNAT version used to generate this file (first line in ALI)

      Main_Program : Main_Program_Type;
      --  Indicator of whether first unit can be used as main program. Not set
      --  if 'M' appears in Ignore_Lines.

      Main_Priority : Int;
      --  Indicates priority value if Main_Program field indicates that this
      --  can be a main program. A value of -1 (No_Main_Priority) indicates
      --  that no parameter was found, or no M line was present. Not set if
      --  'M' appears in Ignore_Lines.

      Main_CPU : Int;
      --  Indicates processor if Main_Program field indicates that this can
      --  be a main program. A value of -1 (No_Main_CPU) indicates that no C
      --  parameter was found, or no M line was present. Not set if 'M' appears
      --  in Ignore_Lines.

      Time_Slice_Value : Int;
      --  Indicates value of time slice parameter from T=xxx on main program
      --  line. A value of -1 indicates that no T=xxx parameter was found, or
      --  no M line was present. Not set if 'M' appears in Ignore_Lines.

      WC_Encoding : Character;
      --  Wide character encoding if main procedure. Otherwise not relevant.
      --  Not set if 'M' appears in Ignore_Lines.

      Locking_Policy : Character;
      --  Indicates locking policy for units in this file. Space means tasking
      --  was not used, or that no Locking_Policy pragma was present or that
      --  this is a language defined unit. Otherwise set to first character
      --  (upper case) of policy name. Not set if 'P' appears in Ignore_Lines.

      Partition_Elaboration_Policy : Character;
      --  Indicates partition elaboration policy for units in this file. Space
      --  means that no Partition_Elaboration_Policy pragma was present or that
      --  this is a language defined unit. Otherwise set to first character
      --  (upper case) of policy name. Not set if 'P' appears in Ignore_Lines.

      Queuing_Policy : Character;
      --  Indicates queuing policy for units in this file. Space means tasking
      --  was not used, or that no Queuing_Policy pragma was present or that
      --  this is a language defined unit. Otherwise set to first character
      --  (upper case) of policy name. Not set if 'P' appears in Ignore_Lines.

      Task_Dispatching_Policy : Character;
      --  Indicates task dispatching policy for units in this file. Space means
      --  tasking was not used, or that no Task_Dispatching_Policy pragma was
      --  present or that this is a language defined unit. Otherwise set to
      --  first character (upper case) of policy name. Not set if 'P' appears
      --  in Ignore_Lines.

      Compile_Errors : Boolean;
      --  Set to True if compile errors for unit. Note that No_Object will
      --  always be set as well in this case. Not set if 'P' appears in
      --  Ignore_Lines.

      No_Object : Boolean;
      --  Set to True if no object file generated. Not set if 'P' appears in
      --  Ignore_Lines.

      Normalize_Scalars : Boolean;
      --  Set to True if file was compiled with Normalize_Scalars. Not set if
      --  'P' appears in Ignore_Lines.

      SSO_Default : Character;
      --  Set to 'H' or 'L' if file was compiled with a configuration pragma
      --  file containing Default_Scalar_Storage_Order (High/Low_Order_First).
      --  Set to ' ' if neither pragma was present. Not set if 'P' appears in
      --  Ignore_Lines.

      Unit_Exception_Table : Boolean;
      --  Set to True if unit exception table pointer generated. Not set if 'P'
      --  appears in Ignore_Lines.

      Zero_Cost_Exceptions : Boolean;
      --  Set to True if file was compiled with zero cost exceptions. Not set
      --  if 'P' appears in Ignore_Lines.

      Restrictions : Restrictions_Info;
      --  Restrictions information reconstructed from R lines

   end record;

   No_Main_Priority : constant Int := -1;
   --  Code for no main priority set

   No_Main_CPU : constant Int := -1;
   --  Code for no main cpu set

   package ALIs is new GNAT.Table (
     Table_Component_Type => ALIs_Record,
     Table_Index_Type     => ALI_Id,
     Table_Low_Bound      => First_ALI_Entry,
     Table_Initial        => 500,
     Table_Increment      => 200);

   ----------------
   -- Unit Table --
   ----------------

   --  Each unit within an ALI file generates an entry in the unit table

   No_Unit_Id : constant Unit_Id := Unit_Id'First;
   --  Special value indicating no unit table entry

   First_Unit_Entry : constant Unit_Id := No_Unit_Id + 1;
   --  Id of first actual entry in table

   type Unit_Type is (Is_Spec, Is_Body, Is_Spec_Only, Is_Body_Only);
   --  Indicates type of entry, if both body and spec appear in the ALI file,
   --  then the first unit is marked Is_Body, and the second is marked Is_Spec.
   --  If only a spec appears, then it is marked as Is_Spec_Only, and if only
   --  a body appears, then it is marked Is_Body_Only).

   subtype Version_String is String (1 .. 8);
   --  Version string, taken from unit record

   type Unit_Record is record

      My_ALI : ALI_Id;
      --  Corresponding ALI entry

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file

      Preelab : Boolean;
      --  Indicates presence of PR parameter for a preelaborated package

      No_Elab : Boolean;
      --  Indicates presence of NE parameter for a unit that has does not
      --  have an elaboration routine (since it has no elaboration code).

      Pure : Boolean;
      --  Indicates presence of PU parameter for a package having pragma Pure

      Dynamic_Elab : Boolean;
      --  Set to True if the unit was compiled with dynamic elaboration checks
      --  (i.e. either -gnatE or pragma Elaboration_Checks (RM) was used to
      --  compile the unit).

      Elaborate_Body : Boolean;
      --  Indicates presence of EB parameter for a package which has a pragma
      --  Elaborate_Body, and also for generic package instantiations.

      Set_Elab_Entity : Boolean;
      --  Indicates presence of EE parameter for a unit which has an
      --  elaboration entity which must be set true as part of the
      --  elaboration of the unit.

      Has_RACW : Boolean;
      --  Indicates presence of RA parameter for a package that declares at
      --  least one Remote Access to Class_Wide (RACW) object.

      Remote_Types : Boolean;
      --  Indicates presence of RT parameter for a package which has a
      --  pragma Remote_Types.

      Serious_Errors : Boolean;
      --  Indicates presence of SE parameter indicating that compilation of
      --  the unit encountered as serious error.

      Shared_Passive : Boolean;
      --  Indicates presence of SP parameter for a package which has a pragma
      --  Shared_Passive.

      RCI : Boolean;
      --  Indicates presence of RC parameter for a package which has a pragma
      --  Remote_Call_Interface.

      Predefined : Boolean;
      --  Indicates if unit is language predefined (or a child of such a unit)

      Internal : Boolean;
      --  Indicates if unit is an internal unit (or a child of such a unit)

      First_With : With_Id;
      --  Id of first withs table entry for this file

      Last_With : With_Id;
      --  Id of last withs table entry for this file

      First_Arg : Arg_Id;
      --  Id of first args table entry for this file

      Last_Arg : Arg_Id;
      --  Id of last args table entry for this file

      Utype : Unit_Type;
      --  Type of entry

      Is_Generic : Boolean;
      --  True for generic unit (i.e. a generic declaration, or a generic
      --  body). False for a non-generic unit.

      Unit_Kind : Character;
      --  Indicates the nature of the unit. 'p' for Packages and 's' for
      --  subprograms.

      Version : Version_String;
      --  Version of unit

      Icasing : Casing_Type;
      --  Indicates casing of identifiers in source file for this unit. This
      --  is used for informational output, and also for constructing the main
      --  unit if it is being built in Ada.

      Kcasing : Casing_Type;
      --  Indicates casing of keywords in source file for this unit. This is
      --  used for informational output, and also for constructing the main
      --  unit if it is being built in Ada.

      Elab_Position : aliased Natural;
      --  Initialized to zero. Set non-zero when a unit is chosen and
      --  placed in the elaboration order. The value represents the
      --  ordinal position in the elaboration order.

      Init_Scalars : Boolean;
      --  Set True if IS qualifier appears in ALI file, indicating that
      --  an Initialize_Scalars pragma applies to the unit.

      SAL_Interface : Boolean;
      --  Set True when this is an interface to a standalone library

      Directly_Scanned : Boolean;
      --  True iff it is a unit from an ALI file specified to gnatbind

      Body_Needed_For_SAL : Boolean;
      --  Indicates that the source for the body of the unit (subprogram,
      --  package, or generic unit) must be included in a standalone library.

      Elaborate_Body_Desirable : Boolean;
      --  Indicates that the front end elaboration circuitry decided that it
      --  would be a good idea if this package had Elaborate_Body. The binder
      --  will attempt, but does not promise, to place the elaboration call
      --  for the body right after the call for the spec, or at least as close
      --  together as possible.

      Optimize_Alignment : Character;
      --  Optimize_Alignment setting. Set to L/S/T/O for OL/OS/OT/OO present

      Has_Finalizer : Boolean;
      --  Indicates whether a package body or a spec has a library-level
      --  finalization routine.
   end record;

   package Units is new GNAT.Table (
     Table_Component_Type => Unit_Record,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Entry,
     Table_Initial        => 100,
     Table_Increment      => 200);

   Stack_Check_Switch_Set : Boolean := False;
   --  Set to True if at least one ALI file contains '-fstack-check' in its
   --  argument list.

   -----------------
   -- Withs Table --
   -----------------

   --  Each With line (W line) in an ALI file generates a Withs table entry

   --  Note: there will be no entries in this table if 'W' lines are ignored

   No_With_Id : constant With_Id := With_Id'First;
   --  Special value indicating no withs table entry

   First_With_Entry : constant With_Id := No_With_Id + 1;
   --  Id of first actual entry in table

   type With_Record is record

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file, set to No_File in generic case

      Afile : File_Name_Type;
      --  Name of ALI file, set to No_File in generic case

      Elaborate : Boolean;
      --  Indicates presence of E parameter

      Elaborate_All : Boolean;
      --  Indicates presence of EA parameter

      Elab_All_Desirable : Boolean;
      --  Indicates presence of AD parameter

      Elab_Desirable     : Boolean;
      --  Indicates presence of ED parameter

      SAL_Interface : Boolean := False;
      --  True if the Unit is an Interface of a Stand-Alone Library

      Limited_With : Boolean := False;
      --  True if unit is named in a limited_with_clause

      Implicit_With_From_Instantiation : Boolean := False;
      --  True if this is an implicit with from a generic instantiation
   end record;

   package Withs is new GNAT.Table (
     Table_Component_Type => With_Record,
     Table_Index_Type     => With_Id,
     Table_Low_Bound      => First_With_Entry,
     Table_Initial        => 5000,
     Table_Increment      => 200);

   ---------------------
   -- Arguments Table --
   ---------------------

   --  Each Arg line (A line) in an ALI file generates an Args table entry

   --  Note: there will be no entries in this table if 'A' lines are ignored

   No_Arg_Id : constant Arg_Id := Arg_Id'First;
   --  Special value indicating no args table entry

   First_Arg_Entry : constant Arg_Id := No_Arg_Id + 1;
   --  Id of first actual entry in table

   package Args is new GNAT.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Arg_Id,
     Table_Low_Bound      => First_Arg_Entry,
     Table_Initial        => 1000,
     Table_Increment      => 100);

   ------------------------------------
   -- Sdep (Source Dependency) Table --
   ------------------------------------

   --  Each source dependency (D line) in an ALI file generates an entry in the
   --  Sdep table.

   --  Note: there will be no entries in this table if 'D' lines are ignored

   No_Sdep_Id : constant Sdep_Id := Sdep_Id'First;
   --  Special value indicating no Sdep table entry

   First_Sdep_Entry : Sdep_Id := No_Sdep_Id + 1;
   --  Id of first Sdep entry for current ali file. This is initialized to the
   --  first Sdep entry in the table, and then incremented appropriately as
   --  successive ALI files are scanned.

   type Sdep_Record is record

      Sfile : File_Name_Type;
      --  Name of source file

      Stamp : Time_Stamp_Type;
      --  Time stamp value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files.

      Checksum : Word;
      --  Checksum value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files

      Dummy_Entry : Boolean;
      --  Set True for dummy entries that correspond to missing files or files
      --  where no dependency relationship exists.

      Subunit_Name : Name_Id;
      --  Name_Id for subunit name if present, else No_Name

      Unit_Name : Name_Id;
      --  Name_Id for the unit name if not a subunit (No_Name for a subunit)
      Rfile : File_Name_Type;
      --  Reference file name. Same as Sfile unless a Source_Reference pragma
      --  was used, in which case it reflects the name used in the pragma.

      Start_Line : Nat;
      --  Starting line number in file. Always 1, unless a Source_Reference
      --  pragma was used, in which case it reflects the line number value
      --  given in the pragma.

   end record;

   package Sdep is new GNAT.Table (
     Table_Component_Type => Sdep_Record,
     Table_Index_Type     => Sdep_Id,
     Table_Low_Bound      => First_Sdep_Entry,
     Table_Initial        => 5000,
     Table_Increment      => 200);

   ----------------------------
   -- Use of Name Table Info --
   ----------------------------

   --  All unit names and file names are entered into the Names table. The Info
   --  fields of these entries are used as follows:

   --    Unit name           Info field has Unit_Id of unit table entry
   --    ALI file name       Info field has ALI_Id of ALI table entry
   --    Source file name    Info field has Source_Id of source table entry

   --------------------------------------
   -- Subprograms for Reading ALI File --
   --------------------------------------

   procedure Initialize_ALI;
   --  Initialize the ALI tables. Also resets all switch values to defaults

   function Scan_ALI
     (F           : File_Name_Type;
      T           : Text_Buffer_Ptr;
      Ignore_ED   : Boolean;
      Err         : Boolean;
      Read_Lines  : String;
      Object_Path : File_Name_Type := No_File) return ALI_Id;
   --  Given the text, T, of an ALI file, F, scan and store the information
   --  from the file, and return the Id of the resulting entry in the ALI
   --  table. Switch settings may be modified as described above in the
   --  switch description settings.
   --
   --    Ignore_ED is normally False. If set to True, it indicates that
   --    all AD/ED (elaboration desirable) indications in the ALI file are
   --    to be ignored. This parameter is obsolete now that the -f switch
   --    is removed from gnatbind, and should be removed ???
   --
   --    Err determines the action taken on an incorrectly formatted file.
   --    If Err is False, then an error message is output, and the program
   --    is terminated. If Err is True, then no error message is output,
   --    and No_ALI_Id is returned.
   --
   --    Ignore_Lines requests that Scan_ALI ignore any lines that start
   --    with any given key character. The default value of X causes all
   --    Xref lines to be ignored. The corresponding data in the ALI
   --    tables will not be filled in this case. It is not possible
   --    to ignore U (unit) lines, they are always read.
   --
   --    Read_Lines requests that Scan_ALI process only lines that start
   --    with one of the given characters. The corresponding data in the
   --    ALI file for any characters not given in the list will not be
   --    set. The default value of the null string indicates that all
   --    lines should be read (unless Ignore_Lines is specified). U
   --    (unit) lines are always read regardless of the value of this
   --    parameter.
   --
   --    Note: either Ignore_Lines or Read_Lines should be non-null, but not
   --    both. If both are provided then only the Read_Lines value is used,
   --    and the Ignore_Lines parameter is ignored.
   --
   --    Read_XREF is set True to read and acquire the cross-reference
   --    information. If Read_XREF is set to True, then the effect is to ignore
   --    all lines other than U, W, D and X lines and the Ignore_Lines and
   --    Read_Lines parameters are ignored (i.e. the use of True for Read_XREF
   --    is equivalent to specifying an argument of "UWDX" for Read_Lines.
   --
   --    Ignore_Errors is normally False. If it is set True, then Scan_ALI
   --    will do its best to scan through a file and extract all information
   --    it can, even if there are errors. In this case Err is only set if
   --    Scan_ALI was completely unable to process the file (e.g. it did not
   --    look like an ALI file at all). Ignore_Errors is intended to improve
   --    the downward compatibility of new compilers with old tools.
   --
   --    Directly_Scanned is normally False. If it is set to True, then the
   --    units (spec and/or body) corresponding to the ALI file are marked as
   --    such. It is used to decide for what units gnatbind should generate
   --    the symbols corresponding to 'Version or 'Body_Version in
   --    Stand-Alone Libraries.

end GPR.ALI;
