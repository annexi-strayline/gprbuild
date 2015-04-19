------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
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

--  This package contains low level, operating system routines

package GPR.Osint is

      function Is_Directory_Separator (C : Character) return Boolean;
      --  Return True iff C is a directory separator inj a path

      function Get_Directory (Name : File_Name_Type) return File_Name_Type;
      --  Get the prefix directory name (if any) from Name. The last separator
      --  is preserved. Return the normalized current directory if there is no
      --  directory part in the name.

      function Executable_Name
        (Name              : File_Name_Type;
         Only_If_No_Suffix : Boolean := False) return File_Name_Type;
      --  Given a file name it adds the appropriate suffix at the end so that
      --  it becomes the name of the executable on the system at end. For
      --  instance under DOS it adds the ".exe" suffix, whereas under UNIX no
      --  suffix is added.

      function Strip_Suffix (Name : File_Name_Type) return File_Name_Type;
      --  Strips the suffix (the last '.' and whatever comes after it) from
      --  Name. Returns the stripped name.

      function Read_Library_Info
        (Lib_File  : File_Name_Type;
         Fatal_Err : Boolean := False) return Text_Buffer_Ptr;
      --  Allocates a Text_Buffer of appropriate length and reads in the entire
      --  source of the library information from the library information file
      --  whose name is given by the parameter Name.
      --
      --  See description of Read_Source_File for details on the format of the
      --  returned text buffer (the format is identical). The lower bound of
      --  the Text_Buffer is always zero
      --
      --  If the specified file cannot be opened, then the action depends on
      --  Fatal_Err. If Fatal_Err is True, an error message is given and the
      --  compilation is abandoned. Otherwise if Fatal_Err is False, then null
      --  is returned. Note that the Lib_File is a simple name which does not
      --  include any directory information. The implementation is responsible
      --  for searching for the file in appropriate directories.
      --
      --  If Opt.Check_Object_Consistency is set to True then this routine
      --  checks whether the object file corresponding to the Lib_File is
      --  consistent with it. The object file is inconsistent if the object
      --  does not exist or if it has an older time stamp than Lib_File. This
      --  check is not performed when the Lib_File is "locked" (i.e. read/only)
      --  because in this case the object file may be buried in a library. In
      --  case of inconsistencies Read_Library_Info behaves as if it did not
      --  find Lib_File (namely if Fatal_Err is False, null is returned).

      function Read_Library_Info_From_Full
        (Full_Lib_File : File_Name_Type;
         Lib_File_Attr : access File_Attributes;
         Fatal_Err     : Boolean := False) return Text_Buffer_Ptr;

      procedure Fail (S : String);
      pragma No_Return (Fail);
      --  Outputs error message S preceded by the name of the executing program
      --  and exits with E_Fatal. The output goes to standard error, except if
      --  special output is in effect (see Output).

      function Get_File_Names_Case_Sensitive return Int;
      pragma Import (C, Get_File_Names_Case_Sensitive,
                     "__gnat_get_file_names_case_sensitive");
      File_Names_Case_Sensitive : constant Boolean :=
        Get_File_Names_Case_Sensitive /= 0;
      --  Set to indicate whether the operating system convention is for file
      --  names to be case sensitive (e.g., in Unix, set True), or non case
      --  sensitive (e.g., in Windows, set False).

      function Get_Env_Vars_Case_Sensitive return Int;
      pragma Import (C, Get_Env_Vars_Case_Sensitive,
                     "__gnat_get_env_vars_case_sensitive");
      Env_Vars_Case_Sensitive : constant Boolean :=
        Get_Env_Vars_Case_Sensitive /= 0;
      --  Set to indicate whether the operating system convention is for
      --  environment variable names to be case sensitive (e.g., in Unix, set
      --  True), or non case sensitive (e.g., in Windows, set False).

      procedure Canonical_Case_File_Name (S : in out String);
      --  Given a file name, converts it to canonical case form. For systems
      --  where file names are case sensitive, this procedure has no effect.
      --  If file names are not case sensitive (i.e. for example if you have
      --  the file "xyz.adb", you can refer to it as XYZ.adb or XyZ.AdB), then
      --  this call converts the given string to canonical all lower case form,
      --  so that two file names compare equal if they refer to the same file.

      procedure Canonical_Case_Env_Var_Name (S : in out String);
      --  Given an environment variable name, converts it to canonical
      --  case form. For systems where environment variable names are case
      --  sensitive, this procedure has no effect. If environment variable
      --  names are not case sensitive, then this call converts the given
      --  string to canonical all lower case form, so that two environment
      --  variable names compare equal if they refer to the same environment
      --  variable.

      function File_Time_Stamp
        (Name : C_File_Name;
         Attr : access File_Attributes) return OS_Time;
      function File_Time_Stamp
        (Name : Path_Name_Type;
         Attr : access File_Attributes) return Time_Stamp_Type;
      --  Return the time stamp of the file

      function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type;
      --  Returns the time stamp of file Name. Name should include relative
      --  path information in order to locate it. If the source file cannot be
      --  opened, or Name = No_File, and all blank time stamp is returned (this
      --  is not an error situation).

      function File_Stamp (Name : Path_Name_Type) return Time_Stamp_Type;
      --  Same as above for a path name

      type Exit_Code_Type is
        (E_Success,    -- No warnings or errors
         E_Warnings,   -- Compiler warnings generated
         E_No_Code,    -- No code generated
         E_No_Compile, -- Compilation not needed (smart recompilation)
         E_Errors,     -- Compiler error messages generated
         E_Fatal,      -- Fatal (serious) error, e.g. source file not found
         E_Abort);     -- Internally detected compiler error

      procedure Exit_Program (Exit_Code : Exit_Code_Type);
      pragma No_Return (Exit_Program);
      --  A call to Exit_Program terminates execution with the given status.
      --  A status of zero indicates normal completion, a non-zero status
      --  indicates abnormal termination.

end GPR.Osint;