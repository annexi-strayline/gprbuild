------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2023, Free Software Foundation, Inc.         --
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

--  This package manages the communication with GNU make jobserver

with Ada.Strings.Hash;
with GPR.Compilation; use GPR.Compilation;
with Ada.Containers.Indefinite_Hashed_Maps;

package GPR.Jobserver is

   JS_Initialize_Error                  : exception;
   --  Error exception raised when jobserver's initialization fails
   JS_Makeflags_Parsing_Detects_Dry_Run : exception;
   --  Exception raised when make was invoked with "-n"
   JS_Access_Error                      : exception;
   --  Error exception raised when jobserver's read or write fails

   procedure Initialize;
   --  Initialize Jobserver communication

   function Preorder_Token return Boolean;
   --  Returns True if there is an available token on the current jobserver,
   --  returns False if there is not.

   procedure Register_Token_Id (Id : GPR.Compilation.Id);
   --  Affiliates the last preordered token to the process Id

   procedure Unregister_All_Token_Id;
   --  Free all registered tokens

   procedure Unregister_Token_Id (Id : GPR.Compilation.Id);
   --  Release the token affiliated to the process Id

   function Registered_Processes return Boolean;
   --  Returns True if there are ongoing processes affiliated with a token,
   --  returns False if there are not.

   function Unavailable_Token return Boolean;
   --  Returns True if the last attempt of preordering a token failed,
   --  returns False otherwise.

private

   package Token_Process_Id is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Character, Ada.Strings.Hash, "=");
   Source_Id_Token_Map : Token_Process_Id.Map;

   type Connection_Type is
     (Undefined, Named_Pipe, Simple_Pipe, Windows_Semaphore);
   Current_Connection_Method : Connection_Type := Undefined;

   type Implemented_Connection_Type is array (Connection_Type) of Boolean;

   type Token_Status is (Not_Needed, Unavailable);
   Default_Token_Status : constant Token_Status := Not_Needed;
   Last_Token_Status    : Token_Status          := Default_Token_Status;

   Char : aliased Character := ASCII.NUL;

end GPR.Jobserver;
