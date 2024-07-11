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

with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;
with Interfaces.C; use Interfaces.C;

with System.Win32;

package body GPR.Jobserver is

   type Handle is new Integer;
   subtype DWord is Interfaces.Unsigned_32;

   HR, HW : File_Descriptor;
   HRW : File_Descriptor;
   Semaphore : Handle;

   Current_Implemented_Connection : constant Implemented_Connection_Type :=
                                      (Windows_Semaphore => True,
                                       others            => False);

   procedure Release (Token : Character);
   --  Release the token to the pipe of the jobserver

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use type Interfaces.Unsigned_32;

      Makeflags             : constant String := Value ("MAKEFLAGS", "");
      JS_Auth               : constant String := "--jobserver-auth=";
      Simple_Pipe_Delimiter : constant String := ",";
      Named_Pipe_Delimiter  : constant String := "fifo:";
      Dry_Run               : constant String := "n";

      function Open_Semaphore
        (Desired_Access : DWord;
         Inherit_Handle : Boolean;
         Name           : System.Address) return Handle
        with Import, Convention => Stdcall, External_Name => "OpenSemaphoreA";

      Idx, Idx0 : Natural := 0;

      procedure Initialize_Connection (Method : Connection_Type);
      --  Try all known ways to connect to a jobserver

      ---------------------------
      -- Initialize_Connection --
      ---------------------------

      procedure Initialize_Connection (Method : Connection_Type) is
         Idx_Tmp  : Natural := Idx;
         Idx0_Tmp : Natural := Idx0;
      begin

         case Method is
            when Windows_Semaphore =>

               Idx_Tmp := Idx_Tmp + JS_Auth'Length;
               Idx0_Tmp := Index (Makeflags, " ", From => Idx_Tmp);

               if Idx0 = 0 then
                  Idx0_Tmp := Makeflags'Last;
               else
                  Idx0_Tmp := Idx0_Tmp + 1;
               end if;

               declare
                  Sem_Name : constant String :=
                               Makeflags (Idx_Tmp .. Idx0_Tmp) & ASCII.NUL;
               begin
                  Semaphore :=
                    Open_Semaphore (16#1F0003#, False, Sem_Name'Address);

                  if Semaphore = 0 then
                     return;
                  end if;
               end;

            when Undefined | Named_Pipe | Simple_Pipe =>
               null;
         end case;

         Current_Connection_Method := Method;

      end Initialize_Connection;
   begin
      if Makeflags = "" then
         raise JS_Initialize_Error
           with "Connecting to a jobserver requires MAKEFLAGS information";
      end if;

      Idx := Index (Makeflags, " ");
      Idx := Index (Makeflags (Makeflags'First .. Idx - 1), Dry_Run);

      if Idx /= 0 then
         raise JS_Makeflags_Parsing_Detects_Dry_Run;
      end if;

      Idx := Index (Makeflags, JS_Auth, Going => Ada.Strings.Backward);

      if Idx = 0 then
         raise JS_Initialize_Error
           with "Wrong MAKEFLAGS information while attempting to connect to a "
           & "jobserver";
      end if;

      for Connection_Method in Connection_Type loop
         if Current_Implemented_Connection (Connection_Method) then
            Initialize_Connection (Method => Connection_Method);
         end if;
         exit when (Current_Connection_Method /= Undefined);
      end loop;

      if Current_Connection_Method = Undefined then
         raise JS_Initialize_Error with "Unable to connect to a jobserver";
      end if;

   end Initialize;

   --------------------
   -- Preorder_Token --
   --------------------

   function Preorder_Token return Boolean is
      use type Interfaces.Unsigned_32;

      function Wait_For_Object
        (Semaphore : Handle; Milliseconds : DWord) return DWord
        with Import, Convention => Stdcall,
        External_Name => "WaitForSingleObject";
   begin
      Last_Token_Status := Unavailable;

      case Current_Connection_Method is
         when Windows_Semaphore =>
            if Wait_For_Object (Semaphore, 0) /= 0 then
               return False;
            end if;

            Char := '+';
         when Undefined | Named_Pipe | Simple_Pipe =>
            null;
      end case;

      return True;
   end Preorder_Token;

   -----------------------
   -- Register_Token_Id --
   -----------------------

   procedure Register_Token_Id (Id : GPR.Compilation.Id) is
      Key : constant String := (if Id.Kind = Local
                                then Pid_To_Integer (Id.Pid)'Img & "-Local"
                                else Id.R_Pid'Img & "-Remote");
   begin
      Source_Id_Token_Map.Insert (Key, Char);
      Last_Token_Status := Default_Token_Status;
   end Register_Token_Id;

   -------------
   -- Release --
   -------------

   procedure Release (Token : Character) is
      function  Release_Semaphore
        (Semaphore      : Handle;
         Release_Count  : Long_Integer;
         Previous_Count : access Long_Integer) return Boolean
        with Import, Convention => Stdcall,
             External_Name => "ReleaseSemaphore";
   begin
      case Current_Connection_Method is
         when Windows_Semaphore =>
            if not Release_Semaphore (Semaphore, 1, null) then
               raise JS_Access_Error with Errno_Message;
            end if;
         when Undefined | Named_Pipe | Simple_Pipe =>
            null;
      end case;
   end Release;

   --------------------------
   -- Registered_Processes --
   --------------------------

   function Registered_Processes return Boolean is
     (not Source_Id_Token_Map.Is_Empty);

   -----------------------
   -- Unavailable_Token --
   -----------------------

   function Unavailable_Token return Boolean is
     (Last_Token_Status = Unavailable);

   -----------------------------
   -- Unregister_All_Token_Id --
   -----------------------------

   procedure Unregister_All_Token_Id is
      Cursor : Token_Process_Id.Cursor;
   begin
      while not Source_Id_Token_Map.Is_Empty loop
         Cursor := Source_Id_Token_Map.First;
         Release (Token => Token_Process_Id.Element (Position => Cursor));
         Source_Id_Token_Map.Delete (Position => Cursor);
      end loop;
   end Unregister_All_Token_Id;

   -------------------------
   -- Unregister_Token_Id --
   -------------------------

   procedure Unregister_Token_Id (Id : GPR.Compilation.Id) is
      Key : constant String := (if Id.Kind = Local
                                then Pid_To_Integer (Id.Pid)'Img & "-Local"
                                else Id.R_Pid'Img & "-Remote");
   begin
      Release (Token => Source_Id_Token_Map.Element (Key));
      Source_Id_Token_Map.Delete (Key);
   end Unregister_Token_Id;

end GPR.Jobserver;
