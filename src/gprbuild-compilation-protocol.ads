------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        G P R B U I L D . C O M P I L A T I O N . P R O T O C O L         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2012-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;           use GNAT;
with GNAT.Sockets;          use GNAT.Sockets;

with Gprbuild.Compilation.Process;

package Gprbuild.Compilation.Protocol is

   Wrong_Command : exception;
   --  Raised when a command cannot be parsed

   WD_Path_Tag : constant String := "<1>";
   --  The string replacing root working diretory of full path name, see
   --  Set_Rewrite below.

   CD_Path_Tag   : constant String := "<2>";
   --  The string replacing the compiler root directory, see Set_Rewrite below

   Any_OS : constant String := "any";
   --  Used when OS check is not necessary, for example gprclean does not need
   --  this check. It is safe to clean-up a Solaris slave from a Windows
   --  master.

   --
   --  Communication
   --

   type Communication_Channel is private;
   --  A communication channel, this channel is used for any communication
   --  between the build master and the slaves.

   No_Channel : constant Communication_Channel;

   function Create (Sock : Socket_Type) return Communication_Channel;
   --  Create a communication channel

   function Sock (Channel : Communication_Channel) return Socket_Type;
   pragma Inline (Sock);

   procedure Close (Channel : in out Communication_Channel);
   --  Close the channel

   procedure Set_Rewrite_WD
     (Channel : in out Communication_Channel; Path : String);
   --  Add rewrite information for the working directory. This is needed to
   --  translate paths to/from build master and slave working directories.

   procedure Set_Rewrite_CD
     (Channel : in out Communication_Channel; Path : String);
   --  Add rewrite information for the compiler directory. This is needed to
   --  translate paths to/from compilers path in build master and in slave.
   --  This is needed to be able to find the files from other projects
   --  installed with the compiler. The translated paths are in the
   --  gprbuild mapping file.

   procedure Clear_Rewrite (Channel : in out Communication_Channel);
   --  Remove any rewrite information from the channel

   function Translate_Receive
     (Channel : Communication_Channel; Str : String) return String;
   --  Translate Str using Channel rewrite

   function Translate_Send
     (Channel : Communication_Channel; Str : String) return String;
   --  Translate Str using Channel rewrite

   type Sync_Kind is (Rsync, File);
   --  The kind of synchronization used. It is either using rsync external tool
   --  or relying on shared directory (samba for example).

   --
   --  Command
   --

   type Command is private;

   type Command_Kind is
     (EX,  -- execute a command
      AK,  -- acknowledge received command (with pid)
      TS,  -- a file timestamp
      ES,  -- end of file timestamp
      FL,  -- a file
      OK,  -- compilation ok (with optional pid)
      KO,  -- compilation failed (with optional pid)
      CX,  -- master context
      CU,  -- clean-up request
      DP,  -- display output
      EC); -- end of compilation

   function Kind (Cmd : Command) return Command_Kind;
   pragma Inline (Kind);

   function Args (Cmd : Command) return OS_Lib.Argument_List_Access;
   pragma Inline (Args);
   --  Returns all arguments for Cmd

   function Output (Cmd : Command) return Unbounded_String;
   pragma Inline (Output);
   --  Returns the output for a DP command

   function Get_Command (Channel : Communication_Channel) return Command;
   --  Wait and return a command as parsed from the communication channel

   Invalid_Pid : constant := -1;

   --
   --  From gprbuild
   --

   procedure Send_Context
     (Channel      : Communication_Channel;
      Target       : String;
      Project_Name : String;
      Build_Env    : String;
      Sync         : Sync_Kind);
   --  Send initial context to the slave

   procedure Send_Exec
     (Channel  : Communication_Channel;
      Project  : String;
      Dir      : String;
      Command  : String;
      Options  : GNAT.OS_Lib.Argument_List;
      Dep_Name : String;
      Env      : String;
      Filter   : access function (Str, Sep : String) return String := null);
   --  Send a compilation job to a slave. The compilation must be done on
   --  Dir. This directory is specified relative to the root directory of
   --  the sources. Dep_Name is the dependency file that is generated by this
   --  compilation and which must be sent back to the build master after the
   --  compilation. Filter is a function used to make path on the command line
   --  all relatives to the root directory. The build master root in full path
   --  is replaced by Full_Path_Tag. The slaves will change back this tag to
   --  the actual full path on their working environment. The Env string is a
   --  set of environement variables (name=value[;name=value]) to set before
   --  spawning the process.

   procedure Send_File
     (Channel   : Communication_Channel;
      Path_Name : String);
   --  Path_Name is the full path name to the local filename

   procedure Sync_File
     (Channel   : Communication_Channel;
      Path_Name : String;
      Timestamp : Time_Stamp_Type);
   --  Send a filename and associated timestamp. Will receive a OK or KO if the
   --  file is to be transfered to the slave.

   procedure Send_End_Of_Compilation (Channel : Communication_Channel);
   --  Send an end of compilation signal, the slave will at this point be able
   --  to get jobs from another build master (Get_Context).

   procedure Send_End_Of_File_List (Channel : Communication_Channel);
   --  Send an end of file list signal, it means that all files timestamps have
   --  been checked. After this the compilation can be started.

   procedure Get_Pid
     (Channel : Communication_Channel;
      Pid     : out Process.Remote_Id;
      Success : out Boolean);
   --  Get a process id, Success is set to False in case of failure

   procedure Send_Clean_Up
     (Channel : Communication_Channel; Project_Name : String);
   --  Send a clean-up requets to the slave

   --
   --  From gprslave
   --

   procedure Get_Context
     (Channel      : Communication_Channel;
      Target       : out Unbounded_String;
      Project_Name : out Unbounded_String;
      Build_Env    : out Unbounded_String;
      Sync         : out Sync_Kind;
      Timestamp    : out Time_Stamp_Type;
      Version      : out Unbounded_String);
   --  Wait for an initial context from a build master

   procedure Send_Slave_Config
     (Channel        : Communication_Channel;
      Max_Process    : Positive;
      Root_Directory : String;
      Clock_Status   : Boolean);
   --  Send the slave configuration to the build master

   procedure Send_Ack
     (Channel : Communication_Channel; Pid : Process.Remote_Id);
   --  Send Acknoledgement of a received compilation job

   procedure Send_Ok
     (Channel : Communication_Channel; Pid : Process.Remote_Id);
   --  Send Pid of a successful command

   procedure Send_Ko
     (Channel : Communication_Channel; Pid : Process.Remote_Id);
   --  Send Pid of an un-successful command

   procedure Send_Ok (Channel : Communication_Channel);
   --  Send Ok for a successful command (clean-up for example)

   procedure Send_Ko (Channel : Communication_Channel);
   --  Send Ko to initial handshake (slave not compatible with master for
   --  example).

   procedure Send_Output (Channel : Communication_Channel; File_Name : String);
   --  Send an output of a command

   function Get_Raw_Data
     (Channel : Communication_Channel) return Stream_Element_Array;
   --  Get a Stream_Element_Array from the channel and return it

private

   type Communication_Channel is record
      Sock           : Socket_Type;
      Channel        : Stream_Access;
      WD_From, WD_To : Unbounded_String; -- working directory
      CD_From, CD_To : Unbounded_String; -- compiler directory
   end record;

   No_Channel : constant Communication_Channel :=
                  (Sock    => <>,
                   Channel => null,
                   others  => Null_Unbounded_String);

   type Command is record
      Cmd    : Command_Kind;
      Args   : Argument_List_Access;
      Output : Unbounded_String;
   end record;

end Gprbuild.Compilation.Protocol;
