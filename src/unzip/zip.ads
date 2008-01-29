--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip library
--------------
-- Library for manipulating archive files in the Zip format
--
-- Pure Ada 95+ code, 100% portable: OS- and compiler- independent.

-- Legal licensing note:

--  Copyright (c) 1999..2008 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Interfaces;
with Ada.Streams.Stream_IO;

package Zip is

  --------------
  -- Zip_info --
  --------------

  type Zip_info is private;
  -- contains the Zip file name and its sorted directory

  -- Load the whole .zip directory in archive (from) into a tree, for
  -- fast searching

  procedure Load(
    info           : out Zip_info;
    from           : in  String;
    case_sensitive : in  Boolean:= False
  );

  Zip_file_Error,
  Zip_file_open_Error,
  Duplicate_name: exception;

  function Is_loaded( info: in Zip_info ) return Boolean;

  function Zip_name( info: in Zip_info ) return String;

  function Entries( info: in Zip_info ) return Natural;

  procedure Delete( info : in out Zip_info );

  Forgot_to_load_zip_info: exception;

  -- Traverse a whole Zip_info directory in sorted order, giving the
  -- name for each entry to an user-defined "Action" procedure.
  generic
    with procedure Action( name: String );
  procedure Traverse( z: Zip_info );

  -- Academic: see how well the name tree is balanced
  procedure Tree_stat(
    z        : in     Zip_info;
    total    :    out Natural;
    max_depth:    out Natural;
    avg_depth:    out Float
  );

  ---------

  subtype Byte is Interfaces.Unsigned_8;
  type Byte_Buffer is array(Integer range <>) of Byte;
  type p_Byte_Buffer is access Byte_Buffer;

  -- Data sizes in archive
  subtype File_size_type is Interfaces.Unsigned_32;

  -- Compression methods
  type PKZip_method is
   ( store,    -- ok
     shrink,   -- ok
     reduce_1, -- ok
     reduce_2, -- ok
     reduce_3, -- ok
     reduce_4, -- ok
     implode,  -- ok
     tokenize, -- not implemented by PKWARE
     deflate,  -- ok
     unknown
   );

  -- Technical: translates the method code as set in zip archives
  function Method_from_code(x: Interfaces.Unsigned_16) return PKZip_method;

  --------------------------------------------------------------------------
  -- Offsets - various procedures giving 1-based indexes to local headers --
  --------------------------------------------------------------------------

  -- Find 1st offset in a Zip file (file opened and kept open)

  procedure Find_first_offset(
    file           : in     Ada.Streams.Stream_IO.File_Type;
    file_index     :    out Ada.Streams.Stream_IO.Positive_Count
  );

  -- Find offset of a certain compressed file
  -- in a Zip file (file opened and kept open)

  procedure Find_offset(
    file           : in     Ada.Streams.Stream_IO.File_Type;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Ada.Streams.Stream_IO.Positive_Count;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  -- Find offset of a certain compressed file in a Zip_info data

  procedure Find_offset(
    info           : in     Zip_info;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Ada.Streams.Stream_IO.Positive_Count;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  File_name_not_found: exception;

  procedure Get_sizes(
    info           : in     Zip_info;
    name           : in     String;
    case_sensitive : in     Boolean;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  -- General-purpose procedure (nothing really specific to Zip / UnZip):
  -- reads either the whole buffer from a file, or if the end of the file
  -- lays inbetween, a part of the buffer.

  procedure BlockRead(
    file         : in     Ada.Streams.Stream_IO.File_Type;
    buffer       :    out Zip.Byte_Buffer;
    actually_read:    out Natural
  );

private
  -- Zip_info, 23.VI.1999.

  -- The PKZIP central directory is coded here as a binary tree
  -- to allow a fast retrieval of the searched offset in zip file.
  -- E.g. for a 1000-file archive, the offset will be found in less
  -- than 11 moves: 2**10=1024 (balanced case), without any read
  -- in the archive.

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right : p_Dir_node;
    name        : String(1..name_len);
    file_index  : Ada.Streams.Stream_IO.Positive_Count;
    comp_size   : File_size_type;
    uncomp_size : File_size_type;
  end record;

  type p_String is access String;

  type Zip_info is record
    loaded          : Boolean:= False;
    zip_file_name   : p_String;
    dir_binary_tree : p_Dir_node;
    total_entries   : Natural;
  end record;

end Zip;
