with Zip.Headers, UnZip.Decompress;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;

package body UnZip.Streams is

   procedure Dispose is new
     Ada.Unchecked_Deallocation( String, p_String );

   procedure Dispose is new
     Ada.Unchecked_Deallocation( Ada.Streams.Stream_Element_Array,
                                 p_Stream_Element_Array );

   procedure Dispose is new
     Ada.Unchecked_Deallocation( Unzip_Stream_Type,
                                 Zipped_File_Type );

  --------------------------------------------------
  -- *The* internal 1-file unzipping procedure.   --
  -- Input must be _open_ and won't be _closed_ ! --
  --------------------------------------------------

  procedure UnZipFile (
    zip_file        :        Ada.Streams.Stream_IO.File_Type;
    header_index    : in out Ada.Streams.Stream_IO.Positive_Count;
    mem_ptr         :    out p_Stream_Element_Array;
    password        : in out Ada.Strings.Unbounded.Unbounded_String;
    hint_comp_size  : in     File_size_type; -- Added 2007 for .ODS files
    cat_uncomp_size : in     File_size_type
  )
  is
    work_index: Ada.Streams.Stream_IO.Positive_Count:= header_index;
    local_header: Zip.Headers.Local_File_Header;
    data_descriptor_present: Boolean;
    encrypted: Boolean;
    method: PKZip_method;
    use Ada.Streams.Stream_IO, Zip;
  begin
    begin
      Ada.Streams.Stream_IO.Set_Index ( zip_file, header_index );
      Zip.Headers.Read_and_check(
        Ada.Streams.Stream_IO.Stream(zip_file).all,
        local_header
      );
    exception
      when Zip.Headers.bad_local_header =>
        raise;
      when others =>
        raise Read_Error;
    end;

    method:= Method_from_code(local_header.zip_type);
    if method = unknown then
      raise Unsupported_method;
    end if;

    -- calculate offset of data

    work_index :=
      work_index + Ada.Streams.Stream_IO.Count(
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
      );

    data_descriptor_present:= (local_header.bit_flag and 8) /= 0;

    if data_descriptor_present then
      -- Sizes and crc are after the data
      local_header.dd.crc_32 := 0;
      local_header.dd.uncompressed_size := cat_uncomp_size;
      local_header.dd.compressed_size   := hint_comp_size;
    else
      -- Sizes and crc are before the data
      if cat_uncomp_size /= local_header.dd.uncompressed_size then
        raise Uncompressed_size_Error;
      end if;
    end if;

    encrypted:= (local_header.bit_flag and 1) /= 0;

    begin
      Set_Index ( zip_file, work_index ); -- eventually skips the file name
    exception
      when others => raise Read_Error;
    end;

    -- Unzip correct type

    UnZip.Decompress.Decompress_data(
      zip_file             => zip_file,
      format               => method,
      mode                 => write_to_memory,
      output_file_name     => "",
      output_memory_access => mem_ptr,
      feedback             => null,
      explode_literal_tree => (local_header.bit_flag and 4) /= 0,
      explode_slide_8KB    => (local_header.bit_flag and 2) /= 0,
      end_data_descriptor  => data_descriptor_present,
      encrypted            => encrypted,
      password             => password,
      get_new_password     => null,
      hint                 => local_header.dd
    );

    -- Set the offset on the next zipped file
    header_index:= header_index +
      Count(
        File_size_type(
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
        ) +
        local_header.dd.compressed_size
      );

    if data_descriptor_present then
      header_index:= header_index + Count(Zip.Headers.data_descriptor_length);
    end if;

  end UnZipFile;

  use Ada.Streams.Stream_IO;

  procedure S_Extract( from     : Zip.Zip_info;
                       what     : String;
                       mem_ptr  : out p_Stream_Element_Array;
                       Password : in String
                 ) is

    zip_file     : File_Type;
    header_index : Positive_Count;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    work_password: Ada.Strings.Unbounded.Unbounded_String:=
      Ada.Strings.Unbounded.To_Unbounded_String(password);
  begin
    Zip.Find_offset(
      from, what, False,
      header_index,
      comp_size,
      uncomp_size
    );
    -- search offset "offline" !
    Open(zip_file, In_File, Zip.Zip_name(from) );
    UnZipFile(
      zip_file,
      header_index,
      mem_ptr,
      work_password,
      comp_size,
      uncomp_size
    );
    Close(zip_file);
  end S_Extract;

  -------------------- for exportation:

  procedure Close (File : in out Zipped_File_Type) is
  begin
    if File = null or else File.state = uninitialized then
       raise Use_Error;
    end if;
    if File.delete_info_on_closing then
      Zip.Delete( File.archive_info );
    end if;
    Dispose(File.file_name);
    Dispose(File.uncompressed);
    Dispose(File);
    File:= null;
  end Close;

  function Is_Open (File : in Zipped_File_Type) return Boolean is
  begin
    return File /= null and then File.state /= uninitialized;
  end Is_Open;

  function End_Of_File (File : in Zipped_File_Type) return Boolean is
  begin
    if File = null or else File.state = uninitialized then
       raise Use_Error;
    end if;
    return File.state = end_of_zip;
  end End_Of_File;

  procedure Open
    (File         : in out Zipped_File_Type; -- File-in-archive handle
     Archive_Info : in Zip.Zip_info;             -- loaded by Load_zip_info
     Name         : in String;               -- Name of zipped entry
     Password     : in String := ""          -- Decryption password
    )
  is
    use Ada.Streams;
  begin
    if File = null then
      File:= new Unzip_Stream_Type;
    elsif File.state /= uninitialized then -- forgot to close last time!
      raise Use_Error;
    end if;
    File.archive_info:= Archive_Info;
    File.file_name:= new String' (Name);
    S_Extract( from    =>   File.archive_info,
               what    =>   Name,
               mem_ptr =>   File.uncompressed,
               Password=>   Password );

    File.index:= File.uncompressed'First;
    File.state:= data_uncompressed;
    -- Bug fix for data of size 0 - 29-Nov-2002
    if File.uncompressed'Last < File.index then -- (1..0) array
      File.state:= end_of_zip;
    end if;
    File.delete_info_on_closing:= False; -- Close won't delete dir tree
    -- Bug fix 1-Mar-2007: False was set only at initialization
  end Open;

  procedure Open
    (File         : in out Zipped_File_Type; -- File-in-archive handle
     Archive_Name : in String;               -- Name of archive file
     Name         : in String;               -- Name of zipped entry
     Password     : in String := ""          -- Decryption password
    )
  is
    temp_info: Zip.Zip_info;
    -- this local record (but not the full tree) is copied by Open(..)
  begin
    Zip.Load( temp_info, Archive_Name );
    Open( File, temp_info, Name, Password );
    File.delete_info_on_closing:= True; -- Close will delete temp. dir tree
  end Open;

  procedure Read
    (Stream : in out Unzip_Stream_Type;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset)
  is
    use Ada.Streams;
  begin
    if Stream.state = uninitialized then
      raise Use_Error;
    end if;
    for i in Item'Range loop
      if Stream.state = end_of_zip then
        raise End_Error;
      end if;
      Item(i):= Stream.uncompressed(Stream.index);
      Last:= i;
      Stream.index:= Stream.index + 1;
      if Stream.index > Stream.uncompressed'Last then
        Stream.state:= end_of_zip;
      end if;
    end loop;
  end Read;


  function Stream (File : Zipped_File_Type) return Stream_Access is
  begin
    return Stream_Access(File);
  end Stream;

  procedure Write
    (Stream : in out Unzip_Stream_Type;
     Item   : in     Ada.Streams.Stream_Element_Array)
  is
    write_not_supported: exception;
  begin
    raise write_not_supported;
  end Write;

end UnZip.Streams;
