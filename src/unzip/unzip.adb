with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Interfaces;                        use Interfaces;

with Zip.Headers, UnZip.Decompress;

package body UnZip is

  --------------------------------------------------
  -- *The* internal 1-file unzipping procedure.   --
  -- Input must be _open_ and won't be _closed_ ! --
  --------------------------------------------------

  procedure UnZipFile (
    zip_file                 : Ada.Streams.Stream_IO.File_Type;
    out_name                 : String;
    name_from_header         : Boolean;
    header_index             : in out Ada.Streams.Stream_IO.Positive_Count;
    hint_comp_size           : File_size_type; -- Added 2007 for .ODS files
    feedback                 : Feedback_proc;
    help_the_file_exists     : Resolve_conflict_proc;
    tell_data                : Tell_data_proc;
    get_pwd                  : Get_password_proc;
    options                  : Option_set;
    password                 : in out Unbounded_String;
    file_system_routines     : FS_routines_type
  )
  is
    work_index: Ada.Streams.Stream_IO.Positive_Count:= header_index;
    local_header: Zip.Headers.Local_File_Header;
    data_descriptor_present: Boolean;
    method: PKZip_method;

    function Exist(name:String) return Boolean is
      f: Ada.Streams.Stream_IO.File_Type;
    begin
      Ada.Streams.Stream_IO.Open(f,Ada.Streams.Stream_IO.In_File,name);
      Ada.Streams.Stream_IO.Close(f);
      return True;
    exception
      when Ada.Streams.Stream_IO.Name_Error =>
        return False;
    end Exist;

    skip_this_file: Boolean:= False;
    mode: constant array(Boolean) of Write_mode:= (write_to_file, just_test);
    actual_mode: Write_mode:= mode( options(test_only) );

    true_packed_size: File_size_type; -- encryption adds 12 to packed size

    the_output_name: Unbounded_String;

    -- 27-Jun-2001 : possibility of trashing directory part of a name
    --               e.g. :  unzipada\uza_src\unzip.ads -> unzip.ads
    function Maybe_trash_dir( n: String ) return String is
      idx: Integer:= n'First - 1;
    begin
      if options( junk_directories ) then
        for i in n'Range loop
          if n(i)= '/' or n(i)='\' then
            idx:= i;
          end if;
        end loop;
        -- idx points on the index just before the interesting part
        return n( idx+1 .. n'Last );
      else
        return n;
      end if;
    end Maybe_trash_dir;

    procedure Set_definitively_named_outfile( composed_name: String ) is
      idx: Integer:= composed_name'First - 1;
      first_in_name: Integer;
    begin
      for i in composed_name'Range loop
        if composed_name(i)= '/' or composed_name(i)='\' then
          idx:= i;
        end if;
      end loop;
      -- idx points on the index just before the name part

      if idx >= composed_name'First and then
         actual_mode = write_to_file and then
         file_system_routines.Create_Path /= null
      then
        -- Not only the name, also a path.
        -- In that case, we may need to create parts of the path.
        declare
          c: constant Character:=
            file_system_routines.Directory_Separator;
          path: String:= composed_name(composed_name'First..idx-1);
        begin
          -- Set the file separator recognized by the O.S.
          for i in path'Range loop
            if path(i)='\' or path(i)='/' then
              path(i):= c;
            end if;
          end loop;
          file_system_routines.Create_Path( path );
        end;
      end if;
      -- Now we can create the file itself.
      first_in_name:= composed_name'First;
      --
      the_output_name:=
        To_Unbounded_String(composed_name( first_in_name .. composed_name'Last ));
    end Set_definitively_named_outfile;

    function Full_Path_Name (Archive_File_Name : String) return String is
    begin
       if File_System_Routines.Compose_File_Name = null then
          return Archive_File_Name;
       else
          return File_System_Routines.Compose_File_Name.all (Archive_File_Name);
       end if;
    end Full_Path_Name;

    procedure Set_outfile( long_not_composed_name: String ) is
      -- Eventually trash the archived directory structure, then
      -- eventually add/modify/... another one:
      name: constant String:=
        Full_Path_Name( Maybe_trash_dir( long_not_composed_name ) );
    begin
      Set_definitively_named_outfile(name);
    end Set_outfile;

    procedure Set_outfile_interactive( long_not_composed_possible_name: String ) is
      -- Eventually trash the archived directory structure, then
      -- eventually add/modify/... another one:
      possible_name: constant String:=
        Full_Path_Name( Maybe_trash_dir( long_not_composed_possible_name ) );
      new_name : String( 1..1024 );
      new_name_length : Natural;
    begin
      if help_the_file_exists /= null and then Exist(possible_name) then
        loop
          case current_user_attitude is
            when yes | no | rename_it => -- then ask for this name too
              help_the_file_exists(
                possible_name,
                current_user_attitude,
                new_name, new_name_length
              );
            when yes_to_all | none | abort_now =>
              exit; -- nothing to decide: previous decision was definitive
          end case;
          exit when not (
            current_user_attitude = rename_it and then -- new name exists too!
            Exist( new_name( 1..new_name_length ) )
          );
        end loop;

        -- User has decided.
        case current_user_attitude is
          when yes | yes_to_all =>
            skip_this_file:= False;
            Set_definitively_named_outfile(possible_name);
          when no | none =>
            skip_this_file:= True;
          when rename_it =>
            skip_this_file:= False;
            Set_definitively_named_outfile(new_name( 1..new_name_length ));
          when abort_now =>
            raise User_abort;
        end case;

      else -- no name conflict or non-interactive (help_the_file_exists=null)

        skip_this_file:= False;
        Set_definitively_named_outfile(possible_name);
      end if;
    end Set_outfile_interactive;

    procedure Inform_User(
      name: String;
      comp, uncomp: File_size_type
    )
    is
    begin
      if tell_data /= null  then
        tell_data( name, comp, uncomp, method );
      end if;
    end Inform_User;

    the_name    : String(1..1000);
    the_name_len: Natural;
    use Ada.Streams.Stream_IO, UnZip.Decompress, Zip;

    actual_feedback: UnZip.Feedback_proc;

    dummy: p_Stream_Element_Array;
    encrypted: Boolean;

  begin
    begin
      Ada.Streams.Stream_IO.Set_Index ( zip_file, work_index);
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

    method:= Zip.Method_from_code(local_header.zip_type);
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
      -- Sizes and CRC are stored after the data
      -- We set size to avoid getting a sudden Zip_EOF !
      local_header.dd.crc_32            := 0;
      local_header.dd.compressed_size   := hint_comp_size;
      local_header.dd.uncompressed_size := File_size_type'Last;
      actual_feedback := null; -- no feedback possible: unknown sizes
    else
      -- Sizes and CRC are stored before the data, inside the local header
      actual_feedback:= feedback; -- use the given feedback procedure
    end if;

    encrypted:= (local_header.bit_flag and 1) /= 0;

    -- 13-Dec-2002
    true_packed_size:= File_size_type(local_header.dd.compressed_size);
    if encrypted then
      true_packed_size:= true_packed_size - 12;
    end if;

    if name_from_header then
      the_name_len:= Natural(local_header.filename_length);
      declare
        b: Unsigned_8;
      begin
        for i in 1..the_name_len loop
          Unsigned_8'Read( Ada.Streams.Stream_IO.Stream(zip_file), b );
          the_name(i):= Character'Val( Natural(b) );
        end loop;
      end;
      if not data_descriptor_present then
        Inform_User(
          the_name(1..the_name_len),
          true_packed_size,
          File_size_type(local_header.dd.uncompressed_size)
        );
      end if;
      if the_name_len = 0 or else the_name( the_name_len ) = '/' then
        -- This is a directory name (12-feb-2000)
        skip_this_file:= True;
      elsif actual_mode = write_to_file then
        Set_outfile_interactive( the_name(1..the_name_len));
      else -- only informational, no need for interaction
        Set_outfile(the_name(1..the_name_len));
      end if;
    else
      if not data_descriptor_present then
        Inform_User(
          out_name,
          true_packed_size,
          File_size_type(local_header.dd.uncompressed_size)
        );
      end if;
      if actual_mode = write_to_file then
        Set_outfile_interactive(out_name);
      else -- only informational, no need for interaction
        Set_outfile(out_name);
      end if;
    end if;

    if skip_this_file then
      actual_mode := just_test;
    end if;

    if skip_this_file and not data_descriptor_present then
      null; -- we can skip actually
    else
      begin
        Set_Index ( zip_file, work_index ); -- eventually skips the file name
      exception
        when others => raise Read_Error;
      end;
      UnZip.Decompress.Decompress_data(
        zip_file             => zip_file,
        format               => method,
        mode                 => actual_mode,
        output_file_name     => To_String(the_output_name),
        output_memory_access => dummy,
        feedback             => actual_feedback,
        explode_literal_tree => (local_header.bit_flag and 4) /= 0,
        explode_slide_8KB    => (local_header.bit_flag and 2) /= 0,
        end_data_descriptor  => data_descriptor_present,
        encrypted            => encrypted,
        password             => password,
        get_new_password     => get_pwd,
        hint                 => local_header.dd
      );

      if file_system_routines.Set_Time_Stamp /= null then
        file_system_routines.Set_Time_Stamp(
          To_String(the_output_name),
          local_header.file_timedate
        );
      end if;

      if data_descriptor_present then -- Sizes and CRC at the end
        -- Inform after decompression
        Inform_User(
          To_String(the_output_name),
          local_header.dd.compressed_size,
          local_header.dd.uncompressed_size
        );
      end if;

    end if; -- not ( skip_this_file and not data_descriptor )

    -- Set the offset on the next zipped file
    header_index:= header_index +
      Ada.Streams.Stream_IO.Count(
        File_size_type(
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
        ) +
        local_header.dd.compressed_size
      );

    if data_descriptor_present then
      header_index:=
        header_index +
        Ada.Streams.Stream_IO.Count(Zip.Headers.data_descriptor_length);
    end if;

  end UnZipFile;

  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  -- Extract all files from an archive (from)

  procedure Extract( from                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : String;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : String;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, rename, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, rename, null, null, null,
             options, password, file_system_routines );
  end Extract;

  -- All previous extract call the following ones, with bogus UI arguments

  ------------------------------------------------------------
  -- All previous extraction procedures, for user interface --
  ------------------------------------------------------------

  use Ada.Streams.Stream_IO;

  procedure Extract( from                 : String;
                     what                 : String;
                     feedback             : Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                )
  is
    zip_file     : File_Type;
    header_index : Positive_Count;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Open(zip_file, In_File, from);
    Zip.Find_offset(
      zip_file,
      what,options( case_sensitive_match ),
      header_index,
      comp_size,
      uncomp_size
    );
    UnZipFile(
      zip_file,
      what, False,
      header_index,
      comp_size,
      feedback, help_the_file_exists, tell_data, get_pwd,
      options,
      work_password,
      file_system_routines
    );
    Close(zip_file);
  end Extract;

  procedure Extract( from                 : String;
                     what                 : String;
                     rename               : String;
                     feedback             : Feedback_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    zip_file     : File_Type;
    header_index : Positive_Count;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Open(zip_file, In_File, from);
    Zip.Find_offset(
      zip_file,
      what,options( case_sensitive_match ),
      header_index,
      comp_size,
      uncomp_size
    );
    UnZipFile(
      zip_file,
      rename, False,
      header_index,
      comp_size,
      feedback, null, tell_data, get_pwd,
      options,
      work_password,
      file_system_routines
    );
    Close(zip_file);
  end Extract;

  procedure Extract( from                 : String;
                     feedback             : Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    zip_file     : File_Type;
    header_index : Positive_Count;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Open(zip_file, In_File, from);
    Zip.Find_first_offset(zip_file, header_index); -- >= 13-May-2001
    -- We simply unzip everything sequentially, until the end:
    all_files: loop
      UnZipFile(
        zip_file,
        "", True,
        header_index,
        File_size_type'Last,
        -- ^ no better hint available if comp_size is 0 in local header
        feedback, help_the_file_exists, tell_data, get_pwd,
        options,
        work_password,
        file_system_routines
      );
    end loop all_files;
  exception
    when Zip.Headers.bad_local_header =>
      Close(zip_file); -- normal case: end was hit
    when Zip.Zip_file_open_Error =>
      raise;    -- couldn't open zip file
    when others =>
      Close(zip_file);
      raise;    -- something else wrong
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     feedback             : Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    zip_file     : File_Type;
    header_index : Positive_Count;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Zip.Find_offset(
      from,what,options( case_sensitive_match ),
      header_index,
      comp_size,
      uncomp_size
    );
    -- search offset "offline" !
    Open(zip_file, In_File, Zip.Zip_name(from) );
    UnZipFile(
      zip_file,
      what, False,
      header_index,
      comp_size,
      feedback, help_the_file_exists, tell_data, get_pwd,
      options,
      work_password,
      file_system_routines
    );
    Ada.Streams.Stream_IO.Close(zip_file);
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     rename               : String;
                     feedback             : Feedback_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    zip_file     : File_Type;
    header_index : Positive_Count;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Zip.Find_offset(
      from,what,options( case_sensitive_match ),
      header_index,
      comp_size,
      uncomp_size
    );
    -- search offset "offline" !
    Open(zip_file, In_File, Zip.Zip_name(from) );
    UnZipFile(
      zip_file,
      rename, False,
      header_index,
      comp_size,
      feedback, null, tell_data, get_pwd,
      options,
      work_password,
      file_system_routines
    );
    Ada.Streams.Stream_IO.Close(zip_file);
  end Extract;

end UnZip;
