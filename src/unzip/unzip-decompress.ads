--  ___  ____  ____  ____  ________  ___   ______       ______     ___
--  |.|  |../  |...\ |../ /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--  |.|  |.|   |.|\.\|.|     /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  |.|__|.|   |.| \...|   _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
--  |______|  /__|  \__|  /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip.Decompress
-------------------
-- Private, internal to UnZip-Ada.
--
-- Created 9-Mar-2007
--
-- This package includes the decompression algorithms for methods
-- store, reduce, shrink (LZW), implode and inflate.
-- It contains the packages UnZ_IO, UnZ_Glob, UnZ_Infl, UnZ_Olds, UnZ_Misc
-- of previous versions of UnZip-Ada.
-- They become local packages inside the Decompress_Data procedure ->
-- previously global variables are now local, one copy per concurrent call.

with Zip.Headers;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

private package UnZip.Decompress is

  procedure Decompress_data(
    zip_file            : Ada.Streams.Stream_IO.File_Type;
    -- zip_file must be open and its index is meant
    -- to point to the beginning of compressed data
    format              : PKZip_method;
    mode                : Write_mode;
    output_file_name    : String; -- relevant only if mode = write_to_file
    output_memory_access: out p_Stream_Element_Array; --   = write_to_memory
    feedback            : Feedback_proc;
    explode_literal_tree: Boolean; -- relevant for the "explode" format
    explode_slide_8KB   : Boolean; -- relevant for the "explode" format
    end_data_descriptor : Boolean;
    encrypted           : Boolean;
    password            : in out Unbounded_String;
    get_new_password    : Get_password_proc; -- if null, initial pwd must fit
    hint                : in out Zip.Headers.Data_descriptor
    -- values are known, or smart fakes and corrected if a closing
    -- Data_descriptor is appended to the compressed data (1-pass written
    -- zip files, like JAR, OpenDocument, etc.)
  );

private

  trace: constant Boolean:= False; -- Primitive tracing with Ada.Text_IO;

end UnZip.Decompress;