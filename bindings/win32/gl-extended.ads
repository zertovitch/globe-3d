--  This package contains extensions to GL as well as items
--  that are in the GL standard but are not (yet) in the GL libraries
--  on all platforms. For instance, standard Opengl32.dll on Windows up
--  to XP support up to GL 1.1; Vista, up to GL 1.4; and even versions
--  provided by graphics card makers lack 1.5 support (as in 2007).

--  *** Windows version -> uses GLEE (just link with glee.o) ***

package GL.Extended is

  --  Vertex buffer objects ("VBO"'s, GL 1.5 extension)
  --

  procedure GenBuffers (n       : in GL.Sizei;
                        buffers : in GL.uintPtr);

  procedure DeleteBuffers (n       : in GL.Sizei;
                           buffers : in GL.uintPtr);

  type VBO_Target_Type is
  (
    ARRAY_BUFFER,
    ELEMENT_ARRAY_BUFFER,
    PIXEL_PACK_BUFFER,
    PIXEL_UNPACK_BUFFER
  );
  for  VBO_Target_Type use
  (
   ARRAY_BUFFER             => 16#8892#,
   ELEMENT_ARRAY_BUFFER     => 16#8893#,
   PIXEL_PACK_BUFFER        => 16#88EB#,
   PIXEL_UNPACK_BUFFER      => 16#88EC#
  );

  procedure BindBuffer (target : in VBO_Target_Type;
                        buffer : in GL.Uint);

  type VBO_Usage is
  (
    STREAM_DRAW,
    STREAM_READ,
    STREAM_COPY,
    STATIC_DRAW,
    STATIC_READ,
    STATIC_COPY,
    DYNAMIC_DRAW,
    DYNAMIC_READ,
    DYNAMIC_COPY
  );
  for VBO_Usage use
  (
    STREAM_DRAW  => 16#88E0#,
    STREAM_READ  => 16#88E1#,
    STREAM_COPY  => 16#88E2#,
    STATIC_DRAW  => 16#88E4#,
    STATIC_READ  => 16#88E5#,
    STATIC_COPY  => 16#88E6#,
    DYNAMIC_DRAW => 16#88E8#,
    DYNAMIC_READ => 16#88E9#,
    DYNAMIC_COPY => 16#88EA#
  );

  procedure BufferData (target : in VBO_Target_Type;
                        size   : in GL.sizeiPtr;
                        data   : in GL.pointer;
                        usage  : in VBO_Usage);

  procedure BufferSubData (target : in VBO_Target_Type;
                           offset : in GL.intPtr;
                           size   : in GL.sizeiPtr;
                           data   : in GL.pointer);

   type Access_Policy is
   (
     READ_ONLY,
     WRITE_ONLY,
     READ_WRITE
   );
   for Access_Policy use
   (
     READ_ONLY  => 16#88B8#,
     WRITE_ONLY => 16#88B9#,
     READ_WRITE => 16#88BA#
   );

  function MapBuffer   (target : in VBO_Target_Type;
                        Policy : in Access_Policy) return GL.pointer;

  function UnmapBuffer (target : in VBO_Target_Type) return GL_Boolean;

  type Buffer_Parameter is
  (
    BUFFER_SIZE,
    BUFFER_USAGE,
    BUFFER_ACCESS,
    BUFFER_MAPPED
  );
  for Buffer_Parameter use
  (
    BUFFER_SIZE   => 16#8764#,
    BUFFER_USAGE  => 16#8765#,
    BUFFER_ACCESS => 16#88BB#,
    BUFFER_MAPPED => 16#88BC#
  );

  procedure GetBufferParameter (target : in VBO_Target_Type;
                                value  : in Buffer_Parameter;
                                data   : in intPointer);

  ------------------------------------------------------------------------------
  -- vertex buffer object imports (GL 1.5)
  --
  pragma Import (Stdcall, GenBuffers,         "_Lazy_glGenBuffers");
  pragma Import (Stdcall, DeleteBuffers,      "_Lazy_glDeleteBuffers");
  pragma Import (Stdcall, BindBuffer,         "_Lazy_glBindBuffer");
  pragma Import (Stdcall, BufferData,         "_Lazy_glBufferData");
  pragma Import (Stdcall, BufferSubData,      "_Lazy_glBufferSubData");
  pragma Import (Stdcall, MapBuffer,          "_Lazy_glMapBuffer");
  pragma Import (Stdcall, UnmapBuffer,        "_Lazy_glUnmapBuffer");
  pragma Import (Stdcall, GetBufferParameter, "_Lazy_glGetBufferParameteriv");

end GL.Extended;
