-- This package contains extensions to GL as well as items
-- that are in the GL standard but are not (yet) in the GL libraries
-- on all platforms. For instance, standard Opengl32.dll on Windows up
-- to XP support up to GL 1.1; Vista, up to GL 1.4; and even versions
-- provided by graphics card makers lack 1.5 support (as in 2007).

-- *** Non-Windows version (just part of GL) ***

with GL;

package GL.Extended is


  procedure GenBuffers (n       : in GL.SizeI;
                        buffers : in GL.uintPtr);

  procedure DeleteBuffers (n       : in GL.SizeI;
                           buffers : in GL.uintPtr);

  procedure BindBuffer (target : in GL.VBO_Target;
                        buffer : in gl.uInt);

  procedure BufferData (target : in GL.vbo_Target;
                        size   : in GL.SizeIPtr;
                        data   : in GL.Pointer;
                        usage  : in GL.VBO_Usage);

  procedure BufferSubData (target : in GL.vbo_Target;
                           offset : in GL.intPtr;
                           size   : in GL.SizeIPtr;
                           data   : in GL.Pointer);

  function MapBuffer   (target : in GL.vbo_Target;
                        Policy : in GL.Access_Policy) return gl.Pointer;

  function UnmapBuffer (target : in GL.vbo_Target) return GL.GL_Boolean;


  procedure GetBufferParameter (target : in GL.vbo_Target;
                                value  : in GL.Buffer_Parameter;
                                data   : in GL.intPointer);

  -- vertex buffer object imports (GL 1.5)
  --
  pragma Import (Stdcall, GenBuffers,         "glGenBuffers");
  pragma Import (Stdcall, DeleteBuffers,      "glDeleteBuffers");
  pragma Import (Stdcall, BindBuffer,         "glBindBuffer");
  pragma Import (Stdcall, BufferData,         "glBufferData");
  pragma Import (Stdcall, BufferSubData,      "glBufferSubData");
  pragma Import (Stdcall, MapBuffer,          "glMapBuffer");
  pragma Import (Stdcall, UnmapBuffer,        "glUnmapBuffer");
  pragma Import (Stdcall, GetBufferParameter, "glGetBufferParameteriv");

end GL.Extended;