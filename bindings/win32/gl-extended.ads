-- This package contains extensions to GL as well as items
-- that are in the GL standard but are not (yet) in the GL libraries
-- on all platforms. For instance, standard Opengl32.dll on Windows up
-- to XP support up to GL 1.1; Vista, up to GL 1.4; and even versions
-- provided by graphics card makers lack 1.5 support (as in 2007).

-- *** Windows version -> uses GLEE (just link with glee.o) ***

with GL;

package GL.Extended is

  procedure GenBuffers (n       : in GL.Sizei;
                        buffers : in GL.uintPtr);

  procedure DeleteBuffers (n       : in GL.Sizei;
                           buffers : in GL.uintPtr);

  procedure BindBuffer (target : in GL.VBO_Target;
                        buffer : in GL.Uint);

  procedure BufferData (target : in GL.VBO_Target;
                        size   : in GL.sizeiPtr;
                        data   : in GL.pointer;
                        usage  : in GL.VBO_Usage);

  procedure BufferSubData (target : in GL.VBO_Target;
                           offset : in GL.intPtr;
                           size   : in GL.sizeiPtr;
                           data   : in GL.pointer);

  function MapBuffer   (target : in GL.VBO_Target;
                        Policy : in GL.Access_Policy) return GL.pointer;

  function UnmapBuffer (target : in GL.VBO_Target) return GL.GL_Boolean;

  procedure GetBufferParameter (target : in GL.VBO_Target;
                                value  : in GL.Buffer_Parameter;
                                data   : in GL.intPointer);

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
