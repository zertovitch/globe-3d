-- Change log:

-- GdM: 28-Nov-2005: replaced Unrestricted_Access with Address
--                   since Unrestricted_Access is GNAT-Specific

-- GdM: 27-Jan-2004: Added Material_Float_vector and Material(...) for it

-- GdM: 11-Apr-2002 : * "gl..." and other useless C prefixes removed
--                    * removing of pointers and
--                      "...4f" -style suffixes in progress

with Ada.Unchecked_Conversion, System;
with Interfaces.C.Strings;
with GL.Extended;

package body GL is

  procedure Light
   (light : LightIDEnm;
    pname : LightParameterVEnm;
    params: Light_Float_vector)
  is
    params_copy: aliased Light_Float_vector:= params;
  begin
    Lightfv( light, pname, params_copy(0)'Unchecked_Access);
  end Light;

  procedure Material (face  : FaceEnm;
                      pname : MaterialParameterVEnm;
                      params: Material_Float_vector)
  is
    params_copy: aliased Material_Float_vector:= params;
  begin
    Materialfv(face, pname, params_copy(0)'Unchecked_Access);
  end Material;

  function Cvt is new Ada.Unchecked_Conversion(System.Address,DoublePtr);
  -- This method is functionally identical as GNAT's Unrestricted_Access
  -- but has no type safety (cf GNAT Docs)
  pragma No_Strict_Aliasing(DoublePtr); -- recommended by GNAT 2005

  procedure Vertex (v: Double_vector_3D) is
  begin
    Vertex3dv(Cvt(v(0)'Address));
  end Vertex;

  procedure Normal (v: Double_vector_3D) is
  begin
    Normal3dv(Cvt(v(0)'Address));
  end Normal;

  procedure Translate (v: Double_vector_3D) is
  begin
    Translate(v(0),v(1),v(2));
  end Translate;

  procedure Color(v: RGB_Color) is
  begin
    Color3dv(Cvt(v.red'Address));
  end Color;

  procedure Color(v: RGBA_Color) is
  begin
    Color4dv(Cvt(v.red'Address));
  end Color;

  function GetString (name: StringEnm) return String is
    function Cvt is new Ada.Unchecked_Conversion(ubytePtr,Interfaces.C.Strings.chars_ptr);
    ps: constant Interfaces.C.Strings.chars_ptr:= Cvt(GL.GetString(name));
    use Interfaces.C.Strings;
  begin
    -- OpenGL doc: If an error is generated, glGetString returns 0.
    if ps = Null_Ptr then
      -- We still return a string, but an empty one (this is abnormal)
      return "";
    else
      return Interfaces.C.Strings.Value(ps);
    end if;
  end GetString;

  -----------------------------
  -- Wrappers of GL.Extended --
  -----------------------------

  procedure GenBuffers (n       : in GL.SizeI;
                        buffers : in GL.uintPtr)
                        renames GL.Extended.GenBuffers;

  procedure DeleteBuffers (n       : in GL.SizeI;
                           buffers : in GL.uintPtr)
                           renames GL.Extended.DeleteBuffers;

  procedure BindBuffer (target : in VBO_Target;
                        buffer : in gl.uInt)
                        renames GL.Extended.BindBuffer;

  procedure BufferData (target : in GL.vbo_Target;
                        size   : in GL.SizeIPtr;
                        data   : in GL.Pointer;
                        usage  : in GL.VBO_Usage)
                        renames GL.Extended.BufferData;

  procedure BufferSubData (target : in GL.vbo_Target;
                           offset : in GL.intPtr;
                           size   : in GL.SizeIPtr;
                           data   : in GL.Pointer)
                           renames GL.Extended.BufferSubData;

  function MapBuffer   (target : in GL.vbo_Target;
                        Policy : in GL.Access_Policy) return gl.Pointer
                        renames GL.Extended.MapBuffer;

  function UnmapBuffer (target : in GL.vbo_Target) return GL_Boolean
                        renames GL.Extended.UnmapBuffer;

  procedure GetBufferParameter (target : in GL.vbo_Target;
                                value  : in Buffer_Parameter;
                                data   : in intPointer)
                                renames GL.Extended.GetBufferParameter;

end GL;
