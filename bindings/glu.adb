with Ada.Unchecked_Conversion, System;

package body GLU is

  type loc_DoublePtr is new GL.doublePtr;

  pragma No_Strict_Aliasing(doubleMatrixPtr);
  pragma No_Strict_Aliasing(viewPortRecPtr);
  pragma No_Strict_Aliasing(loc_DoublePtr);
  -- recommended by GNAT 2005

  procedure Get (pname : GL.ParameterNameEnm;
                 params: out doubleMatrix) is
    function Cvt is new Ada.Unchecked_Conversion(System.Address,doubleMatrixPtr);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
  begin
    Get(pname, Cvt(params(0,0)'Address));
  end Get;

  procedure Get (params: out viewPortRec) is
    function Cvt is new Ada.Unchecked_Conversion(System.Address,viewPortRecPtr);
  begin
    Get(GL.VIEWPORT, Cvt(params.X'Address));
  end Get;

  procedure Project (objx       : GL.Double;
                     objy       : GL.Double;
                     objz       : GL.Double;
                     modelMatrix: doubleMatrix;
                     projMatrix : doubleMatrix;
                     viewport   : viewPortRec;
                     winx       : out GL.Double;
                     winy       : out GL.Double;
                     winz       : out GL.Double;
                     result     : out Boolean )
  is
    function CvV is new Ada.Unchecked_Conversion(System.Address,viewPortRecPtr);
    function CvM is new Ada.Unchecked_Conversion(System.Address,doubleMatrixPtr);
    function Cvt is new Ada.Unchecked_Conversion(System.Address,loc_DoublePtr);
    wx,wy,wz: GL.Double;
    use GL;
  begin
    -- Call the same function with C style
    result:= Project(
      objx,objy,objz,
      CvM(modelMatrix'Address),
      CvM(projMatrix'Address),
      CvV(viewport'Address),
      GL.doublePtr(Cvt(wx'Address)),
      GL.doublePtr(Cvt(wy'Address)),
      GL.doublePtr(Cvt(wz'Address))
    )
    =
      GL.GL_Boolean'Pos(GL.GL_TRUE);
    winx:= wx;
    winy:= wy;
    winz:= wz;
  end Project;

end GLU;
