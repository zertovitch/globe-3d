with Ada.Unchecked_Conversion, System;

package body GLU is

  type loc_DoublePtr is new GL.DoublePtr;

  pragma No_Strict_Aliasing(DoubleMatrixPtr);
  pragma No_Strict_Aliasing(ViewPortRecPtr);
  pragma No_Strict_Aliasing(loc_DoublePtr);
  -- recommended by GNAT 2005

  procedure Get (pname : GL.ParameterNameEnm;
                 params: out doubleMatrix) is
    function Cvt is new Ada.Unchecked_Conversion(System.Address,DoubleMatrixPtr);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
  begin
    Get(pname, Cvt(params(0,0)'Address));
  end Get;

  procedure Get (params: out viewPortRec) is
    function Cvt is new Ada.Unchecked_Conversion(System.Address,viewPortRecPtr);
  begin
    Get(GL.VIEWPORT, Cvt(params.x'Address));
  end Get;


  procedure Project (objx       : GL.double;
                     objy       : GL.double;
                     objz       : GL.double;
                     modelMatrix: doubleMatrix;
                     projMatrix : doubleMatrix;
                     viewport   : viewPortRec;
                     winx       : out GL.double;
                     winy       : out GL.double;
                     winz       : out GL.double;
                     result     : out Boolean )
  is
    function CvV is new Ada.Unchecked_Conversion(System.Address,ViewPortRecPtr);
    function CvM is new Ada.Unchecked_Conversion(System.Address,DoubleMatrixPtr);
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
      GL.DoublePtr(Cvt(wx'Address)),
      GL.DoublePtr(Cvt(wy'Address)),
      GL.DoublePtr(Cvt(wz'Address))
    )
    =
      GL.GL_Boolean'Pos(GL.GL_TRUE);
    winx:= wx;
    winy:= wy;
    winz:= wz;
  end Project;

end GLU;

