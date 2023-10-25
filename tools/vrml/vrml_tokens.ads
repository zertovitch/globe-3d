package Vrml_Tokens is



  type const_type is (
    intval, 
    floatval, 
    doubleval, 
    stringval, 
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Float;
  end record;


    YYLVal, YYVal : YYSType;

    type Token is
        (End_Of_Input, Error, Number, Float_T,
         Vrmlstring, Vrmlword, Asciitext,
         Cone, Coordinate3, Cube,
         Cylinder, Directionallight, Fontstyle,
         Group, Indexedfaceset, Indexedlineset,
         Info, Lod, Material,
         Materialbinding, Levelofdetail, Matrixtransform,
         Normal, Normalbinding, Orthographiccamera,
         Perspectivecamera, Pointlight, Pointset,
         Rotation, Scale, Separator,
         Shapehints_T, Sphere, Spotlight,
         Switch, Texture2, Texture2transform,
         Texturecoordinate2, Transform, Transformseparator,
         Translation, Wwwanchor, Wwwinline,
         Parts, Bottomradius, Height,
         Point, Width, Depth,
         String_T, Spacing, Justification,
         Radius, On, Intensity,
         Color, Direction, Size,
         Family, Style, Coordindex,
         Materialindex, Normalindex, Texturecoordindex,
         Range_T, Screenarea, Translation_F,
         Center, Ambientcolor, Diffusecolor,
         Specularcolor, Emissivecolor, Shininess,
         Transparency, Value, Matrix,
         Vector, Position, Orientation,
         Focaldistance, Heightangle, Location,
         Startindex, Numpoints, Rotation_F,
         Scalefactor, Renderculling, Vertexordering,
         Shapetype, Facetype, Creaseangle,
         Dropoffrate, Cutoffangle, Whichchild,
         Filename, Image, Wraps,
         Wrapt, Scaleorientation, Name,
         Description, Map, Bboxsize,
         Bboxcenter, Def, Use_T,
         Fields, Unknownnode, '{',
         '}', '|', '(',
         ')', ',', '[',
         ']' );

    Syntax_Error : exception;

end Vrml_Tokens;
