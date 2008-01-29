package GLOBE_3D.Materials is

  -- A few colours:

  Red   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (1.0, 0.0, 0.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Orange   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.992157, 0.513726, 0.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Yellow   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (1.0, 0.964706, 0.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Green   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.0, 1.0, 0.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );


  Indigo   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.0980392, 0.0, 0.458824, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Blue   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.0, 0.0, 1.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Violet   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.635294, 0.0, 1.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  White   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.992157, 0.992157, 0.992157, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Black   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.0, 0.0, 0.0, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Medium_Gray   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.454902, 0.454902, 0.454902, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  Light_Gray   : constant Material_type:= (
    ambient   => (0.0, 0.0, 0.0, 1.0),
    diffuse   => (0.682353, 0.682353, 0.682353, 1.0),
    specular  => (0.0225, 0.0225, 0.0225, 1.0),
    emission  => (0.0, 0.0, 0.0, 1.0),
    shininess => 12.8
  );

  -- A few "material" materials:

  Glass   : constant Material_type:= (
              ambient   => (0.0, 0.0, 0.0, 1.0),
              diffuse   => (0.588235, 0.670588, 0.729412, 1.0),
              specular  => (0.9, 0.9, 0.9, 1.0),
              emission  => (0.0, 0.0, 0.0, 1.0),
              shininess => 96.0
            );

  Brass    : constant Material_type:= (
            ambient =>        (0.329412, 0.223529, 0.027451, 1.0),
            diffuse =>        (0.780392, 0.568627, 0.113725, 1.0),
            specular =>       (0.992157, 0.941176, 0.807843, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      27.8974);
  Bronze    : constant Material_type:= (
            ambient =>        (0.2125, 0.1275, 0.054, 1.0),
            diffuse =>        (0.714, 0.4284, 0.18144, 1.0),
            specular =>       (0.393548, 0.271906, 0.166721, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      25.6);
  Polished_Bronze    : constant Material_type:= (
            ambient =>        (0.25, 0.148, 0.06475, 1.0),
            diffuse =>        (0.4, 0.2368, 0.1036, 1.0),
            specular =>       (0.774597, 0.458561, 0.200621, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      76.8);
  Chrome    : constant Material_type:= (
            ambient =>        (0.25, 0.25, 0.25, 1.0),
            diffuse =>        (0.4, 0.4, 0.4, 1.0),
            specular =>       (0.774597, 0.774597, 0.774597, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      76.8);
  Copper    : constant Material_type:= (
            ambient =>        (0.19125, 0.0735, 0.0225, 1.0),
            diffuse =>        (0.7038, 0.27048, 0.0828, 1.0),
            specular =>       (0.256777, 0.137622, 0.086014, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      12.8);
  Polished_Copper    : constant Material_type:= (
            ambient =>        (0.2295, 0.08825, 0.0275, 1.0),
            diffuse =>        (0.5508, 0.2118, 0.066, 1.0),
            specular =>       (0.580594, 0.223257, 0.0695701, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      51.2);
  Gold    : constant Material_type:= (
            ambient =>        (0.24725, 0.1995, 0.0745, 1.0),
            diffuse =>        (0.75164, 0.60648, 0.22648, 1.0),
            specular =>       (0.628281, 0.555802, 0.366065, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      51.2);
  Polished_Gold    : constant Material_type:= (
            ambient =>        (0.24725, 0.2245, 0.0645, 1.0),
            diffuse =>        (0.34615, 0.3143, 0.0903, 1.0),
            specular =>       (0.797357, 0.723991, 0.208006, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      83.2);
  Pewter    : constant Material_type:= (
            ambient =>        (0.105882, 0.058824, 0.113725, 1.0),
            diffuse =>        (0.427451, 0.470588, 0.541176, 1.0),
            specular =>       (0.333333, 0.333333, 0.521569, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      9.84615);
  Silver    : constant Material_type:= (
            ambient =>        (0.19225, 0.19225, 0.19225, 1.0),
            diffuse =>        (0.50754, 0.50754, 0.50754, 1.0),
            specular =>       (0.508273, 0.508273, 0.508273, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      51.2);
  Polished_Silver    : constant Material_type:= (
            ambient =>        (0.23125, 0.23125, 0.23125, 1.0),
            diffuse =>        (0.2775, 0.2775, 0.2775, 1.0),
            specular =>       (0.773911, 0.773911, 0.773911, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      89.6);
  Emerald    : constant Material_type:= (
            ambient =>        (0.0215, 0.1745, 0.0215, 0.55),
            diffuse =>        (0.07568, 0.61424, 0.07568, 0.55),
            specular =>       (0.633, 0.727811, 0.633, 0.55),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      76.8);
  Jade    : constant Material_type:= (
            ambient =>        (0.135, 0.2225, 0.1575, 0.95),
            diffuse =>        (0.54, 0.89, 0.63, 0.95),
            specular =>       (0.316228, 0.316228, 0.316228, 0.95),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      12.8);
  Obsidian    : constant Material_type:= (
            ambient =>        (0.05375, 0.05, 0.06625, 0.82),
            diffuse =>        (0.18275, 0.17, 0.22525, 0.82),
            specular =>       (0.332741, 0.328634, 0.346435, 0.82),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      38.4);
  Pearl    : constant Material_type:= (
            ambient =>        (0.25, 0.20725, 0.20725, 0.922),
            diffuse =>        (1.0, 0.829, 0.829, 0.922),
            specular =>       (0.296648, 0.296648, 0.296648, 0.922),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      11.264);
  Ruby    : constant Material_type:= (
            ambient =>        (0.1745, 0.01175, 0.01175, 0.55),
            diffuse =>        (0.61424, 0.04136, 0.04136, 0.55),
            specular =>       (0.727811, 0.626959, 0.626959, 0.55),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      76.8);
  Turquoise    : constant Material_type:= (
            ambient =>        (0.1, 0.18725, 0.1745, 0.8),
            diffuse =>        (0.396, 0.74151, 0.69102, 0.8),
            specular =>       (0.297254, 0.30829, 0.306678, 0.8),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      12.8);
  Black_Plastic    : constant Material_type:= (
            ambient =>        (0.0, 0.0, 0.0, 1.0),
            diffuse =>        (0.01, 0.01, 0.01, 1.0),
            specular =>       (0.50, 0.50, 0.50, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      32.0);
  Black_Rubber    : constant Material_type:= (
            ambient =>        (0.02, 0.02, 0.02, 1.0),
            diffuse =>        (0.01, 0.01, 0.01, 1.0),
            specular =>       (0.4, 0.4, 0.4, 1.0),
            emission =>       (0.0,0.0,0.0,0.0),
            shininess =>      10.0);

  VRML_Defaults   : constant Material_type:= (
            ambient =>        (0.2, 0.2, 0.2, 1.0),
            diffuse =>        (0.8, 0.8, 0.8, 1.0),
            specular =>       (0.0, 0.0, 0.0, 1.0),
            emission =>       (0.0, 0.0, 0.0, 1.0),
            shininess =>       25.6);

end GLOBE_3D.Materials;

