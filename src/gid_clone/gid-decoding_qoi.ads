--  QOI - The "Quite OK Image" format for fast, lossless image compression
--
--  https://github.com/phoboslab/qoi
--
--  Dominic Szablewski - https://phoboslab.org

private package GID.Decoding_QOI is

  --------------------
  -- Image decoding --
  --------------------

  generic
    type Primary_Color_Range is mod <>;
    with procedure Set_X_Y (x, y : Natural);
    with procedure Put_Pixel
      (red, green, blue : Primary_Color_Range;
       alpha            : Primary_Color_Range);
    with procedure Feedback (percents : Natural);
  --
  procedure Load (image : in out Image_Descriptor);

end GID.Decoding_QOI;
