package body Float_portable_binary_transfer is

  inverse_scaling: constant Num:= 1.0 / imposed_scaling;

  -- We rely on Ada's attributes of floating-point types, RM: A.5.3

  procedure Split (f: in Num; m: out Mantissa_type; e: out Exponent_type) is
  begin
    if imposed_mantissa then
      m:= Mantissa_type(imposed_scaling * Num'Fraction(f));
    else
      m:= Mantissa_type(Num'Scaling(Num'Fraction(f),Num'Machine_Mantissa));
    end if;
    e:= Num'Exponent(f);
  end Split;

  procedure Merge (m: in Mantissa_type; e: in Exponent_type; f: out Num) is
    fraction: Num;
  begin
    if imposed_mantissa then
      fraction:= Num(m) * inverse_scaling;
    else
      fraction:= Num'Scaling(Num(m),-Num'Machine_Mantissa);
    end if;
    -- We compose a float with the fraction and the exponent
    f:= Num'Compose(fraction, e);
  end Merge;

  -- Split / Merge in two parts --

  machine_half_mantissa: constant Integer:= 1+Num'Machine_Mantissa/2;

  procedure Split(f: in Num; m1,m2: out Mantissa_type; e: out Exponent_type) is
    f1,f2: Num;
  begin
    if imposed_mantissa then
      f1:= imposed_scaling * Num'Fraction(f);
    else
      f1:= Num'Scaling(Num'Fraction(f),machine_half_mantissa);
    end if;
    m1:= Mantissa_type(f1);
    f1:= f1 - Num(m1);
    if imposed_mantissa then
      f2:= imposed_scaling * f1;
    else
      f2:= Num'Scaling(f1,machine_half_mantissa);
    end if;
    m2:= Mantissa_type(f2);
    e:= Num'Exponent(f);
  end Split;

  procedure Merge(m1,m2: in Mantissa_type; e: in Exponent_type; f: out Num) is
    fraction: Num;
  begin
    if imposed_mantissa then
      fraction:= (Num(m1)+Num(m2) * inverse_scaling) * inverse_scaling;
    else
      fraction:= Num'Scaling(Num(m1)+Num'Scaling(Num(m2),-machine_half_mantissa),-machine_half_mantissa);
    end if;
    -- We compose a float with the fraction and the exponent
    f:= Num'Compose(fraction, e);
  end Merge;

end Float_portable_binary_transfer;

