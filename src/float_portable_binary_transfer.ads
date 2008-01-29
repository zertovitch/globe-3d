------------------------------------------------------------------------------
--  File:            Float_portable_binary_transfer.ads
--  Description:     Split & merge floating-point numbers into integers to
--                   facilitate a portable transfer, including Input-Output
--  Date / Version:  2-Sep-2006; 28-Aug-2006; 27-Aug-2006
--  Author:          G. de Montmollin - public domain code
------------------------------------------------------------------------------
generic
  type Num is digits <>;
  type Mantissa_type is range <>;
  type Exponent_type is range <>;
  -- If one is not sure that 'Machine_Mantissa and 'Machine_Radis
  -- will be the same on the machine performing Split than on the
  -- machine performing Merge, it is better to impose the mantissa scaling
  imposed_mantissa : Boolean; -- choice
  imposed_scaling  : Num;     -- factor from fraction to mantissa value

package Float_portable_binary_transfer is

  procedure Split(f: in Num; m: out Mantissa_type; e: out Exponent_type);

  procedure Merge(m: in Mantissa_type; e: in Exponent_type; f: out Num);

  -- Split / Merge in two parts (e.g. for transporting a Long_Float
  -- when the compiler has only up to 32-bit integers. For splitting,
  -- the scaling factor is applied once for m1, then a second time for m2.

  procedure Split(f: in Num; m1,m2: out Mantissa_type; e: out Exponent_type);

  procedure Merge(m1,m2: in Mantissa_type; e: in Exponent_type; f: out Num);


end Float_portable_binary_transfer;
