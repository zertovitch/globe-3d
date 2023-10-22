--  Change log:

--  RK  18-Oct-2006: removed initialisation by dynamic allocation,
--                   to avoid memory leaks

with GLU;

package body GLOBE_3D.Software_Anti_Aliasing is

  use GL;

  type Jitter_Matrix is
     array (Positive range <>, Positive range <>) of GL.Double;

  type p_Jitter_Matrix is access all Jitter_Matrix;

  --  Matrices for anti-aliasing (choice: matrix J at the end of Jitter) :

  J3 : aliased Jitter_Matrix :=
   (((0.5, 0.5),
     (1.35899e-05, 0.230369),
     (0.000189185, 0.766878)));

  J4 : aliased Jitter_Matrix :=
   ((0.375, 0.23), (0.123, 0.77),
     (0.875, 0.27), (0.627, 0.73));

  J11 : aliased Jitter_Matrix :=
   (((0.5, 0.5), (0.406537, 0.135858),
     (0.860325, 0.968558), (0.680141, 0.232877),
     (0.775694, 0.584871), (0.963354, 0.309056),
     (0.593493, 0.864072), (0.224334, 0.415055),
     (0.0366643, 0.690884), (0.139685, 0.0313988),
     (0.319861, 0.767097)));

  J16 : aliased Jitter_Matrix :=
   (((0.4375, 0.4375), (0.1875, 0.5625),
     (0.9375, 1.1875), (0.4375, -0.0625),
     (0.6875, 0.5625), (0.1875, 0.0625),
     (0.6875, 0.3125), (0.1875, 0.3125),
     (0.4375, 0.1875), (-0.0625, 0.4375),
     (0.6875, 0.8125), (0.4375, 0.6875),
     (0.6875, 0.0625), (0.9375, 0.9375),
     (1.1875, 0.8125), (0.9375, 0.6875)));

  J29 : aliased Jitter_Matrix :=
   (((0.5, 0.5), (0.498126, 0.141363),
     (0.217276, 0.651732), (0.439503, 0.954859),
     (0.734171, 0.836294), (0.912454, 0.79952),
     (0.406153, 0.671156), (0.0163892, 0.631994),
     (0.298064, 0.843476), (0.312025, 0.0990405),
     (0.98135, 0.965697), (0.841999, 0.272378),
     (0.559348, 0.32727), (0.809331, 0.638901),
     (0.632583, 0.994471), (0.00588314, 0.146344),
     (0.713365, 0.437896), (0.185173, 0.246584),
     (0.901735, 0.474544), (0.366423, 0.296698),
     (0.687032, 0.188184), (0.313256, 0.472999),
     (0.543195, 0.800044), (0.629329, 0.631599),
     (0.818263, 0.0439354), (0.163978, 0.00621497),
     (0.109533, 0.812811), (0.131325, 0.471624),
     (0.0196755, 0.331813)));

  J90 : aliased Jitter_Matrix :=
   (((0.5, 0.5), (0.784289, 0.417355),
     (0.608691, 0.678948), (0.546538, 0.976002),
     (0.972245, 0.270498), (0.765121, 0.189392),
     (0.513193, 0.743827), (0.123709, 0.874866),
     (0.991334, 0.745136), (0.56342, 0.0925047),
     (0.662226, 0.143317), (0.444563, 0.928535),
     (0.248017, 0.981655), (0.100115, 0.771923),
     (0.593937, 0.559383), (0.392095, 0.225932),
     (0.428776, 0.812094), (0.510615, 0.633584),
     (0.836431, 0.00343328), (0.494037, 0.391771),
     (0.617448, 0.792324), (0.688599, 0.48914),
     (0.530421, 0.859206), (0.0742278, 0.665344),
     (0.979388, 0.626835), (0.183806, 0.479216),
     (0.151222, 0.0803998), (0.476489, 0.157863),
     (0.792675, 0.653531), (0.0990416, 0.267284),
     (0.776667, 0.303894), (0.312904, 0.296018),
     (0.288777, 0.691008), (0.460097, 0.0436075),
     (0.594323, 0.440751), (0.876296, 0.472043),
     (0.0442623, 0.0693901), (0.355476, 0.00442787),
     (0.391763, 0.361327), (0.406994, 0.696053),
     (0.708393, 0.724992), (0.925807, 0.933103),
     (0.850618, 0.11774), (0.867486, 0.233677),
     (0.208805, 0.285484), (0.572129, 0.211505),
     (0.172931, 0.180455), (0.327574, 0.598031),
     (0.685187, 0.372379), (0.23375, 0.878555),
     (0.960657, 0.409561), (0.371005, 0.113866),
     (0.29471, 0.496941), (0.748611, 0.0735321),
     (0.878643, 0.34504), (0.210987, 0.778228),
     (0.692961, 0.606194), (0.82152, 0.8893),
     (0.0982095, 0.563104), (0.214514, 0.581197),
     (0.734262, 0.956545), (0.881377, 0.583548),
     (0.0560485, 0.174277), (0.0729515, 0.458003),
     (0.719604, 0.840564), (0.325388, 0.7883),
     (0.26136, 0.0848927), (0.393754, 0.467505),
     (0.425361, 0.577672), (0.648594, 0.0248658),
     (0.983843, 0.521048), (0.272936, 0.395127),
     (0.177695, 0.675733), (0.89175, 0.700901),
     (0.632301, 0.908259), (0.782859, 0.53611),
     (0.0141421, 0.855548), (0.0437116, 0.351866),
     (0.939604, 0.0450863), (0.0320883, 0.962943),
     (0.341155, 0.895317), (0.952087, 0.158387),
     (0.908415, 0.820054), (0.481435, 0.281195),
     (0.675525, 0.25699), (0.585273, 0.324454),
     (0.156488, 0.376783), (0.140434, 0.977416),
     (0.808155, 0.77305), (0.282973, 0.188937)));

  J : p_Jitter_Matrix;

  function Anti_Alias_Phases return Positive is
  begin
    if J = null then
      return 1;
    else
      return J'Length (1) + 2;
    end if;
  end Anti_Alias_Phases;

  procedure Display_with_Anti_Aliasing (phase : Positive) is

    procedure Jitter is
      Dxy : array (J'Range (2)) of GL.Double;
      weight : constant GL.Float := 1.0 / GL.Float (J'Length (1));

      procedure LoadDxDy (jt : Positive) is
        view : aliased GLU.viewPortRec;
        inv : array (1 .. 2) of GL.Double;
      begin
        --  GLU.Get( VIEWPORT, view'unrestricted_access );
        GLU.Get (view);
        inv (1) := 10.0 / GL.Double (view.Width);
        inv (2) := 10.0 / GL.Double (view.Height);
        for d in Dxy'Range loop
          Dxy (d) := (J (jt, d) - 0.25) * inv (d);
        end loop;
      end LoadDxDy;

    begin
      if phase = 1 then
          Clear (COLOR_BUFFER_BIT or ACCUM_BUFFER_BIT);
      elsif phase in 2 .. Anti_Alias_Phases - 1 then
          Clear (COLOR_BUFFER_BIT);
          LoadDxDy (phase - 1);
          MatrixMode (MODELVIEW);
          PushMatrix;
          Translate (Dxy (1), Dxy (2), 0.0);
          Display;
          MatrixMode (MODELVIEW);
          PopMatrix;
          Accum (ACCUM, weight);
      elsif phase = Anti_Alias_Phases then
          Accum (GL_RETURN, 1.0);
          --  ^ Transfers accumulation buffer values to the color buffer or
          --    buffers currently selected for writing.
          Flush;
      else
          raise Constraint_Error;
      end if;
    end Jitter;
  begin
    if J = null then
      Clear (COLOR_BUFFER_BIT);
      Display;
      Flush;
    else
      Jitter;
    end if;
  end Display_with_Anti_Aliasing;

  procedure Set_Quality (q : Quality) is
  begin
    case q is
      when Q1  => J := null;
      when Q3  => J := J3'Access;
      when Q4  => J := J4'Access;
      when Q11 => J := J11'Access;
      when Q16 => J := J16'Access;
      when Q29 => J := J29'Access;
      when Q90 => J := J90'Access;
    end case;
  end Set_Quality;

begin
  Set_Quality (Q3);
end GLOBE_3D.Software_Anti_Aliasing;
