with GL;
with GLOBE_3D.Math;
with Ada.Numerics;                      use Ada.Numerics;

package body Planet is

  procedure Create(
    object  : in out GLOBE_3D.p_Object_3D;
    scale   :        GLOBE_3D.Real;
    centre  :        GLOBE_3D.Point_3D;
    mercator:        GLOBE_3D.Image_ID;
    parts   :        Positive:= 30
  )
  is
    use GLOBE_3D, GL, GLOBE_3D.REF, GLOBE_3D.Math;
    n: constant Positive:= parts;
    step_i     : constant Real:= 1.0 / Real(n);
    step_j     : constant Real:= 1.0 / Real(n-1);
    step_psi   : constant Real:= Pi * step_j;
    step_theta : constant Real:= 2.0 * Pi * step_i;
    psi, theta, sin_psi: Real;
    k: Positive;
    face_0 : Face_type; -- takes defaults values
    procedure Map_texture(i,j: Natural) is
      ri,rj, ri1,rj1: Real;
    begin
      ri := Real(i);
      ri1:= Real(i+1);
      rj := Real(j);
      rj1:= Real(j+1);
      face_0.texture_edge_map:=
        (
          ( ri *step_i, rj *step_j ),
          ( ri1*step_i, rj *step_j ),
          ( ri1*step_i, rj1*step_j ),
          ( ri *step_i, rj1*step_j )
        );
    end Map_texture;
  begin
    object:= new Object_3D( Max_points=> n*n, Max_faces=> (n-1)*n );
    object.centre:= centre;
    -- Set points:
    k:= 1;
    for j in 0..n-1 loop
      for i in 0..n-1 loop
        theta := Real(i) * step_theta;
        psi   := Real(j) * step_psi;
        sin_psi:= Sin(psi);
        object.point(k):= scale * (sin_psi * Cos(theta), sin_psi * Sin(theta), -Cos(psi));
        -- phi [from North pole] = pi - psi; sin phi = sin psi; cos phi = -cos psi
        k:= k + 1;
      end loop;
    end loop;

    -- Set faces:
    face_0.whole_texture:= False; -- indeed all faces share the same texture
    face_0.skin:= texture_only;
    face_0.texture:= mercator;
    k:= 1;
    for j in 0..n-2 loop
      for i in 0..n-2 loop
        if j=0 then
          face_0.P:= (i+1, 0, i+n+2, i+n+1);
        elsif j=n-2 then
          face_0.P:= (n*j+i+1, n*j+i+2, n*j+i+n+2, 0);
        else
          face_0.P:= (n*j+i+1, n*j+i+2, n*j+i+n+2, n*j+i+n+1);
        end if;
        Map_texture(i,j);
        object.face(k):= face_0;
        k:= k + 1;
      end loop;
      if j=0 then
        face_0.P:= (n, 0, n*j+n+1, n*j+n+n);
      elsif j=n-2 then
        face_0.P:= (n*j+n, n*j+1, n*j+n+1, 0);
      else
        face_0.P:= (n*j+n, n*j+1, n*j+n+1, n*j+n+n);
      end if;
      Map_texture(n-1,j);
      object.face(k):= face_0;
      k:= k + 1;
    end loop;

  end Create;

end Planet;
