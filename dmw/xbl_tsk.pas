unit xbl_tsk; interface

function Geoid_Dist(b1,l1, b2,l2: double; var fi: double): double; stdcall;

function Geoid_Forw(b1,l1, r,fi: double; out b2,l2: double): double; stdcall;

implementation

uses
  Math,xbl;

const
  eps = 0.000000000048;

function Dist_small(f1,l1, f2,l2: Double; out a12,a21: Double): Double;
var
  dl,df,fm,c,e12,cf,et,vv,n,m,q,p,da,am,r,wc,ws: double;
  orr,ozz: Double;
begin
  dl:=l2-l1;

  if abs(abs(dl)-pi)<=eps then begin
    df:=pi*abs(f1)/f1-f1-f2; fm:=f1+df/2;
  end
  else begin
    df:=f2-f1; fm:=(f1+f2)/2;
  end;

  c:=c_.A/(1-c_.F);
  e12:=c_.Es/(1-c_.Es);

  cf:=cos(fm);
  et:=e12*cf*cf; vv:=1+et;
  n:=c/Sqrt(vv); m:=n/vv;

  if abs(Sin(dl)) <= eps then begin
    ws:=0; wc:=0;
  end
  else begin
    ws:=dl*sin(fm); wc:=dl*cf;
  end;

  r:=Sqrt(m*n);
  q:=df*m*(1-(e12-2*et)*df*df/8-vv*wc*wc/12-ws*ws/8);
  p:=wc*n*(1+(1-9*e12+8*et)*df*df/24-ws*ws/24);
  da:=ws*(1+(3+2*et)*df*df/24+vv*wc*wc/24);

  Result:=Sqrt(p*p+q*q);
  orr:=r; ozz:=Result/r;

  if abs(Sin(dl)) <= eps then begin

    if abs(dl) > eps then begin
      a12:=pi*(1-abs(f1+f2)/(f1+f2))/2;
      a21:=a12;
    end
    else begin
      a12:=pi*(1-abs(f2-f1)/(f2-f1))/2;
      a21:=a12-pi;
    end;
    
  end
  else begin
    if abs(q) <= eps then
      am:=pi/2*abs(p)/p
    else begin
      am:=Arctan(p/q);
      if q < 0 then am:=am+pi;
    end;

    a12:=am-da/2;
    a21:=am+da/2-pi;
  end;

  if abs(a12-pi*2)<=eps then a12:=0;
  if abs(a21-pi*2)<=eps then a21:=0;
  if a12 < 0 then a12:=a12+pi*2;
  if a21 < 0 then a21:=a21+pi*2;
end;

function Sphere(f1,l1, f2,l2: Double; out a12,a21: Double): Double;
var
  sf1,sf2,cf1,cf2,sdl,cdl, x,y,z: Extended;
  cz,sz, ca,sa: Extended;
begin
  Result:=0; a12:=0; a21:=0;

  SinCos(f1, sf1,cf1);
  SinCos(f2, sf2,cf2);

  SinCos(l2-l1, sdl,cdl);
  cz:=sf1*sf2 + cf1*cf2*cdl;

  if abs(cz) <= eps then begin
    z:=pi/2; sz:=1;
  end
  else begin
    sz:=sqrt(1-cz*cz); z:=arctan(sz/cz);
    if z < 0 then z:=z+pi
  end;

  if z*c_.A < 60000 then
    Result:=Dist_small(f1,l1, f2,l2, a12,a21)
  else begin
    Result:=z*c_.A;

    x:=sf2-sf1*cz; y:=cf1*sz;
    if abs(y) > eps then begin
      ca:=x/y;
      if abs(ca) > 1 then begin
        if ca < 0 then a12:=pi else a12:=0;
      end
      else begin
        sa:=cf2*sdl/cz;
        a12:=arctan(sa/ca);
        if ca < 0 then a12:=a12*pi;
      end;
    end
    else a12:=pi/2*x/abs(x);

    x:=+sf1-sf2*cz; y:=cf2*sz;
    if abs(y) > eps then begin
      ca:=x/y;
      if abs(ca) > 1 then begin
        if ca < 0 then a21:=pi else a21:=0;
      end
      else begin
        sa:=cf1*sdl/cz;
        a21:=arctan(sa/ca);
        if ca < 0 then a21:=a21+pi;
      end;
    end
    else a21:=pi/2*abs(x)/x;

  end;
end;

function AZM(suo1,suo2,cuo1,sao,dl,z: double): double;
var
  ca,sa,ao,x: double;
begin
  Result:=0;

  if abs(sao) <= eps then begin
    if abs(dl) > eps then begin
      if (abs(suo1) <= eps) and (abs(suo2) <= eps) then
        ao:=0
      else
      if abs(suo1+suo2) <= eps then
        ao:=pi*(1-abs(suo2)/suo2)/2
      else
        ao:=pi*(1-abs(suo1+suo2)/(suo1+suo2))/2;
    end
    else
      ao:=pi*(1-abs(suo2-suo1)/(suo2-suo1))/2;
  end
  else begin
    if abs(abs(sao)-cuo1)<=eps then
      ao:=pi/2*abs(sao)/sao
    else begin
      sa:=sao/cuo1;
      ao:=arctan(sa/sqrt(1-sa*sa));
      x:=suo2-suo1*cos(z);
      if abs(x) > eps then begin
        ca:=x/cuo1/sin(z);
        if ca < 0 then ao:=pi*abs(ao)/ao-ao;
      end;
    end;

    if ao < 0 then ao:=ao+pi*2;
  end;

  Result:=ao;
end;

function Dist_large(f1,l1, f2,l2: Double; out a12,a21: Double): Double;
var
  e2,e12,k,adl, a0,a1,a2,a3,a4, b1,b2,b3, c,c0: Extended;
  sf1,cf1, sf2,cf2, cz,sz,z, w,w1,w2,wl, fa,fa1: Extended;
  su1,su2,cu1,cu2, sa0,ca0, dlt,dlt1, x,y: Extended;
  alfa1,beta1, ax,bx,cx: Extended; dl: Double;
  icz,i: Integer;
begin
  e2:=c_.Es; k:=e2/2;
  e12:=c_.Es/(1-c_.Es);

  adl:=Pi/c_.Alfa;
  a1:=k*(1+k*(1+k*(1+5*k/4))/2);
  a2:=-k*k*(1+2*k*(1+15*k/8))/4;
  a3:=3*k*k*k*(1+15*k/4)/16;
  a4:=-25*k*k*k*k/128;
  b1:=k*k*(1+2*k*(1+15*k/8))/8;
  b2:=-k*k*k*(1+15*k/4)/8;
  b3:=75*k*k*k*k/512;

  SinCos(f1, sf1,cf1);
  SinCos(f2, sf2,cf2);

  dl:=l2-l1;
  cz:=sf1*sf2 + cf1*cf2*cos(dl);

  if cz > Cos(70000/c_.A) then
    Result:=Dist_small(f1,l1, f2,l2, a12,a21)
  else begin
    w1:=sqrt(1-e2*sf1*sf1); su1:=sf1*sqrt(1-e2)/w1;
    w2:=sqrt(1-e2*sf2*sf2); su2:=sf2*sqrt(1-e2)/w2;
    cu1:=cf1/w1; cu2:=cf2/w2;

    wl:=dl; dlt:=0; dlt1:=1; icz:=0; i:=0; 

    if Abs(Pi-Abs(dl)) <= adl then begin
      c:=1-abs(dl)/pi; w:=pi;
      x:=c*c_.Alfa; a0:=Arctan(x/sqrt(1-x*x))
    end;

    while Abs(dlt-dlt1) > eps do begin
      icz:=icz+1; if icz > 50 then Break;

      dlt1:=dlt;
      cz:=su1*su2+cu1*cu2*cos(wl);
      sz:=sqrt(1-cz*cz);

      if Abs(cz) <= eps then begin
        z:=pi/2; sz:=1;
      end
      else begin
        z:=arctan(sz/cz);
        if cz < 0 then z:=z+pi;
      end;

      if Abs(Pi-Abs(dl)) <= adl then begin

        if i = 0 then begin
          while Abs(a0-w) > eps do begin
            i:=i+1; if i > 50 then Break;

            SinCos(a0,sa0,ca0);
            w:=a0; c0:=1-sa0*sa0;

            x:=a1+c0*(a2+c0*(a3+a4*c0));
            y:=a2+c0*(2*a3+3*a4*c0);
            fa:=x*sa0-c;
            fa1:=(x-2*y*sa0*sa0)*ca0;
            a0:=a0-fa/fa1;
          end;

          a0:=a0*abs(dl)/dl;
          sa0:=sa0*abs(dl)/dl;
        end
      end
      else begin
        sa0:=cu1*cu2*sin(wl)/sin(z);
        c0:=1-sa0*sa0;
      end;

      x:=2*su1*su2-c0*cz;
      alfa1:=a1+c0*(a2+c0*(a3+c0*a4));
      beta1:=2*(b1+c0*(b2+c0*b3));
      dlt:=(alfa1*z-beta1*x*sz)*sa0;
      wl:=dl+dlt;
    end;

    k:=e12*c0/4;
    ax:=c_.B*(1+k*(1-k*(3-5*k)/4));
    bx:=c_.B*e12*(1-k*(1-15*k/8))/4;
    cx:=c_.B*e12*e12*(1-3*k)/64;
    y:=(c0*c0-2*x*x)*cz;

    Result:=ax*z+(bx*x+cx*y)*sz;

    a12:=azm(su1,su2,cu1,sa0,dl,z);
    a21:=azm(su2,su1,cu2,-sa0,dl,z);
  end
end;

function Geoid_Dist(b1,l1, b2,l2: double; var fi: double): double;
var
  dl, a12,a21: Double;
begin
  Result:=0; a12:=0; a21:=0;

  dl:=l2-l1; if Abs(dl) > Pi then
  dl:=dl-pi*2*abs(dl)/dl;

  if (Abs(b1-b2) > eps)
  or (Abs(dl) > eps) then

  if c_.Alfa > eps then
    Result:=Dist_large(b1,l1, b2,l2, a12,a21)
  else
    Result:=Sphere(b1,l1, b2,l2, a12,a21);

  if abs(a12) <= eps then a12:=0;
  if abs(a21) <= eps then a21:=0;
  if a12 < 0 then a12:=a12+pi*2;
  if a21 < 0 then a21:=a21+pi*2;

  fi:=a12
end;

function Old_Dist(b1,l1, b2,l2: double; var fi: double): double; stdcall;
var
  w1,w2,su1,cu1,su2,alfa,sinsg: Extended;
  cu2,c1,d1,c2,b,d2,beta,cossg: Extended;
  lam,sg,l,p,q,sina0,x_cosa0: Extended;
  x,y,del,dl,a,e2: Extended;
begin
  e2:=c_.Es;

  if Abs(b2-b1) < 0.00001 then
  if Abs(l2-l1) < 0.00001 then
  begin fi:=0; Result:=0; Exit end;

  b1:=b1 + 1.0E-20; b2:=b2 + 1.0E-20;
  l1:=l1 + 1.0E-20; l2:=l2 + 1.0E-20;

  if l1 > pi2 then l1:=(l1/pi2 -  Trunc(l1 /pi2))*pi2;
  if b1 > pi2 then b1:=(b1/pi2 -  Trunc(b1 /pi2))*pi2;
  if l2 > pi2 then l2:=(l2/pi2 -  Trunc(l2 /pi2))*pi2;
  if b2 > pi2 then b2:=(b2/pi2 -  Trunc(b2 /pi2))*pi2;

  w1:=Sqrt(1.0 - e2*sin(b1)*sin(b1)) + 1.0E-10;
  w2:=Sqrt(1.0 - e2*sin(b2)*sin(b2)) + 1.0E-10;

  su1:=sin(b1) * Sqrt(1.0 - e2) / w1;
  su2:=sin(b2) * Sqrt(1.0 - e2) / w2;
  cu1:=cos(b1) / w1  + 1.0E-10      ;
  cu2:=cos(b2) / w2  + 1.0E-10      ;

  l := l2-l1  ;
  c1:= su1*su2;
  c2:= cu1*cu2;
  d1:= cu1*su2;
  d2:= su1*cu2;

  del:=0;

  repeat
    dl  := del;
    lam := l + dl ;
    p   := cu2* sin (lam);
    q   := d1 - d2*cos(lam) + 1.0E-10 ;
    fi  := arctan(p/q);

    if p >= 0.0 then if q >= 0.0 then fi :=      abs(fi)
                                 else fi := pi - abs(fi)
                else if q >= 0.0 then fi := pi2 - abs(fi)
                                 else fi := pi  + abs(fi);

    sinsg:=p*Sin(fi) + q*Cos(fi);
    cossg:=c1 + c2* Cos(lam);

    sg:=Abs(ArcTan(sinsg/cossg));
    if cossg < 0.0 then sg:=Pi - sg;

    sina0  :=cu1 * sin(fi);
    x_cosa0:=1 - sina0*sina0;

    x   := 2*c1 - cos(sg)*x_cosa0;
    alfa:= 3.352329869E-3 - (2.8189E-6 - 7.0E-9 * x_cosa0) * x_cosa0;
    beta:= 2.8189E-6 - 9.4E-9 * x_cosa0;
    del := (alfa* sg - beta* x* sin(sg)) * sina0;

  UNTIL Abs(del - dl) <= 1.0E-10;

  y:=(Sqr(x_cosa0) - 2*x*x) * cos(sg);

  a:=6356863.020 + (10708.949 - 13.474  * x_cosa0) * x_cosa0;
  b:=10708.938 - 17.956 * x_cosa0;

  Result:=a* sg + (b* x + 4.487* y) * sinsg
end;

function Geoid_Forw(b1,l1, r,fi: double; out b2,l2: double): double; stdcall;

procedure small_forw(f1,lo1,ao12,s: double; out f2,lo2,ao21: double);
var
  c,e12,sc: Extended;
  df0,dl0,da0,fm,am,cf,et: Extended;
  vv,kf,kl,ka,df,dl,da: Extended;
begin
  f2:=f1; lo2:=lo1; ao21:=ao12;

  c:=c_.A/(1-c_.F); sc:=s/c;
  e12:=c_.Es/(1-c_.Es);

  df:=sc*Cos(ao12);
  fm:=f1+df/2; if Abs(fm) > Pi_2 then
  fm:=Pi*fm/Abs(fm)-fm;

  dl:=0; da:=0;

  if Abs(Sin(ao12)) > Eps then begin
    dl:=sc*sin(ao12)/cos(fm);
    da:=dl*sin(FM);
  end;

  df0:=0; dl0:=0; da0:=0; am:=ao12+da/2;

  while (Abs(df-df0) > eps) or
        (Abs(dl-dl0) > eps) or
        (Abs(da-da0) > eps) do begin

    df0:=df; dl0:=dl; da0:=da ;
    cf:=cos(fm); et:=e12*cf*cf; vv:=1+et;
    kf:=sc*vv*sqrt(vv)*cos(am);
    kl:= sc*sqrt(vv)*sin(am)/cf;
    ka:=kl*sin(fm);
    df:=kf*(1+(e12-2*et)*kf*kf/8+vv*kl*kl/12+(1-2*et)*ka*ka/24);

    if Abs(sin(ao12)) > Eps then begin
      dl:=kl*(1+sqr(ka)/24-(1-9*e12+8*et)*sqr(kf)/24);
      da:=ka*(1+(2+9*e12-6*et)*sqr(kf)/24+vv*sqr(kl)/12
               -(1+2*et)*sqr(ka)/24);
    end;

    fm:=f1+df/2; if abs(fm) > Pi_2 then
    fm:=pi*fm/abs(fm)-fm;

    am:=ao12+da/2;
  end;

  if am < 0 then am:=am+pi_2;

  f2:=f1+df;
  if Abs(f2) > pi_2 then
  if Abs(sin(ao12)) <= eps then begin
    f2:=pi*(f2)/abs(f2)-f2; dl:=pi;
  end;

  lo2:=lo1+dl;
  if Abs(sin(dl)) <= eps then

    if abs(dl) > eps then
      ao21:=pi*(1-(f1+f2)/abs(f1+f2))/2
    else
      ao21:=pi*(1-(f1-f2)/abs(f1-f2))/2

  else
    ao21:=am+da/2-pi;

  if ao21 < 0 then ao21:=ao21+pi_2;
  if abs(pi_2-abs(ao21)) <= eps then ao21:=0;
end;

procedure large_forw(f1,lo1,ao12,s: double; out f2,lo2,ao21: double);
var
  e2,e12, k,a1,a2,a3,a4,z0,z,sz,cz,dlt,p,q: Extended;
  b1,b2,b3,sf1,w1,su1,cu1,su2,cu2,sa0,sin2z,cos2z,ca: Extended;
  ctz,c0,ax,bx,cx,alfa1,beta1,sin2z1,cos2z1,w,x,dl: Extended;
begin
  f2:=f1; lo2:=lo1; ao21:=ao12;

  e2:=c_.Es; k:=e2/2;
  e12:=c_.Es/(1-c_.Es);

  a1:=k*(1+k/2*(1+k*(1+5*k/4)));
  a2:=-sqr(k)*(1+k*2*(1+15*k/8))/4;
  a3:=3*sqr(k)*k*(1+15*k/4)/16;
  a4:=-25*sqr(k)*k/128;
  b1:=sqr(k)*(1+k*2*(1+15*k/8))/8;
  b2:=-sqr(k)*k*(1+15*k/4)/8;
  b3:=75*sqr(k)*sqr(k)/512;
  sf1:=sin(f1);
  w1:=sqrt(1-e2*sf1*sf1);

  su1:=sf1*sqrt(1-e2)/w1;
  cu1:=cos(f1)/w1;
  sa0:=cu1*sin(ao12);

  if abs(f1) <= eps then begin
    sin2z:=0; cos2z:=0
  end
  else begin
    ctz:=cu1*cos(ao12)/su1; w:=sqr(ctz);
    sin2z:=2*ctz/(w+1); cos2z:=(w-1)/(w+1);
  end;

  c0:=1-sqr(sa0); k:=e12/4*c0;
  ax:=c_.B*(1+k*(1-k/4*(3-5*k)));
  bx:=c_.B*k*(1-k*(1-15*k/8))/2;
  cx:=c_.B*k*k*(1-3*k)/8;

  alfa1:=a1+c0*(a2+c0*(a3+c0*a4));
  beta1:=c0*(b1+c0*(b2+c0*b3));

  z0:=(s-(bx+cx*cos2z)*sin2z)/ax;
  x:=2*z0;

  if abs(x) > pi*2 then x:=x-pi*2*x/abs(x);
  sin2z1:=sin2z*cos(x)+cos2z*sin(x);
  cos2z1:=cos2z*cos(x)-sin2z*sin(x);

  z:=z0+(bx+5*cx*cos2z1)*sin2z1/ax;
  SinCos(z, sz,cz);

  su2:=su1*cz+cu1*sz*cos(ao12);
  if abs(abs(su2)-1) <= eps then begin
    f2:=pi/2*su2/abs(su2); cu2:=0
  end
  else begin
    cu2:=sqrt(1-su2*su2);
    f2:=arctan(su2/cu2/sqrt(1-e2))
  end;

  dlt:=(alfa1*z+beta1*(sin2z1-sin2z))*sa0;
  p:=sin(ao12)*sz; q:=cu1*cz-su1*sz*cos(ao12);

  if abs(q) <= eps then
    w:=pi/2*p/abs(p)
  else begin
    w:=arctan(p/q);
    if q < 0 then w:=w+pi
  end;

  dl:=w-dlt; lo2:=lo1+dl;
  if abs(lo2) > pi then
  lo2:=lo2-pi*2*lo2/abs(lo2);

  if abs(sa0) <= eps then begin
    if abs(dl) > eps then ao21:=pi*(1-(f1+f2)/abs(f1+f2))/2
                     else ao21:=pi*(1-(f1-f2)/abs(f1-f2))/2
  end
  else begin
    p:=sa0; q:=cu1*cz*cos(ao12)-su1*sz;

    if abs(q)<=eps then
      ao21:=-pi/2*p/abs(p)
    else begin
      ao21:=arctan(p/q);
      x:=su1-su2*cz;

      if abs(x) > eps then begin
        ca:=x/cu2/sz;
        if ca < 0 then ao21:=ao21+pi
      end;
    end;

    if ao21 < 0 then ao21:=ao21+pi*2;
    if abs(pi*2-abs(ao21)) <= eps then ao21:=0;
  end;
end;

begin
  if r < 70000 then
    small_forw(b1,l1, fi,r, b2,l2, Result)
  else
    large_forw(b1,l1, fi,r, b2,l2, Result)
end;

end.
