unit dmw_cn; interface

uses
  Classes,Math,
  otypes,xlist,xclasses,
  dmw_dm;

type
  tdmw_Map = class(tdm_Map)

    function Break_mf(mf: TPolyList;
                      buf: PLLine; bufz: PIntegers;
                      bufMax: int): int;

    function Break_contour(dst: PLLine; dst_Max: Integer;
                           lp: PLPoly; hp: PIntegers;
                           lp_n,usag: Integer): Integer;

    function vc_merge(vc1,vc2: Cardinal): Boolean;
    function ve_merge(ve1,ve2: Cardinal): Boolean;

    procedure After_move(p: Int64);

  private
    function Get_ref(p: Int64; ref: TIntegerList): Integer;
    procedure Bound_ref(ref: TIntegerList);

    function fe_to_fe(id1,id2: Integer;
                      fe1: PIntegers; n1: Integer;
                      fe2: PIntegers; n2: Integer;
                      tags: tbyte_set): Integer;
  end;

implementation

uses
  xline;

function tdmw_Map.Break_mf(mf: TPolyList;
                           buf: PLLine; bufz: PIntegers;
                           bufMax: int): int;
var
  lp: PLPoly; cp: PIntegers; i,n,k: int;
begin
  buf.N:=-1;

  lp:=mf.First;
  cp:=mf.Parts.First;
  n:=mf.PartCount;

  for i:=0 to n-1 do begin k:=cp[i];
    Break_contour(buf,bufMax,lp,nil,k-1,1);
    lp:=@lp[k];
  end;

  Result:=buf.N
end;

function tdmw_Map.Break_contour(dst: PLLine; dst_Max: Integer;
                                lp: PLPoly; hp: PIntegers;
                                lp_n,usag: Integer): Integer;
const
  lp_Max = 8000;

function ve_lp(lp: PLPoly; n: Integer): PLLine;
begin
  Result:=@PWords(lp)[3]; Result.N:=n
end;

var
  dn, n1,n2: Integer;
  vc,vc1,vc2, ve: Cardinal;
  zp: PInteger; p: TPoint;
  lock: Boolean;
begin
  zp:=nil;

  if Tree.is_cn then

  if lp_n > 0 then begin
    lock:=Points_Equal(lp[0],lp[lp_n]);

    n1:=0; n2:=lp_n+1; vc:=0; vc1:=0; vc2:=0;

    while n1 < lp_n do begin
      dn:=n2; if dn > lp_Max then
      dn:=Min(lp_Max,dn div 2); Dec(n2,dn);

      if vc2 > 0 then vc1:=vc2 else begin
        if Assigned(hp) then zp:=@hp[0];
        vc1:=add_vc1(lp[n1],zp);
      end;

      if vc = 0 then vc:=vc1;

      if (n2 = 0) and lock then vc2:=vc else begin
        if Assigned(hp) then zp:=@hp[dn-1];
        vc2:=add_vc1(lp[n1+dn-1],zp);
      end;

      p:=lp[n1]; ve:=0;

      if (vc1 > 0) and (vc2 > 0) then begin
        if Assigned(hp) then hp:=@hp[1];
        ve:=cn_add(cn_edge,0, 1,1, vc1,vc2,
                   ve_lp(@lp[n1],dn-3), hp);
      end;

      lp[n1]:=p;

      if ve > 0 then begin
        p:=cn_ptr(ve,cn_edge,1,usag,255);
        if dst.N < dst_Max then begin
          Inc(dst.N); dst.Pol[dst.N]:=p
        end
      end;

      if Assigned(hp) then hp:=@hp[dn-1];
      Inc(n1,dn);
    end;
  end;

  Result:=dst.N
end;

procedure tdmw_Map.Bound_ref(ref: TIntegerList);
var
  i,id,p: Integer;
begin
  for i:=0 to ref.Count-1 do begin
    id:=ref.Values[i];
    p:=Offset_by_Id(id);

    if p > 0 then fe_bound(p)
  end
end;

function tdmw_Map.Get_ref(p: Int64; ref: TIntegerList): Integer;

procedure add_fe(ref: TIntegerList;
                 ve_Id,vc_Id: Cardinal);
var
  i,n: Integer; fe: tfe_id;
begin
  n:=Tree.Get_fe(ve_Id,vc_Id,@fe,0);
  for i:=0 to n-1 do ref.AddItem(fe[i])
end;

var
  x: TInt64; i: Integer;
begin
  x.x:=p; ref.Clear;
  ref.Duplicates:=false;

  if Enabled_Map then

  if x.cn = cn_node then begin
    add_fe(ref,0,x.id);

    for i:=0 to ve_Count-1 do
    add_fe(ref,ve_Id[i],x.id);
  end else
  if x.cn = cn_edge then
    add_fe(ref,x.id,0);

  Result:=ref.Count
end;

procedure tdmw_Map.After_move(p: Int64);
var
  list: TIntegerList;
begin
  list:=TIntegerList.Create;
  try
    list.Duplicates:=false;
    if Get_ref(p,list) > 0 then
    Bound_ref(list);
  finally
    list.Free
  end
end;

function tdmw_Map.fe_to_fe(id1,id2: Integer;
                           fe1: PIntegers; n1: Integer;
                           fe2: PIntegers; n2: Integer;
                           tags: tbyte_set): Integer;
var
  i,id,pt: Integer;
begin
  Result:=n2;

  for i:=0 to n1-1 do begin

    id:=fe1[i]; pt:=Offset_by_Id(id);
    if fe_ref(pt, id1,id2, tags) then

    if Int_Contains(fe2,Result,id) < 0 then
    if Result < fe_Max then begin
      fe2[Result]:=id; Inc(Result)
    end
  end
end;

function tdmw_Map.vc_merge(vc1,vc2: Cardinal): Boolean;
var
  ref: TIntegerList; ve: tcn_rec;
  i,n1,n2: Integer; x: TInt64;
  fe1,fe2: tfe_id; is_doc: Boolean;
begin
  Result:=false;

  ref:=TIntegerList.Create;
  try
    ref.Duplicates:=false;

    if Begin_Update then

    if Tree.vc_IndexOf(vc1) >= 0 then
    if Tree.vc_IndexOf(vc2) >= 0 then begin

      x.cn:=cn_node; x.id:=vc1;
      Get_ref(x.x,ref);

      n1:=Tree.Get_vc_ref(vc1,@fe1,0);
      for i:=0 to n1-1 do begin
        x.id:=fe1[i]; x.cn:=cn_edge;

        if Tree.get_ve(x.id,@ve) then begin

          if ve.vc1 = vc1 then ve.vc1:=vc2 else
          if ve.vc2 = vc1 then ve.vc2:=vc2;

          Undo_Object(x.x,undo_mov);
          Tree.Put_ve(x.x,@ve);

          Edit_Object(x.x,x.id,tx_edit_mf)
        end
      end;

      is_doc:=isDoctor; isDoctor:=true;

      n1:=Tree.Get_fe(0,vc1, @fe1,0);
      n2:=Tree.Get_fe(0,vc2, @fe2,0);

      n2:=fe_to_fe(vc1,vc2,
                   @fe1,n1,@fe2,n2,[21]);

      Tree.vc_Put_fe(vc1,nil,0);
      Tree.vc_Put_fe(vc2,@fe2,n2);

      Bound_ref(ref);

      IsDoctor:=is_doc; Result:=true
    end;

  finally
    ref.Free
  end;

  EditMode:=false;
end;

function tdmw_Map.ve_merge(ve1,ve2: Cardinal): Boolean;
var
  i,id,p,n1,n2: Integer;
  fe1,fe2: tfe_id; 
begin
  Result:=false;

  if Begin_Update then

  if Tree.ve_IndexOf(ve1) >= 0 then
  if Tree.ve_IndexOf(ve2) >= 0 then begin

    n1:=Tree.Get_fe(ve1,0, @fe1,0);
    n2:=Tree.Get_fe(ve2,0, @fe2,0);

    n2:=fe_to_fe(ve1,ve2,
                 @fe1,n1,@fe2,n2,[22,23]);

    Tree.ve_Put_fe(ve1,nil,0);
    Tree.ve_Put_fe(ve2,@fe2,n2);

    for i:=0 to n1-1 do begin
      id:=fe1[i]; p:=Offset_by_Id(id);
      if p > 0 then fe_bound(p)
    end;

    Result:=true
  end;

  EditMode:=false;
end;

end.
