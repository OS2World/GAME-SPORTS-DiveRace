(***************************************************************************
  Some Graphics Primitives which access the video-buffer.
****************************************************************************)

Unit GfxFunc;

interface

uses Use32;

type
  pixel = record
            x,y:SmallInt;
          end;




Procedure Span(y,x1,x2,c:integer);
Procedure SetPixel(x,y,c:integer);
procedure line(x1,y1,x2,y2,c:integer);
procedure FillTria(p1,p2,p3:pixel;c:integer);
procedure FillQuad(p1,p2,p3,p4:pixel;c:integer);
Procedure OutStr(x,y,fore:integer;s:string);

implementation

Uses DreiD;


type
 fix = record case boolean of            (* 32Bit Fixed-Point number *)
          true:(fix:longint);
          false:(f:SmallWord;i:SmallInt);
        end;


{$I FONT8x8.INC}


Procedure OutStr(x,y,fore:integer;s:string);
type
  Fontbuff = array[0..32000] of byte;
var
  line,spos,bit,bpos:integer;
  LineBuffer : array[0..640] of byte;
  ScrAddr:integer;
begin
  if length(s)>80 then
    s:=copy(s,1,80);

  scraddr:=y*xpels+x;
  for line:=0 to 7 do
  begin
    bpos:=scraddr;
    for spos:=1 to length(s) do
      for bit:=7 downto 0 do
      begin
        if (Font8x8[ord(s[spos]) shl 3+line] shr bit) and 1=1 then
          DispBuffer^[bpos]:=fore;
       (* else
          LineBuffer[bpos]:=back;*)
        inc(bpos);
      end;
    Inc(ScrAddr,xpels);
  end;
end;




Procedure Span(y,x1,x2,c:integer);
var h:integer;
begin
  if x1>x2 then begin h:=x1;x1:=x2;x2:=h;end;
  if (y>=0) and (y<=ymax) then (*y in range ?*)
    if (x1>=0) and (x1<=xmax) then
      if (x2>=0) and (x2<=xmax) then
        Fillchar(DispBuffer^[y*xpels+x1],x2-x1+1,c)
      else (*x2 has to be > xmax*)
        Fillchar(DispBuffer^[y*xpels+x1],xmax-x1+1,c)
    else
      if (x2>=0) and (x2<=xmax) then (*x2 ok ?->x1<0*)
        Fillchar(DispBuffer^[y*xpels],x2,c)
      else (*none ok*)
        if (x1<0) and (x2>xmax) then (*x1 left out and x2 on right*)
          Fillchar(DispBuffer^[y*xpels],xmax+1,c);
end;


Procedure SetPixel(x,y,c:integer);
begin
  if (x>=0) and (x<=xmax) and (y>=0) and (y<=ymax) then
    DispBuffer^[y*xpels+x]:=c;
end;



procedure line(x1,y1,x2,y2,c:integer);
var
  dyDdx,dxDdy,sy,sx:fix;
  dy,dx,h: integer;

begin
  dy:=abs(y2-y1);
  dx:=abs(x2-x1);
  if (dx>dy) then
  begin
    if x2<x1 then  {swap x1,x2}
    begin
      h:=x1;
      x1:=x2;
      x2:=h;
      h:=y1;
      y1:=y2;
      y2:=h;
    end;
    SetPixel(x1,y1,c);
    if dx<>0 then
    begin
      sy.i:=y1;
      sy.f:=$8000;
      dyDdx.i:=y2-y1;
      dyDdx.f:=0;
      dyDdx.fix:=dyDdx.fix div dx;
      while x1<x2-1 do
      begin
        INC(x1);
        INC(sy.fix,dyDdx.fix);
        SetPixel(x1,sy.i,c);
      end;
      SetPixel(x2,y2,c);
    end; { Ende mehr als ein Punkt }
  end { Ende dx>dy }
  else
  begin
    if y2<y1 then
    begin
      h:=x1;
      x1:=x2;
      x2:=h;
      h:=y1;
      y1:=y2;
      y2:=h;
    end;
    SetPixel(x1,y1,c);
    if dy<>0 then
    begin
      sx.i:=x1;
      sx.f:=$8000;
      dxDdy.i:=x2-x1;
      dxDdy.f:=0;
      dxDdy.fix:=dxDdy.fix div dy;
      while y1<y2-1 do
      begin
        INC(y1);
        INC(sx.fix,dxDdy.fix);
        setpixel(sx.i,y1,c);
      end;
      SetPixel(x2,y2,c);
    end; { Ende mehr als ein Punkt }
  end; { Ende dy>dx }
end;


procedure FillTria(p1,p2,p3:pixel;c:integer);

var x21,x31,x32,                       (* x-Differenzen der drei Kanten    *)
    y21,y31,y32:integer;               (* y-Differenzen der drei Kanten    *)
    ex,                   (* aktuelle Werte lange Kante (Festkomma 32 Bit) *)
    e1x,                  (* aktuelle Werte kurze Kante (Festkomma 32 Bit) *)
    edx,                  (* Steigungen lange Kante     (Festkomma 32 Bit) *)
    e1dx:fix;             (* Steigungen kurze Kante     (Festkomma 32 Bit) *)
    N: longint;                    (* Nenner Steigung Span (siehe Kasten)  *)
    ph: pixel;                     (* Hilfsvariable zum Sortieren der Pkt. *)
begin
  if LineMode then
  begin
    Line(p1.x,p1.y,p2.x,p2.y,c); (* Nur die Kanten zeichnen   *)
    Line(p2.x,p2.y,p3.x,p3.y,c);
    Line(p3.x,p3.y,p1.x,p1.y,c);
  end else begin

    if p1.y>p2.y then begin ph:=p1; p1:=p2; p2:=ph; end; (* p1,p2,p3 sor-  *)
    if p1.y>p3.y then begin ph:=p1; p1:=p3; p3:=ph; end; (* tieren. Es gilt*)
    if p2.y>p3.y then begin ph:=p2; p2:=p3; p3:=ph; end; (* p1y<=p2y<=p3y  *)

    y31:=p3.y - p1.y;                         (* L„nge der 'langen' Kante  *)
    if y31<>0 then begin                      (* Dreieck keine hor. Linie? *)
      y21:=p2.y-p1.y; x21:=p2.x-p1.x;         (* Differenzen 'kurze' Kante *)
      x31:=p3.x-p1.x;                         (* Breite der langen Kante   *)
      N:=longint(y21)*x31-longint(y31)*x21;   (* Siehe Kasten              *)
      (*if N<>0 then*) begin                      (* kein entartetes Dreieck:  *)
        SetPixel(p1.x,p1.y,c);                (* obersten Punkt ausgeben   *)
        INC(p1.y);                            (* p1.y = treibender Wert    *)
        ex.i:=p1.x; ex.f:=$8000;              (* Anfangswert x lange Kante *)
        edx.i:=x31; edx.f:=0;                 (* dx lange Kante            *)
        edx.fix:=edx.fix div y31;             (* dx/dy lange Kante         *)
        if y21<>0 then begin                  (* Kante (P1,P2) nicht horiz.*)
          e1x.fix:=ex.fix;                    (* Anfangswerte obere kurze  *)
          e1dx.i:=x21; e1dx.f:=0;             (* dx kurze Kante            *)
          e1dx.fix:=e1dx.fix div y21;         (* dx/dy kurze Kante         *)
          while p1.y<p2.y do begin (* fr alle Zeilen zwischen p1y u. p2y: *)
            INC(ex.fix,edx.fix);              (* inkrementieren. Als y-Ko- *)
            INC(e1x.fix,e1dx.fix);            (* alle Komponenten von e1   *)
            Span(p1.y,ex.i,e1x.i,c);          (* Span ]e,e1[  *)
            INC(p1.y);                        (* y fr e,e1 u. Span weiter *)
          end;                                (* Ende alle Spans ber P2   *)
          INC(ex.fix,edx.fix);                (* e inkrementieren. e liegt *)
        end;                        (* Ende Kante (P1,P2) nicht waagerecht *)
        p1.y:=p2.y;                 (* y-Koordinate fr e,e1 u. Spans      *)

        SetPixel(ex.i,p1.y,c);
        SetPixel(p2.x,p2.y,c);

        Span(p1.y,ex.i,p2.x,c);                     (* Span in H”he P2     *)
        INC(p1.y);                                  (* y einen tiefer      *)
        (* Ab hier folgt nun das gleiche Spiel fr die untere kurze Kante.
           Das Prinzip ist identisch, so daá auf Kommentar verzichtet
           wird. (.. wenn Turbo doch Spaltenbl”cke kopieren k”nnte!)       *)
        y32:=p3.y-p2.y;
        if y32<>0 then begin             (* Kante (P2,P3) nicht waagerecht *)
          x32:=p3.x-p2.x;
          e1x.i:=p2.x; e1x.f:=$8000;
          e1dx.i:=x32; e1dx.f:=0; e1dx.fix:=e1dx.fix div y32;
          while p1.y<p3.y do begin
            INC(ex.fix,edx.fix);
            INC(e1x.fix,e1dx.fix);
            Span(p1.y,ex.i,e1x.i,c);
            INC(p1.y);
          end; (* Ende alle Spans  unter P2 *)
        end; (* Ende untere kurze Kante nicht senkrecht *)
        SetPixel(p3.x,p3.y,c);
      end (* Ende kein entartetes Dreieck *)
(*      else
        write(#7);Line(p1.x,p1.y,p3.x,p3.y,c);*)
    end (* Ende keine vertikale Linie *)
    else Line(p1.x,p1.y,p3.x,p3.y,c);
  end; (* Ende trifillenabled *)
end; (* Ende FillTria() *)


procedure FillQuad(p1,p2,p3,p4:pixel;c:integer);
var l1,l2,l1dx,l2dx:fix;
    ph:pixel;
    under1,under2:boolean;

  Procedure AssignLine(var p1,p2:pixel;var l,ldx:fix);(*p2 muá unter p1 sein*)
  var dx,dy:integer;
  begin
    dx:=p2.x-p1.x;
    dy:=p2.y-p1.y;
    l.i:=p1.x;
    l.f:=$8000;
    ldx.i:=dx;
    ldx.f:=0;
    ldx.fix:=ldx.fix div dy;
  end;

   (*Fllt zwischen l1 und l2 bis y=ymax*)

  Procedure FillBetweenLines(y,ymax:integer);
  begin
    while y<=ymax do
    begin
      inc(l1.fix,l1dx.fix);
      inc(l2.fix,l2dx.fix);
      Span(y,l1.i,l2.i,c);
      inc(y);
    end;
  end;

begin
  if p1.y>p2.y then begin ph:=p1; p1:=p2; p2:=ph; end; (* p1,p2,p3,p4 sor*)
  if p1.y>p3.y then begin ph:=p1; p1:=p3; p3:=ph; end; (* tieren. Es gilt*)
  if p1.y>p4.y then begin ph:=p1; p1:=p4; p4:=ph; end;
  if p2.y>p3.y then begin ph:=p2; p2:=p3; p3:=ph; end; (*p1y<=p2y<=p3y<=p4y*)
  if p2.y>p4.y then begin ph:=p2; p2:=p4; p4:=ph; end;
  if p3.y>p4.y then begin ph:=p3; p3:=p4; p4:=ph; end;

  if p4.y<>p1.y then
  begin  (*nicht Horizontale Linie ?*)
    if p3.y<>p1.y then
    begin (*Keine Linie+Dreieck ?*)
      if p1.x=p3.x then
      begin
        under1:=p2.x<p1.x;
        under2:=p4.x<p1.x;
      end
      else
      begin
        under1:=p3.y-p2.y<(longint(p3.x-p2.x)*(p3.y-p1.y)) div (p3.x-p1.x);
        under2:=p3.y-p4.y<(longint(p3.x-p4.x)*(p3.y-p1.y)) div (p3.x-p1.x);
      end;

      SetPixel(p1.x,p1.y,c);
      if under1 xor under2  then
      begin (*p3 liegt unter der Geraden durch p2-p4*)
        if linemode then
        begin
          Line(p1.x,p1.y,p2.x,p2.y,c); (* Nur die Kanten zeichnen   *)
          Line(p2.x,p2.y,p3.x,p3.y,c);
          Line(p3.x,p3.y,p4.x,p4.y,c);
          Line(p4.x,p4.y,p1.x,p1.y,c);
        end
        else
        begin
          AssignLine(p1,p4,l1,l1dx);
          if p1.y=p2.y then
            Span(p1.y,p1.x,p2.x,c)
          else
          begin
            AssignLine(p1,p2,l2,l2dx);
            FillBetweenLines(p1.y+1,p2.y);
          end;
          if p2.y<>p3.y then
          begin
            AssignLine(p2,p3,l2,l2dx);
            FillBetweenLines(p2.y+1,p3.y);
          end;
          if p3.y<>p4.y then
          begin
            AssignLine(p3,p4,l2,l2dx);
            FillBetweenLines(p3.y+1,p4.y);
          end;
        end;
      end
      else
      begin
        if linemode then
        begin
          Line(p1.x,p1.y,p2.x,p2.y,c); (* Nur die Kanten zeichnen   *)
          Line(p2.x,p2.y,p4.x,p4.y,c);
          Line(p4.x,p4.y,p3.x,p3.y,c);
          Line(p3.x,p3.y,p1.x,p1.y,c);
        end
        else
        begin
          AssignLine(p1,p3,l1,l1dx);
          if p1.y=p2.y then
            Span(p1.y,p1.x,p2.x,c)
          else
          begin
            AssignLine(p1,p2,l2,l2dx);
            FillBetweenLines(p1.y+1,p2.y);
          end;
          if p2.y<>p4.y then
          begin
            AssignLine(p2,p4,l2,l2dx);
            if p2.y<>p3.y then
              FillBetweenLines(p2.y+1,p3.y);
            if p3.y<>p4.y then
            begin
              AssignLine(p3,p4,l1,l1dx);
              FillBetweenLines(p3.y+1,p4.y);
            end;
          end;
        end;
      end;
    end
    else
    begin  (*p1.y=p2.y=p3.y*)
      Span(p1.y,p1.x,p2.x,c);
      Span(p1.y,p1.x,p3.x,c);
      AssignLine(p2,p4,l1,l1dx);
      AssignLine(p3,p4,l2,l2dx);
      FillBetweenLines(p3.y+1,p4.y);
    end;
  end
  else
  begin (*p1=p2=p3=p4*)
    Span(p1.y,p1.x,p2.x,c);
    Span(p1.y,p1.x,p3.x,c);
    Span(p1.y,p1.x,p4.x,c);
  end;
end;


end.
