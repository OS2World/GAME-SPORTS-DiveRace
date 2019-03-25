(***************************************************************************
  DREID does 3-D calculations for the game.

  This Unit is ported from a stone-aged dos app and is quite proprietary for
  this Game. Perhaps you can make need of the graphics primitives in the
  gfx unit.

****************************************************************************)

unit dreid;


interface

uses OS2PMAPI,use32,gfxfunc;

const maxobjpts = 4;
      maxcarnum = 10;

      ymax=199;
      xmax=399; (*Value+1 has to be at DWORD boundary !!*)
      xpels=xmax+1;


type
  objekte = record
              Anz_Punkte : integer;
              maxwid : integer;
              ardir : integer;{Normal-Angle on Area, -1 for none}
              xm,ym,zm : longint; {Center}
              xo,yo,zo:array[0..maxobjpts-1] of Longint;{Object coords}
              dist:integer;
              Farbe : byte;
            end;

 carentry = record
              cx,cz : longint;    {Position}
              cyw : integer;      {Angle}
              carsp : integer;
              lastdist : integer;
              Col1,Col2 : byte;   {Colours}
            end;

  carlist = record
              carnum : integer;
              carinfo : array[0..MaxCarnum] of CarEntry;
            end;

     proj = record
              xp,yp,zp:array[0..maxobjpts-1] of Longint;{Position relativ to viewing point}
              disppunkte : integer;
              scr:array[0..maxobjpts] of pixel;       {Screen-coords}
              display:boolean;
            end;


  world = record
            objnum : integer;
            obj : array[1..500] of objekte;
          end;

  dispentry = record
                num : integer;
                objdist : longint;
              end;
  displist = record
               dispnum : integer;
               dispobj : array[1..MaxCarnum] of dispentry;
             end;



(*Procedure Mode(x:word);*)
Procedure Display(x,y,z:longint;yangle:integer;var cl:carlist);
Function pisin(faktor:integer;winkel:integer):longint;
Function picos(faktor:integer;winkel:integer):longint;
Function GetAngle(x,z:integer):integer;

Procedure Projektion(var o:Objekte;var p:proj);

Procedure InitDreid;


const
  faktor=5;
  {Keycodes}
  ESC = #27;
  CR  = #13;
  F1  = #59;
  F2  = #60;
  F3  = #61;
  F4  = #62;
  F5  = #63;
  F6  = #64;
  F7  = #65;
  F8  = #66;
  F9  = #67;
  F10 = #68;
  LeftKey  = #75;
  RightKey = #77;
  CtrlLeftKey = #115;
  CtrlRightKey = #116;
  CtrlPgUpKey = #132;
  CtrlPgDnKey = #118;
  UpKey    = #72;
  DownKey  = #80;
  PgUpKey  = #73;
  PgDnKey  = #81;
  HomeKey  = #71;
  EndKey   = #79;
  DelKey   = #83;
  InsKey   = #82;



type
  dispbuffer_arr = array[0..(ymax+1)*(xmax+1)-1] of Byte;

var
  tsin,tcos : array[0..360] of SmallInt;
  tatn      : array[0..100] of SmallInt;
  isin,icos : array[0..360] of SmallInt;
  linecol   : array[0..ymax] of integer;

  wo : world;
  dl : displist;
  linemode : boolean;
  test:integer;

  dispbuffer:^dispbuffer_arr;


implementation


const
      pa = (100*xpels) div 320; {Distance to projection area}


  ColOfs = 16;

  Black         = 0 +ColOfs;
  Blue          = 1 +ColOfs;
  Green         = 2 +ColOfs;
  Cyan          = 3 +ColOfs;
  Red           = 4 +ColOfs;
  Magenta       = 5 +ColOfs;
  Brown         = 6 +ColOfs;
  LightGray     = 7 +ColOfs;
  DarkGray      = 8 +ColOfs;
  LightBlue     = 9 +ColOfs;
  LightGreen    = 10+ColOfs;
  LightCyan     = 11+ColOfs;
  LightRed      = 12+ColOfs;
  LightMagenta  = 13+ColOfs;
  Yellow        = 14+ColOfs;
  White         = 15+ColOfs;


  baumstamm : objekte = (anz_punkte:4;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-8,-8,8,8);
                         yo:(0,50,50,0);zo:(0,0,0,0);dist:2000;farbe:Brown);
  baumkrone : objekte = (anz_punkte:3;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-38,0,38,0);
                         yo:(50,110,50,0);zo:(0,0,0,0);dist:2000;farbe:Green);

  cardist= 2000*faktor;
  carobjs= 9;
  car : array[1..carobjs] of objekte = (
                     (anz_punkte:4;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-30,-30,-30,-30);
                         yo:(5,0,0,5);zo:(-20,-20,-32,-32);dist:1000;farbe:DarkGray),
                     (anz_punkte:4;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(30,30,30,30);
                         yo:(5,0,0,5);zo:(-20,-20,-32,-32);dist:1000;farbe:DarkGray),
                     (anz_punkte:4;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-30,-30,-30,-30);
                         yo:(5,0,0,5);zo:(24,24,12,12);dist:1000;farbe:DarkGray),
                     (anz_punkte:4;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(30,30,30,30);
                         yo:(5,0,0,5);zo:(24,24,12,12);dist:1000;farbe:DarkGray),
                     (anz_punkte:3;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-30,-30,0,0);
                         yo:(5,5,10,0);zo:(-50,40,50,0);dist:2000;farbe:LightRed),
                     (anz_punkte:3;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(30,0,30,0);
                         yo:(5,10,5,0);zo:(-50,50,40,0);dist:2000;farbe:LightRed),
                     (anz_punkte:3;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(-30,0,0,0);
                         yo:(5,10,30,0);zo:(-50,50,-50,0);dist:2000;farbe:Blue),
                     (anz_punkte:3;maxwid:0;ardir:-1;xm:0;ym:0;zm:0;xo:(0,0,30,0);
                         yo:(30,10,5,0);zo:(-50,50,-50,0);dist:2000;farbe:Blue),
                     (anz_punkte:3;maxwid:0;ardir:0;xm:0;ym:13;zm:-50;xo:(-30,0,30,0);
                         yo:(5,30,5,0);zo:(-50,-50,-50,0);dist:2000;farbe:Blue));


var
  xv,yv,zv : longint; {Position}
  yw : integer; {viewing angle around y-axis}
  p : proj;


  sinyw,cosyw:integer;

const
  bufflen=sizeof(dispbuffer_arr);


Function pisin(faktor:longint;winkel:integer):LONGINT;
BEGIN
  pisin:=(faktor*tsin[winkel]) div 1000;
END;

Function picos(faktor,winkel:LONGINT):LONGINT;
BEGIN
  picos:=(faktor*tcos[winkel]) div 1000;
END;


Procedure ScanObjects(var w:world);

var f:text;
    s:string;
    o:objekte;
    i,dx,dy,dz,px,py,pz:integer;
    rc:longint;

  Procedure CutSpaces(var s:string);
  begin
    while (length(s)>0) and (s[1]=' ') do
      delete(s,1,1);
  end;

  Function GetNum(var s:string):integer;
  var n:integer;
  begin
    CutSpaces(s);
    n:=0;
    while (length(s)>0) and (s[1]>='0') and (s[1]<='9') do
    begin
      n:=n*10+(ord(s[1])-ord('0'));
      delete(s,1,1);
    end;
    GetNum:=n;
  end;

begin
  with w do
  begin
    ObjNum:=0;
    assign(f,'RACE.LST');
    {$I-}reset(f);{$I+}
    if ioresult<>0 then begin
      rc:=WinMessageBox( HWND_DESKTOP, HWND_DESKTOP,
         'File RACE.LST not in current directory !!',
         'Error!', 0, MB_OK or MB_MOVEABLE );
      HALT;
    end
    else
    begin
      repeat
        readln(f,s);
      until pos('OBJECTS:',s)<>0;
      readln(f,s);
      while pos('ANIMATORS:',s)=0 do
      begin
        if pos('"',s)<>0 then
        begin
          if pos('"RCTNGL',s)=6 then
          begin
            inc(ObjNum);
            with obj[Objnum] do
            begin
              readln(f,s);
              s:=copy(s,17,255);
              px:=Getnum(s);
              py:=Getnum(s)-20;{Adjust}
              pz:=Getnum(s);
              s:=copy(s,7,255);
              dx:=Getnum(s);
              dy:=Getnum(s);
              dz:=Getnum(s);
              Anz_Punkte:=4;
              xo[0]:=px;
              xo[1]:=px;
              xo[2]:=px+dx;
              xo[3]:=px+dx;
              zo[0]:=pz;
              zo[1]:=pz+dz;
              zo[2]:=pz+dz;
              zo[3]:=pz;
              yo[0]:=py;
              yo[1]:=py;
              yo[2]:=py+dy;
              yo[3]:=py+dy;
              readln(f,s);
              dist:=2000;
              if (pos('15x7',s)<>0) then
                Farbe:=Black
              else
                if (pos('4x8',s)<>0) then
                begin
                  Farbe:=White;
                  dist:=1000;
                end
                else
                  if (pos('8x5',s)<>0) or (pos('15,15',s)<>0) then
                    Farbe:=Brown
                  else
                    begin farbe:=White end;
            end;
          end
          else
            if pos('"QUAD',s)=6 then
            begin
              inc(ObjNum);
              with obj[Objnum] do
              begin
                readln(f,s);
                s:=copy(s,17,255);
                px:=Getnum(s);
                py:=Getnum(s)-20;{Adjust}
                pz:=Getnum(s);
                Anz_Punkte:=4;
                readln(f,s);
                if pos('15x7',s)<>0 then
                begin
                  farbe:=Black;
                  dist:=2000;
                end
                else
                  if pos('4x8',s)<>0 then
                  begin
                    farbe:=White;
                    dist:=1000;
                  end
                  else
                    farbe:=White;
                for i:=0 to 3 do
                begin
                  readln(f,s);
                  s:=copy(s,17,255);
                  dx:=Getnum(s);
                  dy:=Getnum(s);
                  dz:=Getnum(s);
                  xo[i]:=px+dx;
                  yo[i]:=py+dy;
                  zo[i]:=pz+dz;
                end;
              end;
            end
            else
              if pos('"TRNGLE',s)=6 then
              begin
                inc(ObjNum);
                with obj[Objnum] do
                begin
                  readln(f,s);
                  s:=copy(s,17,255);
                  px:=Getnum(s);
                  py:=Getnum(s)-20;{Adjust}
                  pz:=Getnum(s);
                  Anz_Punkte:=3;
                  readln(f,s);
                  dist:=2000;
                  if (pos('2x12',s)<>0) then
                    farbe:=Green
                  else
                    farbe:=White;
                  for i:=0 to 2 do
                  begin
                    readln(f,s);
                    s:=copy(s,17,255);
                    dx:=Getnum(s);
                    dy:=Getnum(s);
                    dz:=Getnum(s);
                    xo[i]:=px+dx;
                    yo[i]:=py+dy;
                    zo[i]:=pz+dz;
                  end;
                end;
              end;
        end;
        readln(f,s);
      end;
      close(f);
    end;
  end;
end;



Function GetAngle(x,z:integer):integer;

var
  ofs:integer;
begin
  if (x=0) and (z=0) then
    GetAngle:=0 {Sonderfall}
  else
    if abs(x)>=abs(z) then
      if (x>=0) and (z>=0) then
        GetAngle:=tatn[(longint(z)*100) div x]
      else
        if (x<0) and (z>=0) then
          GetAngle:=180-tatn[(longint(z)*100) div -x]
        else
          if (x<0) and (z<0) then
            GetAngle:=180+tatn[(longint(z)*100) div x]
          else
            GetAngle:=360-tatn[(longint(-z)*100) div x]
    else
      if (x>=0) and (z>=0) then
        GetAngle:=90-tatn[(longint(x)*100) div z]
      else
        if (x<0) and (z>=0) then
          GetAngle:=90+tatn[(longint(-x)*100) div z]
        else
          if (x<0) and (z<0) then
            GetAngle:=270-tatn[(longint(x)*100) div z]
          else
            GetAngle:=270+tatn[(longint(x)*100) div -z];
end;



Procedure InitSinCos;

var i:integer;
begin
  for i:=0 to 360 do
  begin
    tsin[i]:=round(sin(i*pi/180.0)*1000);
    tcos[i]:=round(cos(i*pi/180.0)*1000);
    isin[i]:=round(sin(i*pi/180.0)*1024); {Shiftable}
    icos[i]:=round(cos(i*pi/180.0)*1024);
  end;
  for i:=0 to 100 do
    tatn[i]:=round(arctan(i/100)*180.0/pi);
end;


Procedure ObjMult(var w:world;f:integer);

var k,i,j,m2:integer;
begin
  with w do begin
    i:=0;
    while i<objnum do begin
      inc(i);
(*    for i:=1 to objnum do begin
      k:=i;*)
      with obj[i] do
      begin
        xm:=0;
        ym:=0;
        zm:=0;
        dist:=dist*f;
        for j:=0 to anz_punkte-1 do
        begin
          xo[j]:=xo[j]*f;
          inc(xm,xo[j]);
          yo[j]:=yo[j]*f;
          inc(ym,yo[j]);
          zo[j]:=zo[j]*f;
          inc(zm,zo[j]);
        end;
        if anz_punkte=4 then
          maxwid:=round(sqrt(sqr(xo[2]-xo[0])+sqr(zo[2]-zo[0])))
        else
        begin
          maxwid:=round(sqrt(sqr(xo[1]-xo[0])+sqr(zo[1]-zo[0])));
          m2:=round(sqrt(sqr(xo[2]-xo[1])+sqr(zo[2]-zo[1])));
          if m2>maxwid then maxwid:=m2;
          m2:=round(sqrt(sqr(xo[0]-xo[2])+sqr(zo[0]-zo[2])));
          if m2>maxwid then maxwid:=m2;
        end;
        xm:=xm div anz_punkte;
        ym:=ym div anz_punkte;
        zm:=zm div anz_punkte;
      end;
    end;
  end;


  for i:=1 to carobjs do begin
    with car[i] do
    begin
      dist:=dist*f;
      for j:=0 to anz_punkte-1 do
      begin
        xo[j]:=xo[j]*f;
        yo[j]:=yo[j]*f;
        zo[j]:=zo[j]*f;
      end;
      xm:=xm*f;
      ym:=ym*f;
      zm:=zm*f;
      if anz_punkte=4 then
        maxwid:=round(sqrt(sqr(xo[1]-xo[0])+sqr(zo[2]-zo[1])))
      else
      begin
        maxwid:=round(sqrt(sqr(xo[1]-xo[0])+sqr(zo[1]-zo[0])));
        m2:=round(sqrt(sqr(xo[2]-xo[1])+sqr(zo[2]-zo[1])));
        if m2>maxwid then maxwid:=m2;
        m2:=round(sqrt(sqr(xo[0]-xo[2])+sqr(zo[0]-zo[2])));
        if m2>maxwid then maxwid:=m2;
      end;
    end;
  end;
end;



Procedure Drehe(var o:objekte;x,z:longint;w:integer);
var i:integer;
    x2:longint;
begin
  with o do
    for i:=0 to anz_punkte-1 do
    begin
      x2:=x-picos(xo[i],w)+pisin(zo[i],w);
      zo[i]:=z+picos(zo[i],w)+pisin(xo[i],w);
      xo[i]:=x2;
    end;
end;

Procedure AddTrees(var w:world);
var
  f:text;
  s:string[100];
  i,j : integer;
  x,z:integer;

begin
  assign(f,'COURSE1.TXT');
  {$I-}reset(f);{$I+}
  if ioresult<>0 then begin
    WinMessageBox( HWND_DESKTOP, HWND_DESKTOP,
       'File COURSE1.TXT not in current directory !!',
       'Error!', 0, MB_OK or MB_MOVEABLE );
    HALT;
  end
  else
    with w do
    begin
      readln(f,s);
      for i:=0 to 81 do
      begin
        readln(f,s);
        s:=copy(s,3,82);
        for j:=0 to 81 do
          if s[j+1] in ['>','^'] then
          begin
            inc(objnum);
            obj[objnum]:=Baumstamm;
            if s[j+1]='>' then
              Drehe(obj[objnum],j*100+50,i*100+50,90)
            else
              Drehe(obj[objnum],j*100+50,i*100+50,0);
            inc(objnum);
            obj[objnum]:=Baumkrone;
            if s[j+1]='>' then
              Drehe(obj[objnum],j*100+50,i*100+50,90)
            else
              Drehe(obj[objnum],j*100+50,i*100+50,0);
          end;
      end;
      close(f);
    end;
end;

Function To180(x:integer):integer;
begin
  x:=x mod 360;
  if x>180 then dec(x,360)
  else
    if x<-180 then inc(x,360);
  To180:=x;
end;

Procedure SortDispList(var dl:displist);
var c:integer;
    i:integer;
    h:longint;
begin
  with dl do begin
    repeat
      c:=0;
      for i:=1 to dispnum-1 do
        with dispobj[i] do
          if objdist<dispobj[i+1].objdist then
        begin
          h:=objdist;objdist:=dispobj[i+1].objdist;dispobj[i+1].objdist:=h;
          h:=num;num:=dispobj[i+1].num;dispobj[i+1].num:=h;
          inc(c);
        end;
    until c=0;
  end;
end;


Procedure Display(x,y,z:longint;yangle:integer;var cl:carlist);

const
  sky=11;
  ground=10;
var
  c:char;
  i,j:integer;
  odreh:objekte;
  cxf,czf,cdist:longint;
  adr:word;

  tempangle:integer;

begin
  with wo do
  begin
    xv:=-x*faktor;
    yv:=-y*faktor;
    zv:=-z*faktor;
    yw:=yangle;
    sinyw:=tsin[yw];
    cosyw:=tcos[yw];

(*    asm
      push ds
      pop es
      mov di,OFFSET Buffer
      cld
      mov al,100
    @1:
      mov ah,al
      mov cx,160
      rep stosw
      inc al
      cmp al,200
      jne @1
    end;*)

    adr:=0;
    for i:=0 to ymax do
    begin
      Fillchar(DispBuffer^[adr],xpels,linecol[i]);
      inc(adr,xpels);
    end;

    for i:=1 to objnum do
    begin
      with obj[i],p do
      begin
        if abs(xv+xm)+abs(zv+zm)<=dist then
        begin
          Projektion(obj[i],p);
          if display then
            if disppunkte=4 then
              FillQuad(scr[0],scr[1],scr[2],scr[3],Farbe)
            else
              if disppunkte=5 then
              begin
                FillTria(scr[0],scr[1],scr[2],Farbe);
                FillQuad(scr[2],scr[3],scr[4],scr[0],Farbe);
              end
              else
                if disppunkte=3 then
                begin
                  FillTria(scr[0],scr[1],scr[2],Farbe);
                end
        end;
      end;
    end;

    with cl,dl do
    begin
      dispnum:=0;
      for i:=1 to cl.carnum do
        with carinfo[i] do
        begin
          cdist:=abs(xv+cx*faktor)+abs(zv+cz*faktor);
          if cdist<=cardist then
          begin
            inc(dispnum);
            dispobj[dispnum].objdist:=cdist;
            dispobj[dispnum].num:=i;
          end;
        end;
    end;

    SortDispList(dl);

    with dl,cl do (*Show cars*)
    begin
      for i:=1 to dl.dispnum do
        with carinfo[dispobj[i].num] do
        begin
          cxf:=cx*faktor;
          czf:=cz*faktor;
          car[7].farbe:=col1;
          car[8].farbe:=col1;
          car[9].farbe:=col1+8;
          for j:=1 to carobjs do
          begin
            if dispobj[i].objdist<=car[j].dist then
            begin
              odreh:=car[j];
              with odreh,p do
              begin
                if ardir<0 then
                  display:=true
                else
                begin
                  drehe(odreh,cxf,czf,cyw);

                  tempangle:=GetAngle(zo[1]+zv,xo[1]+xv);
                  display:=abs(to180(ardir-cyw+tempangle))<=90;

{                  display:=abs(to180(ardir-cyw+GetAngle(zo[1]+zv,xo[1]+xv)))<=90;}
                end;
                if (dispobj[i].num=2) and (ardir>=0) then
                  test:=GetAngle(zo[1]+zv,xo[1]+xv);
                if display then
                begin
                  if ardir<0 then
                    drehe(odreh,cxf,czf,cyw);
                  Projektion(odreh,p);
                  if display then
                    if disppunkte=4 then
                    begin
                      FillQuad(scr[0],scr[1],scr[2],scr[3],Farbe)
                    end
                    else
                      if disppunkte=5 then
                      begin
                        FillTria(scr[0],scr[1],scr[2],Farbe);
                        FillQuad(scr[2],scr[3],scr[4],scr[0],Farbe);
                      end
                      else
                        if disppunkte=3 then
                        begin
                          FillTria(scr[0],scr[1],scr[2],Farbe);
                        end
                end;
              end;
            end;
          end;
        end;
    end;

(*    asm
      mov si,OFFSET Buffer
      mov ax,0a000h
      mov es,ax
      mov di,10*320
      mov cx,Bufflen
      shr cx,1
      cld
      rep movsw
    end;*)
(*    Move(Buffer,Mem[$a000:10*320],sizeof(Buffer));*)
  end;
end;


Procedure Projektion(var o:Objekte;var p:proj);
(*begin end;*)

var i,pts:integer;
    dispcnt :integer;

   Function Norm(x:integer):integer;
   begin
     if x<0 then
       inc(x,pts)
     else
       if x>=pts then
         dec(x,pts);
     Norm:=x;
   end;

   Function Interp(x1,x2,z1,z2:Longint):longint;
   begin (*x1 lies behind projection area, x2 in front of it*)
     Interp:=( ((x1-x2)*(z2-pa)) div (z2-z1)) + x2;
   end;

BEGIN
  With o,p do
  begin
    pts:=Anz_Punkte;
    dispcnt:=0;
    display:=false;
    zp[0]:=picos(zo[0]+zv,yw)+pisin(xo[0]+xv,yw);
(*      if (abs(xo[0]+xv)>maxint) or (abs(zo[0]+zv)>maxint) then
        begin xv:=xv;end;*)
    if zp[0]<pa-maxwid then
    begin
      exit;
    end;
    if zp[0]>=pa then inc(dispcnt);
    for i:=1 to pts-1 do
    begin
(*      if (abs(xo[i]+xv)>maxint) or (abs(zo[i]+zv)>maxint) then
        begin xv:=xv;end;*)
      zp[i]:=picos(zo[i]+zv,yw)+pisin(xo[i]+xv,yw);
      if zp[i]>=pa then inc(dispcnt);
    end;
    if dispcnt>0 then
    begin
      for i:=0 to pts-1 do
      begin
        xp[i]:=picos(xo[i]+xv,yw)-pisin(zo[i]+zv,yw);
        yp[i]:=yo[i]+yv;
      end;

      display:=true;
      disppunkte:=pts;
      if dispcnt=pts then
      begin
        for i:=0 to pts-1 do
          with scr[i] do
          begin
            x:=(xmax shr 1)+((pa*xp[i]) div zp[i]);(*ProjMul(xp[i],zp[i]);*)
            y:=(ymax shr 1)-((pa*yp[i]) div zp[i]);(*ProjMul(yp[i],zp[i]);*)
(*            x:=(xmax shr 1)+((pa*xp[i]) div zp[i]);
            y:=(ymax shr 1)-((pa*yp[i]) div zp[i]);*)
          end
      end
      else
      begin
        disppunkte:=0;
        for i:=0 to pts-1 do
          if zp[i]>=pa then (*calculate the correct points*)
          begin
            with scr[disppunkte] do
            begin
              x:=(xmax shr 1)+((pa*xp[i]) div zp[i]);(*ProjMul(xp[i],zp[i]);*)
              y:=(ymax shr 1)-((pa*yp[i]) div zp[i]);(*ProjMul(yp[i],zp[i]);*)
(*              x:=(xmax shr 1)+((pa*xp[i]) div zp[i]);
              y:=(ymax shr 1)-((pa*yp[i]) div zp[i]);*)
            end;
            inc(disppunkte);
            if zp[norm(i+1)]<pa then (*next point invisible ?*)
            begin
              with scr[disppunkte] do
              begin
                x:=(xmax shr 1)+Interp(xp[norm(i+1)],xp[i],
                                                    zp[norm(i+1)],zp[i]);
                y:=(ymax shr 1)-Interp(yp[norm(i+1)],yp[i],
                                                    zp[norm(i+1)],zp[i]);
              end;
              inc(disppunkte);
            end;
          end
          else (*Jetziger nicht sichtbar*)
            if zp[norm(i+1)]>=pa then (*N„chster Punkt wieder sichtbar ?*)
            begin
              with scr[disppunkte] do
              begin
                x:=(xmax shr 1)+Interp(xp[i],xp[norm(i+1)],
                                                    zp[i],zp[norm(i+1)]);
                y:=(ymax shr 1)-Interp(yp[i],yp[norm(i+1)],
                                                    zp[i],zp[norm(i+1)]);
              end;
              inc(disppunkte);
            end;
      end;
    end;
  end;
end;


Procedure InitDreid;
var i:longint;
begin
  InitSinCos;
  ScanObjects(wo);
  AddTrees(wo);
  ObjMult(wo,faktor);
  with wo do
  begin
    xv:=-3000*faktor;
    yv:=-100;
    zv:=-200*faktor;
    yw:=270;
  end;
(*  FillChar(DispBuffer^,sizeof(DispBuffer_Arr),0);*)
  linemode:=false;
  for i:=0 to ymax do
    linecol[i]:=((i*99) div ymax)+100;
end;

end.
