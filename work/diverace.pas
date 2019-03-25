(***************************************************************************
  DIVERACE a Game-Programming example for Virtual Pascal.

  Author  :  Michael Mrosowski
             Fidonet:  2:240/5022.1
             EMail:    lagaffe@offline.sh.sub.de

  Last Chg:  19.08.95

  This game and the sourcecode, as far as it is not copyrighted by other
  parties is free for use. It is released for the purpose to promote game
  programming under os/2.

  Do not expect a full documented full-blown 100% perfect code. This is just
  an old dos game which i ported to os/2. But i hope this game can at least
  be an example how to use multithreading and MMPM to write a game.

  If you want futher development, don't ask me. You have the source
  code so you can do it ;-).

  Perhaps some time a new version of the game is released.

  What is still to do:
    DIVE:    The new Fullscreen DIVE interface is not yet implemented.
             Look for the file DIVEFS2.ZIP for docs.

    JOYSTICK:The game uses direct port io which ist not the best method.
             Look for the files JOYSTICK.ZIP and JOYDOCS.ZIP for a OS/2
             MMPM joystick driver.

    SOUND:   A playlist with alternatively updated datablocks and
             cuepoint-operations is used. This takes much proc-time for
             message processing etc. There is a new example for direct-
             driver access. Look for DIRAUD.ZIP for documentation.

             The Sound-Part is only roughly tested and may not run
             with some drivers. For the GUS it works with the Gravis drivers
             but not with the manley-drivers. The Problem here is that
             the driver is reading the playlist-buffer once and then is
             ignoring any changes in the buffer.

    ERRORS:  Error processing is minimal or not-existing in this example.


    To compile:

    Put the .LIB files in the LIB directory.
    The .res and .rc have probably to stay in the output dirs.
    The .def has to stay in the source-dir.

    Turn Stack-Checking off in the Compiler settings.
    Link as PM-Application.

    The Compiler is: Virtual Pascal Version 1.0 Beta (#003)
                     (i applied no fixes yet)
                     from : Vitaly Miryanov


    Used sources of information -> Credits:
      SHOW.C of IBM for dive-programming.
      The MM4EMX package of Marc E.E. van Woerkom for the MCI-Interface.
      The plug-n-play sound source from Semir Patel.

****************************************************************************)


Program DiveRace;

{$S-}
{$R-}


uses Os2Def, Os2Base, Os2PmApi, Use32,Strings,Dive,DreiD,GfxFunc,SoundDrv;


{$IFDEF DYNAMIC_VERSION}
  {$Dynamic System }
  {$L VPRTL.LIB}
{$ENDIF}



(*Thread List:

Prio:       ³Name:           ³ Description:
-----------------------------³--------------------------------------------
Regular     ³Main Thread     ³ Message Processing for Dive Window
            ³                ³
Idle+10     ³Blitter Thread  ³ Setting up Graphics Buffer and Blit Buffer
            ³                ³ to Screen
            ³                ³
Regular     ³Game Thread     ³ Game Control & Simulation
            ³                ³
            ³                ³
Timecrit-2  ³Sound Thread    ³ Performing Sound Mixing.   (if enabled)
Timecrit-1  ³Timer Thread    ³ Providing Timed Information
Timecrit    ³JoyStick Thread ³ Checking Joystick Controls (if available)

*)


(*Race Globals*)



const
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

  carcol : array[0..7] of byte = (White,Blue,Brown,Cyan,Red,Green,Magenta,Black);
  maxpos = 200;
  maxsub = 10;

  dirincmax = 40;
  spincmax = 12;
  dirincdiv = 2;
  dirincinc = 6;
  dirincdec = 8;

  linedist  = 1000;


  zentmax1 = 3000; {Car Direction Centering "Force" on Street}
  zentmax2 = 1000;(*1000*) {On Grass}
  maxspeed = 600;

  carmaxsp = 600; {Maximum "Speed" for Computer Cars}
  caracc   = 5;   {Car accelleration}


var
  ground : array[0..81,0..9] of byte; (*Ground-Matrix: Street or Grass*)

  KeyHeldDown : array[1..127] of boolean; (*Keyboard Scancode-Info *)

  dirinc,                 (*Direction-Change Value of user-controlled car*)
  lastdirinc,             (*Dir-Change of previous sim-turn*)
  keydirinc,              (*Keyboard Steering-Value*)
  speed,                  (*Speed of User controlled car*)

  xrest,                  (*X-Move Remainder of User-Controlled Car*)
  zrest,                  (*Z-Move Remainder of User-Controlled Car*)
                          (*Remainders are kept for next Simulation Turn*)

  dx,                     (*X-Move*)
  dz : longint;           (*Z-Move*)

  skid,lastskidded : boolean; (*Car Skidding, was Skidding ?*)

  zentri,zentmax,         (*Centering-"Force" of user-controlled car*)
  preskid,                (*Before-Skid countdown*)

  dw: longint;            (*Turn-Angle change from last Sim-Turn*)

  godir,                  (*Direction where car actually moves*)
                          (*godir<>y_angle when skidding*)
  sp : integer;           (*Speed Increment*)

  y_angle : longint;      (*User Controlled car, Turn Angle*)
  px,py,pz : longint;     (*Car-Position*)

  cars:carlist;           (*Data for cars in Game*)

  Timer : longint;        (*Counter of Timer-Events*)

  TimeStart,TimeStop,     (*Statistical Values*)
  Frames,calcs:longint;

  xv,yv,zv,yw:integer;    (*Eye position and angle*)

  tarcsin : array[-100..100] of integer;

  (***************************************)
  Laps,                   (*Display Stats*)
  GameTime,
  BestTime : integer;

  (***************************************)

type params = record
                Joystick,
                Sound,
                Test,
                Up,
                Wave,
                NoTrans,
                PauseNoFocus : boolean; (*Program Parameters*)
                Reserved : array[0..62] of byte;
              end;
var
  parms : params;

(***************************************************************************)

Function GetCfgName:string;
var cfgname:string;
begin
  cfgname:=paramstr(0);
  if cfgname='' then
    cfgname:='DIVERACE';
  if pos('.',cfgname)<>0 then
    cfgname:=copy(cfgname,1,pos('.',cfgname)-1);

  GetCfgName:=cfgname+'.CFG';
end;


Procedure GetParams;
var
  cfg:file of params;
begin
  Fillchar(parms,sizeof(params),FALSE);
  parms.PauseNoFocus:=TRUE;
  Assign(cfg,GetCfgName);
  {$I-}Reset(cfg);{$I+}
  If ioresult=0 then begin
    Read(cfg,parms);
    Close(cfg);
  end;
end;

Procedure WriteParams;
var
  cfg:file of params;
begin
  Assign(cfg,GetCfgName);
  Rewrite(cfg);
  If ioresult=0 then begin
    Write(cfg,parms);
    Close(cfg);
  end;
end;

(***************************************************************************)

(*Angle-Conversion Routines*)
Function To360(x:longint):Longint;
begin
  x:=x mod 360;
  if x>360 then dec(x,360)
  else
    if x<0 then inc(x,360);
  To360:=x;
end;

Function To180(x:longint):Longint;
begin
  x:=x mod 360;
  if x>180 then dec(x,360)
  else
    if x<-180 then inc(x,360);
  To180:=x;
end;

(*Is Ground at y,x Street or Grass ?*)
Function GroundOk(y,x:integer):boolean;
begin
  GroundOk:=Ground[x,y shr 3] and (1 shl (y and 7))=0;
end;

(*Range-Check for values*)
Function Range(x:Longint;min,max:Longint):Longint;
begin
  if x<min then x:=min
    else if x>max then x:=max;
  Range:=x;
end;

(*Convert String To Upper Case*)
Function Strupcase(s:string):string;
var i:integer;
begin
  for i := 1 to Length(s) do
    s[i] := UpCase(s[i]);
  Strupcase:=s;
end;

(*Find a Commandline Parameter*)
Function FindParam(s:string):boolean;
var i:integer;
begin
  s:=strupcase(s);
  FindParam:=false;
  for i:=1 to paramcount do
    if pos(s,strupcase(paramstr(i)))<>0 then
      FindParam:=true;
end;

(*Inits ArcSin Array*)
Procedure InitArcSin;

const
  cpi=3.141592654;

var i,j:longint;
    dt,dtalt:double;
    adt,adtalt:double;

    sinarg,sinval:double;


begin
  i:=270;
  j:=-100;
  while j<=100 do
  begin
    dt:=sin(i*pi/180.0)-j/100;
    repeat
      inc(i);
      if i=360 then i:=0;
      dtalt:=dt;

      sinarg:=(i*cpi)/180.0;
      sinval:=sin(sinarg);

      dt:=sin((i*cpi)/180.0)-j/100;

      adt:=abs(dt); (*compiler-error circumvent*)
      adtalt:=abs(dtalt);

    until adt>adtalt;
    dec(i);
    if i<0 then inc(i,360);
    tarcsin[j]:=i;
    inc(j);
  end;
end;


(***************************************************************************)
(*Collision Check for cars*)
Function CheckCol(var cars:carlist;var chx,chz,chsp:integer;chyw,notcheck:integer):boolean;
const
(*  coldist : array[0..18] of integer =
             (50,60,70,75,65,55,45,40,35,30,35,40,45,55,65,75,70,60,50);*)
  coldist : array[0..18] of integer =
             (60,70,80,85,75,60,50,45,35,30,35,45,50,60,75,85,80,70,60);
var
  i,w,d,maxd:integer;
begin
  CheckCol:=false;
    for i:=0 to cars.carnum do begin
      if i<>notcheck then begin
        with cars.carinfo[i] do
        begin
          d:=abs(cx-chx)+abs(cz-chz);
          if d<180 then
          begin
            w:=GetAngle(cx-chx,cz-chz);
            maxd:=coldist[abs(to180(w-chyw)) div 10]+
                  coldist[abs(to180(w-180-cyw)) div 10];
            if d<maxd then
            begin
              if(i=0) then
                PlayVoice(2);

              d:=maxd-d;
              if abs(to180(w-chyw))<90 then {Stoá nach vorne ?}
              begin
                CheckCol:=TRUE;
                if chsp>d then dec(chsp,d)
                  else chsp:=0;
              end;

              chx:=chx-((cx-chx)*(d+10)) div maxd;
              chz:=chz-((cz-chz)*(d+10)) div maxd;
            end;
          end;
        end;
      end;
    end;
end;

(***************************************************************************)
(*Sound of Computer-Controlled cars*)
Procedure Carsound(var c:integer);
const
(*  volmax = 6;*)
  VolMax = 100;
var
  dx,dz,dist,Vol,Volf,VolL,VolR,doppler:integer;
begin
  with cars.carinfo[c] do
  begin
    dx:=cx-px;             {Sound}
    dz:=cz-pz;
    dist:=abs(dx)+abs(dz);
    doppler:=(lastdist-dist) div 10;
    if abs(doppler)>50 then
      if doppler<0 then doppler:=-50 else doppler:=50;
    if Dist<1000 then
    begin
      Vol:=VolMax-((VolMax)*dist) div 1000;
      Volf:=8000+((Carsp+doppler)*40000) div carmaxsp;
      Volf:=Volf+((Volf*doppler) div 100);

      If Volf<1000 then Volf:=1000;
      If Volf>80000 then Volf:=80000;

      SetVoiceRate(c+3,Volf);
      SetVoiceVol(c+3,Vol,Vol);

      If not VoiceIsPlaying(c+3) then
        PlayVoice(c+3);
    end
    else
      If VoiceIsPlaying(c+3) then StopVoice(c+3);

    lastdist:=dist;
  end;
end;


(***************************************************************************)
(*Simulation Part for computer-controlled cars*)
Procedure AutoCar;

var
  k,c,dw,i:integer;
  endit:boolean;

  Function Dirok(var c:carentry;w:integer):boolean;

  begin
    with c do
      Dirok:=groundok((cx+pisin(400,w)) div 100,
                      (cz+picos(400,w)) div 100);

  end;

begin
  with cars do
  begin
    for k:=1 to carnum do begin
      c:=k;
      with carinfo[c] do
      begin
        if not DirOk(carinfo[c],cyw) then
        begin
          dw:=0;
          endit:=false;
          repeat
            inc(dw,5);
            if DirOk(carinfo[c],To360(cyw-dw)) then
            begin
              cyw:=To360(cyw-dw);
              endit:=true;
            end
            else
              if  DirOk(carinfo[c],To360(cyw+dw)) then
              begin
                cyw:=To360(cyw+dw);
                endit:=true;
              end
              else
                if dw>=5 then
                begin
                  if carsp>70 then
                    dec(carsp,20+random(30-(2*c)));
                end;
          until endit;
        end
        else
          if carsp<carmaxsp then
            inc(carsp,caracc);
        cz:=cz+longint(tcos[cyw])*carsp div 10000;
        cx:=cx+longint(tsin[cyw])*carsp div 10000;
        if not groundok(cx div 100,cz div 100) then
          if carsp>carmaxsp shr 2 then
             dec(carsp,spincmax);


        if CheckCol(cars,integer(cx),integer(cz),carsp,cyw,c) then
          if dirok(carinfo[c],to360(cyw-5)) then
            cyw:=to360(cyw-5)
          else
          if dirok(carinfo[c],to360(cyw+5)) then
            cyw:=to360(cyw+5);

        CarSound(c);
      end;
    end;
  end;
end;

(***************************************************************************)
(*Read Ground-Info File*)
Procedure GetGround;
var
  f:text;
  s:string[100];
  i,j : integer;
begin
  fillchar(ground,sizeof(ground),0);
  assign(f,'COURSE1.TXT');
  {$I-}reset(f);{$I+}
  if ioresult<>0 then begin
    WinMessageBox( HWND_DESKTOP, HWND_DESKTOP,
       'File COURSE1.TXT not in current directory !!',
       'Error!', 0, MB_OK or MB_MOVEABLE );
    HALT;
  end
  else
  begin
    readln(f,s);
    for i:=0 to 81 do
    begin
      readln(f,s);
      s:=copy(s,3,82);
      for j:=0 to 81 do
        if s[j+1]<>' ' then
          ground[i,j shr 3]:=ground[i,j shr 3] or (1 shl (j and 7));
    end;
    close(f);
  end;
end;

(***************************************************************************)
(*Players Car Sound*)

Procedure DriveSound(x:longint);
begin
  x:=8000+(x*40000) div maxspeed;
  SetVoiceRate(0,x);
end;

(***************************************************************************)
(*Car Simulation*)
Procedure CarControl;
begin
  if not skid then
    if speed<0 then
    begin
      if sp>0 then inc(speed,sp);
    end
    else
      if (speed<maxspeed) or (sp<0) then
      begin
        inc(speed,sp);
        if speed>maxspeed then speed:=maxspeed
        else
          if speed<0 then speed:=0;
      end;

  DriveSound(Speed);
  if speed<>0 then
  begin
    if groundok(px div 100,pz div 100) then
    begin
      zentmax:=zentmax1;
     (* Mem[$a000:150*320+20]:=0;*)
    end
    else
    begin
      zentmax:=zentmax2;
      (*Mem[$a000:150*320+20]:=9;*)
      if speed>maxspeed div 2 then
      begin
        if sp>0 then dec(speed,sp);
        if speed>maxspeed div 2 then
          dec(speed,spincmax);
      end;
(*      if speed>maxspeed div 4 then
      begin
        if sp>0 then dec(speed,sp);
        if speed>maxspeed div 4 then
          dec(speed,spincmax);
      end;*)
    end;

    if not lastskidded then    {Eigentlich div 100 aber speed ist 10fach}
      zentri:=(tsin[to360( dirinc*speed div  1000 )] * speed) div 10 {1000fach}
    else
      zentri:=(tsin[to360( y_angle+lastdirinc-godir)] * speed) div 10;

    skid:=false;
    if abs(zentri)>zentmax then begin
      if preskid<=0 then
        skid:=true
      else begin
        dec(preskid,(abs(zentri)-zentmax) div 4);
        If preskid>=0 then
          SetVoiceVol(1,((zentmax-preskid)*100) div zentmax,
                        ((zentmax-preskid)*100) div zentmax);
        If not VoiceIsPlaying(1) then PlayVoice(1);
      end;
    end
    else begin
      If VoiceIsPlaying(1) then StopVoice(1);
      preskid:=zentmax;
    end;

    if not skid then (*No Skidding -> car drives in view direction*)
    begin

      if lastskidded then
        speed:=(tcos[to360(y_angle-godir)]*longint(speed)) div 1000;

      dx:=(tsin[to360(dirinc div 2 + y_angle)]*speed) div 10 + xrest;
      dz:=(tcos[to360(dirinc div 2 + y_angle)]*speed) div 10 + zrest;
      lastdirinc:=dirinc*speed div 1000;
      y_angle:=to360(y_angle+lastdirinc);
      godir:=y_angle;
    end
    else
    begin
      If not VoiceIsPlaying(1) then PlayVoice(1);

      dx:=(tsin[godir]*speed) div 10 + xrest;
      dz:=(tcos[godir]*speed) div 10 + zrest;
      y_angle:=to360(y_angle+lastdirinc);
      dw:=to180(y_angle-godir);
      if abs(dw)<90 then
        if dw>0 then
          godir:=to360(godir+(tcos[to360(dw)]*tarcsin[zentmax div speed]) div 1000)
        else
          godir:=to360(godir-(tcos[to360(dw)]*tarcsin[zentmax div speed]) div 1000);
      if speed>0 then
      begin
        dec(speed,abs(dirincmax*tsin[to360(y_angle+lastdirinc-godir)]) div 1000);
        if speed<0 then speed:=0;
      end
      else
      begin
        inc(speed,abs(dirincmax*tsin[to360(y_angle+lastdirinc-godir)]) div 1000);
        if speed>0 then speed:=0;
      end;
    end;
    lastskidded:=skid;
    lastdirinc:=(lastdirinc*19) div 20;

    xrest:=dx mod 1000;
    zrest:=dz mod 1000;
    dx:=dx div 1000;
    dz:=dz div 1000;
  end
  else
  begin
    dx:=0;
    dz:=0;
    xrest:=0;
    zrest:=0;
    skid:=false;
    lastskidded:=false;
    If VoiceIsPlaying(1) then StopVoice(1);
  end;
end;


(***************************************************************************)

(* Other Game Globals *)



type
  WinData = record
    fVrnDisabled:Bool;       (* ????  Visual region enable/disable  *)
    fChgSrcPalette:Bool;     (* Flag for change of source palette   *)
    hwndFrame:HWND;          (* Frame window handle                 *)
    hwndClient:HWND;         (* Client window handle                *)
    hDive:HDIVE;             (* DIVE handle                         *)
    ulnumColor:ULONG;        (* Number of colors in bitmaps         *)
    ulWidth:ULONG;           (* Bitmap width in pels                *)
    ulHeight:ULONG;          (* Bitmap Height in pels               *)
    fccColorFormat:FOURCC;   (* Bitmap color format                 *)
    ulSrcLineSizeBytes:ULONG;(* source linesize                     *)
 end;

 p_WinData = ^WinData;

 t_rgbx = record
            b,g,r,x:byte;
          end;

 t_rgbxpal8 = array[0..255] of t_rgbx;
 p_rgbxpal8 = ^t_rgbxpal8;

const

(*Window ID's*)

  ID_MAINWND=256;
  ID_OPTIONS=257;
  ID_SNAP   =258;
  ID_SNAP2  =259;
  ID_SNAPFULL=264;
  ID_EXIT   =260;
  ID_NEWTEXT=261;
  ID_START  =262;
  ID_PAUSE  =263;

  ID_STARTUPDLG=300;
  ID_BITMAP =301;

  ID_SOUND  =320;
  ID_MOUSE  =321;
  ID_JOY    =322;
  ID_TEST   =323;
  ID_WAVE   =324;
  ID_TRANS  =325;
  ID_FOCUS  =326;


  (*Source Color-Type definitions, LUT8 is good for 8-Bit Source-Images
    it uses a color lookuptable.

    You can disable coltranslate, but this speeds only up if destination
    is 1:1 and Display Depth is also 8-Bit

    Good Video drivers could make translation and sizing with hardware ;-)
    Then its also useless to stop coltranslate
    *)


  FOURCC_LUT8 = ord('L')+(ord('U') shl 8)+(ord('T') shl 16)+(ord('8') shl 24);
  FOURCC_R565 = ord('R')+(ord('5') shl 8)+(ord('6') shl 16)+(ord('5') shl 24);
  FOURCC_BGR4 = ord('B')+(ord('G') shl 8)+(ord('R') shl 16)+(ord('4') shl 24);
  FOURCC_SCRN = 0;

  WM_VRNDISABLED = $007E;
  WM_VRNENABLED  = $007F;



var
  h_ab:HAB;                                (* PM anchor block handle            *)
  h_dc:HDC;                                (* Device Context*)
  h_pal:HPAL;                              (* Game Palette Handle*)
  ulImage:   ULONG;    (* Image buffer number from Dive    *)
  pPalette:  Pointer;  (* Pointer to bitmap palette area    *)

  timerevent:hev; (*Timer-Event *)
  BlitEvent:hev;  (*Blit Synchronization with Game Thread*)
  SimDataAccess:hev; (*Mutex-Sem for Simulation Data Access*)

  win_Data:WINDATA;         (* window data                          *)

  screenx,screeny:Integer;               (* Screen Size*)

  MainWav:Integer; (*Number/Handle of Car-Sound Wave-Info*)
  CrashWav:Integer;


const
  pszMyWindow:Pchar = 'MyWindow';         (* Window class name              *)
  pszTitleText:Pchar = 'Dive-Drive !';    (* Title bar text                 *)
  outputWinSize:string[80]='';

  ulToEnd:Ulong = 0;                     (* End Blitting-Thread             *)
  PauseDisplay:Boolean = FALSE;          (* Indicator to Pause Game         *)
  coltranslate:boolean = TRUE;           (* Translate 256 Colors for Dive ? *)



(***************************************************************************)

(*Integer to String*)
Function Strint(x:longint):string;
var s:string[20];
begin
  Str(x,s);
  StrInt:=s;
end;

(*Real to String*)
Function StrReal(x:real;len,prec:integer):string;
var s:string[20];
begin
  Str(x:len:prec,s);
  StrReal:=s;
end;

(***************************************************************************)
var achTitle: array[0..255] of char;
Procedure SetMainWindowTitle(s:string);
begin
  StrPCopy(@achTitle,s);
  WinPostMsg ( win_Data.hwndFrame, WM_COMMAND,
               ID_NEWTEXT, longint(@achTitle));
end;

(***************************************************************************)
Procedure ShowTime(x,y:longint;Time:longint;colf:byte);
type
  str2=string[2];
var
  min,sec,cent:longint;

begin
  cent:=(Time*1000) div 182;
  sec:=cent div 100;
  cent:=cent mod 100;
  min:=sec div 60;
  sec:=sec mod 60;
  OutStr( x , y ,Colf,chr(min div 10+ord('0'))+chr(min mod 10+ord('0'))+':'+
                       chr(sec div 10+ord('0'))+chr(sec mod 10+ord('0'))+':'+
                      chr(cent div 10+ord('0'))+chr(cent mod 10+ord('0')));
end;

(***************************************************************************)


const                     (*JoyStick Info Variables*)
  stickmax:integer=5000;
  joystick_found : boolean=FALSE;
  game_running   : boolean=FALSE;

var
  button,stick_x,stick_mx,stick_y,stick_my:integer;


(*JoyStick Thread. Checks Periodically Joystick-Information*)

Procedure GetStickXY(param:ulong); (*Time_Crit*)
var x,y,joyloop : word;
    p : byte;

begin
  repeat
    x:=0;
    y:=0;
    Port[$201]:=0;
    for joyloop:=1 to stickmax do
    begin
      p:=Port[$201];
      Inc(x,p and 1);
      Inc(y,p shr 1 and 1);
    end;
    stick_x:=x;
    stick_y:=y;
    Button:=(not (p shr 4)) and 3;

    if stickmax=5000 then begin (*calibrate*)
      stick_mx:=x;
      stick_my:=y;
      stickmax:=x*2;
      if y*2>stickmax then
        stickmax:=y*2;
      if (stickmax=5000) or (stickmax=0) then
        exit;
      joystick_found:=TRUE;
    end;


    DosSleep(55);
  until ulToEnd<>0;
end;


(***************************************************************************)
(*Provides Timed EventSem, and counts Timer variable*)

Procedure TimerThread(param:ulong); (*Should run at Time_Crit or foreground*)
var rc:longint;
begin
  Timer:=0;
  while ulToEnd=0 do begin
    rc:=DosPostEventSem(TimerEvent);
    if rc=6 then exit;
    DosSleep(55);
    if not PauseDisplay then
      Inc(Timer);
  end;
end;

(***************************************************************************)
(*Blitter Thread: Does 3D Calculations and Blits Picture Buffer to Screen*)

Procedure BlitterThread(param1:ulong); (*Should run at idle + 10 (to avoid pulse)*)
var
  pwinData:P_WINDATA;       (* pointer to window data                 *)
  ulFirstTime:ULONG;

  ulScanLineBytes:ULONG;
  ulScanLines:ULONG;
  mousepos:pointl;
  StartTimer:Longint;
  i,j:Longint;

  rc,postcount:Longint;

begin
  pwinData:=P_WINDATA(param1);
  ulFirstTime:=0;
  frames:=0;

  while not game_running do
    DosSleep(50);

  StartTimer:=Timer;
  While game_running and (ulToEnd=0) do begin
    repeat
      rc:=DosWaitEventSem(BlitEvent,500);  (*Synchronize with Game-Control Updates*)

      if rc=6 then exit;
      DosResetEventSem(BlitEvent,postcount);
    until (rc<>640) or (not game_running) or (ulToEnd<>0); (*640 = Timeout Error*)
    if ( pwinData^.fChgSrcPalette ) or ( ulFirstTime=0 ) then begin
       if coltranslate then
         DiveSetSourcePalette ( pwinData^.hDive, 0,
                                pwinData^.ulnumColor,
                                pPalette );
       ulFirstTime:=$FFFF;
    end;


    if DiveBeginImageBufferAccess ( pwinData^.hDive,
                                    ulImage,
                                    Pointer(DispBuffer),
                                    ulScanLineBytes,
                                    ulScanLines )<>0 then Exit;

    DosRequestMutexSem(SimDataAccess,1000);

    Display(xv,yv,zv,yw,cars);

    DosReleaseMutexSem(SimDataAccess);


    WinQueryPointerPos(HWND_DESKTOP,mousepos);

    If not parms.Test then begin
      OutStr(10,10,White,'LAP:');
      OutStr(10+ 5*8,10,Red,chr(Laps div 10 + ord('0'))+chr(laps mod 10+ord('0')));

      OutStr(10+10*8,10,White,'TIME:');
      ShowTime(10+16*8,10,GameTime,Red);

      if BestTime<>999999 then begin
        OutStr(10+26*8,10,White,'BEST:');
        ShowTime(10+32*8,10,BestTime,Red);
      end;

      if (speed<=maxspeed) and (speed>0) then
        for i:=2 to 8 do begin
           FillChar(DispBuffer^[i*xpels],maxspeed div 2,Blue);
           FillChar(DispBuffer^[i*xpels],speed div 2,LightGreen);
        end;

    end;

    if joystick_found then begin
      mousepos.y:=range( (((stick_my-stick_y)*(ymax div 2)) div stick_my)+
                         (ymax div 2),0,ymax);
      mousepos.x:=range( (((stick_x-stick_mx)*(xmax div 2)) div stick_mx)+
                         (xmax div 2),0,xmax);
    end
    else begin
      mousepos.x:=(mousepos.x*(xmax+1)) div screenx;
      mousepos.y:=(mousepos.y*(ymax+1)) div screeny;
    end;

    for i:=range(mousepos.x-10,0,xmax) to range(mousepos.x+10,0,xmax) do
      DispBuffer^[(ymax-range(mousepos.y,0,ymax))*(xmax+1)+i]:=Red;
    for i:=range(mousepos.y-10,0,ymax) to range(mousepos.y+10,0,ymax) do
      DispBuffer^[(ymax-i)*(xmax+1)+range(mousepos.x,0,xmax)]:=Red;

    If parms.Wave then
      ShowWave(DispBuffer^,Red,xmax,ymax);

    (*color bar*)
{    for i:=16 to 32 do
      for j:=0 to 4 do
        FillChar(DispBuffer^[(xmax+1)*(((I-16)*5)+j)],50,i);}

    DispBuffer^[10*(xmax+1)+currentblock]:=White;


    DiveEndImageBufferAccess ( pwinData^.hDive, ulImage );


    DiveBlitImage ( pwinData^.hDive,
                    ulImage,
                    DIVE_BUFFER_SCREEN );

    inc(frames);
                        (*Update Statistics when neccessary*)
    if Timer-StartTimer>=40 then begin
      SetMainWindowTitle(StrReal((frames*18)/(Timer-StartTimer),2,6)+' Fps');
      frames:=0;
      StartTimer:=Timer;
    end;
  end;
end;

(***************************************************************************)
(*Simulation Thread for Watching Computer-Controlled Cars*)

Procedure Tester(parm1:Ulong); (*Regular prio*)
var
  c:char;
  k,i:integer;
  savetime,nowtime,lasttime:longint;
  sp,d:integer;
  first:boolean;
  lastyw:integer;
  postcount:integer;
  rc:longint;
  mousepos:pointl;

  vol:integer;

begin

  with cars do
  begin
    carnum:=7; (*7*)
    for i:=1 to carnum do begin
      k:=i;
      with carinfo[k] do
      begin
        cx:=4750-150*k;
        (*cx:=3700+150*k;*)
        if odd(i) then
          cz:=250
        else
          cz:=350;
        cyw:=90;
        carsp:=0;
        col1:=carcol[k];
      end;
      InitVoice(i+3,MainWav,5,TRUE);
    end;
  end;

  with wo do
  begin
    d:=0;
    sp:=0;


    xv:=5500;
    yv:=50;
    zv:=200;
    yw:=270;
    frames:=0;calcs:=0;
    Timer:=0;
    TimeStart:=Timer;
    c:=#0;
    game_running:=TRUE;

    rc:=DosResetEventSem(TimerEvent,postcount);
    postcount:=0;

    InitVoice(0,MainWav,1,TRUE);
    SetVoiceVol(0,100,100);
    SetVoiceRate(0,15000);
(*    PlayVoice(0);*)

    If parms.Sound then
      StartPlayBack;

    While ulToEnd=0 do begin

      Dec(PostCount);
      if PostCount<=0 then begin (*Wait only when all posts are worked out*)
        rc:=DosWaitEventSem(TimerEvent,200);
        if rc=6 then exit;
        DosResetEventSem(TimerEvent,postcount);
      end;

      DosRequestMutexSem(SimDataAccess,1000);

      inc(calcs);
      AutoCar;

      if joystick_found then
      begin
        sp:=(((stick_my-stick_y)*23) div stick_my)*3;
        d:=((stick_x-stick_mx)*10) div stick_mx;
        inc(xv,pisin(sp,yw));
        inc(zv,picos(sp,yw));
        yw:=to360(yw+d);
      end
      else begin
        WinQueryPointerPos(HWND_DESKTOP,mousepos);

        sp:=(mousepos.y*150) div screeny - 75;

        d:=(mousepos.x*32) div screenx - 16;

        yw:=to360(yw+d);
        inc(xv,pisin(sp,yw));
        inc(zv,picos(sp,yw));
      end;

      px:=xv;
      pz:=zv;
      DosReleaseMutexSem(SimDataAccess);


      DosPostEventSem(BlitEvent); (*Blitting would be useful now*)

      If PauseDisplay then begin
        While PauseDisplay Do
          DosSleep(100);
        DosResetEventSem(TimerEvent,Postcount);
        PostCount:=1;
      end;
    end;
    game_running:=FALSE;
    TimeStop:=Timer;
  end;
end;

(***************************************************************************)
(*Car-Steering*)

Procedure Steering;
var
  sx,sy,dirmax:integer;
  mousepos:pointl;


begin
  dirmax:=dirincmax div 4 +
          ((dirincmax - (dirincmax div 4))*(maxspeed-speed)) div maxspeed;

  if joystick_found then
  begin
    dirinc:=((stick_x-stick_mx)*dirmax) div stick_mx;

    if abs(stick_y-stick_my) > stick_my shr 2 then
      if stick_y-stick_my<0 then
        sp:=spincmax-(spincmax*speed) div (maxspeed+1)
      else
        sp:=-spincmax*2
    else sp:=-((spincmax*speed) div (maxspeed+1) div 2);
  end
  else begin
    WinQueryPointerPos(HWND_DESKTOP,mousepos);

    dirinc:=((mousepos.x-(screenx shr 1))*dirmax) div (screenx shr 1);
    if abs((screeny shr 1)-mousepos.y)>(screeny shr 4) then
      if (screeny shr 1)-mousepos.y<0 then
        sp:=spincmax-(spincmax*speed) div (maxspeed+1)
      else
        sp:=-spincmax*2
    else sp:=-((spincmax*speed) div (maxspeed+1) div 2);
  end;

  CarControl;
end;

(****************************************************************************)
(*Race Game Sim Thread*)

procedure RaceGame;
const
  cardx=230;
  cardy=115;
  speedx=10;
  speedy=120;
  speedw=100;
  dirx=10;
  diry=130;
  dirw=100;

  finx=4080;
  finz=300;
  checkx=3850;
  checkz=6500;

var
  c:char;
  NowTime,SaveTime,StartTime:longint;
  checked,dummy:boolean;
  dirold,i,subcnt : integer;

  rc,postcount:integer;


(*  Procedure ShowCarpos(on:boolean);
  var i,x,z:longint;
  begin
    with cars do
    begin
      for i:=0 to carnum do
        with carinfo[i] do
        begin
          x:=cx;
          z:=cz;

          if on then
            if i<>0 then
              SetPixel(cardx+81- x div 100,cardy+ z div 100,col1)
            else
              SetPixel(cardx+81- x div 100,cardy+ z div 100,15)
          else
            if groundok(x div 100,z div 100) then
              SetPixel(cardx+81- x div 100,cardy+ z div 100,0)
            else
              SetPixel(cardx+81- x div 100,cardy+ z div 100,5);
        end;
    end;
  end;*)

  Procedure CheckPos;
  var NowTime:Longint;
  begin
    NowTime:=Timer;
    GameTime:=NowTime-StartTime;
    if not checked then
      if(abs(checkx-px)+abs(checkz-pz))<1000 then checked:=TRUE;
    if checked then
      if (px>=finx) and (px<finx+300) and (pz<600) then
      begin
        inc(laps);


        if NowTime-StartTime<BestTime then
          BestTime:=NowTime-StartTime;
        StartTime:=NowTime;
        Checked:=False;
      end;
  end;

  Procedure SetCarinfo;
  begin
    with cars.carinfo[0] do
    begin
      cx:=px;
      cz:=pz;
      cyw:=y_angle;
      carsp:=speed;
    end;
  end;


begin
  px:=4000;
  py:=20;
  pz:=300;
  y_angle:=90;
  with cars do
  begin
    carnum:=6;
    for i:=1 to 6 do
      with carinfo[i] do
      begin
        cx:=3700+150*i;
        if odd(i) then cz:=250 else cz:=350;
        cyw:=90;
        carsp:=0;
        col1:=carcol[i];
        lastdist:=abs(cx-px)+abs(cz-pz);

        InitVoice(i+3,MainWav,5,TRUE);
      end;
  end;
  dx:=0;dz:=0;
  godir:=y_angle;
  Speed:=0;
  xrest:=0;
  zrest:=0;
  dirinc:=0;
  keydirinc:=0;
  lastdirinc:=0;
  preskid:=0;
  skid:=false;
  dirold:=0;
  lastskidded:=false;

  c:=#0;
  Frames:=0;
  calcs:=0;

  PlayVoice(0);

  Checked:=false;


  If parms.Sound then
    StartPlayBack;

  SaveTime:=Timer;
  Timer:=0;
  StartTime:=Timer;
  BestTime:=999999;
  GameTime:=0;
  laps:=0;
  TimeStart:=Timer;

  rc:=DosResetEventSem(TimerEvent,postcount);
  postcount:=0;

  game_running:=TRUE;

  While ulToEnd=0 do begin
    Inc(SoundWatchDog);
    Dec(PostCount);
    if PostCount<=0 then begin (*Wait only when all posts are worked out*)
      rc:=DosWaitEventSem(TimerEvent,200);
      if rc=6 then exit;
      DosResetEventSem(TimerEvent,postcount);
    end;

    DosRequestMutexSem(SimDataAccess,1000);


    dirold:=dirinc;
    Steering;

    AutoCar;

    px:=range(px+dx,0,8100);
    pz:=range(pz+dz,0,8100);
    Setcarinfo;

    xv:=px;
    yv:=py;
    zv:=pz;
    yw:=y_angle;

                     (*BANG at Collision*)
    if CheckCol(cars,px,pz,speed,y_angle,0) then
      PlayVoice(2);

    CheckPos;
    inc(calcs);

    DosReleaseMutexSem(SimDataAccess);


    DosPostEventSem(BlitEvent); (*Blitting would be useful now*)

    If PauseDisplay then begin
      While PauseDisplay Do
        DosSleep(100);
      DosResetEventSem(TimerEvent,Postcount);
      PostCount:=1;
    end;
  end;
  game_running:=FALSE;
  TimeStop:=Timer;
end;


(****************************************************************************)
(*Sets Up the Palette Array*)

Procedure SetPaletteArray(var pPalette:Pointer);
var
  i:longint;
  rgbxpal:p_rgbxpal8; (*Pointer to Palette*)

begin
  GetMem(pPalette,256*sizeof(ULONG));

  rgbxpal:=pPalette;
  FillChar(rgbxpal^,sizeof(rgbxpal^),#0);

  for i:=0 to 31 do  (*initialize Palette*)
    with rgbxpal^[i] do begin   (*Basic Colors*)
      b:=((i and 1)*8+(i and 8))*15;
      g:=((i and 2)*4+(i and 8))*15;
      r:=((i and 4)*2+(i and 8))*15;
      if i>15 then
        x:=PC_RESERVED
      else
        x:=0;
    end;

  for i:=0 to 49 do    (*Sky*)
    with rgbxpal^[i+100] do begin
      r:=((i*200) div 64);
      g:=(200+(i*40) div 64);
      b:=(200+(i*40) div 64);
      x:=PC_RESERVED;
    end;
  for i:=0 to 49 do    (*Ground*)
    with rgbxpal^[199-i] do begin
      r:=((i*40) div 64)    shl 2;
      g:=(40+(i*20) div 64) shl 2;
      b:=((i*40) div 64)    shl 2;
      x:=PC_RESERVED;
    end;
end;


(****************************************************************************)
(*Initializes Buffer allocated by DIVE APIs*)

Function AllocImgBuffer (var win_Data:WINDATA ):Ulong;
var
   ulNumBytes:ULONG;            (* output for number of bytes actually read *)


   pbBuffer:Pointer;            (* pointer to the image/ temp. palette data *)

   ulScanLineBytes:ULONG;       (* output for number of bytes a scan line   *)
   ulScanLines:ULONG;           (* output for height of the buffer          *)

   i,j:Longint;

   pbTmpDst:Pointer;            (* temporaly destination pointer            *)
   ulAllocX:Ulong;

const
  winx=xmax+1;
  winy=ymax+1;
  ColorBits=8; (*Allowed: 8,16,24*)

begin
  (* Set how many color bitmap data is supporting
  *)
  win_Data.ulnumColor:=1 shl ColorBits;

  (* Set bitmap width and height in pels.
  *)
  win_Data.ulWidth  := Winx;
  win_Data.ulHeight := Winy;

  (* Calculate source line size.  It should be double word boundary.
  *)
  win_Data.ulSrcLineSizeBytes := (((WinX* (ColorBits shr 3))+3) div 4)*4;

  (* Adjust the width in pixels for memory to allocate.
    *)
  ulAllocX := Winx*(ColorBits shr 3);
  while ulAllocX < win_Data.ulSrcLineSizeBytes do
    Inc(ulAllocX,ColorBits shr 3);

  (* Set bitmap coclor format.
   *)
  case ColorBits of
    8: win_Data.FccColorFormat:=FOURCC_LUT8;
   16: win_Data.FccColorFormat:=FOURCC_R565;
   24: win_Data.FccColorFormat:=FOURCC_BGR4;
  end;

   (* Allocate a buffer for image data
   *)

   if DiveAllocImageBuffer ( win_Data.hDive,
                             ulImage,
                             win_Data.fccColorFormat,
                             ulAllocX,
                             win_Data.ulHeight,
                             win_Data.ulSrcLineSizeBytes,
                             nil )<>0 then begin
      AllocImgBuffer:=1;
      exit;
   end;

   if DiveBeginImageBufferAccess ( win_Data.hDive,
                                     ulImage,
                                     pbBuffer,
                                     ulScanLineBytes,
                                     ulScanLines )<>0 then begin
      DiveFreeImageBuffer ( win_Data.hDive, ulImage );
      AllocImgBuffer:=1;
      exit;
   end;

   (* Init image Data-Buffer to 0 *)
   Fillchar(pbBuffer^,win_Data.ulWidth*win_Data.ulHeight,0);

   (* release the access to the image buffer *)

   DiveEndImageBufferAccess ( win_Data.hDive, ulImage );

   AllocImgBuffer:=0;
end;

(***************************************************************************
 * GetSnapHeight and -Width calculate the Height & Width of a Std-Window
 * with Menu for a target size of the client area
 ****************************************************************************)

Function GetSnapHeight(ulHeight:ULONG):ULONG;
begin
  Inc(ulHeight,
           WinQuerySysValue ( HWND_DESKTOP, SV_CYSIZEBORDER ) * 2);
  Inc(ulHeight,
           WinQuerySysValue ( HWND_DESKTOP, SV_CYBORDER ) * 2);
  Inc(ulHeight,
           WinQuerySysValue ( HWND_DESKTOP, SV_CYMENU ));
  Inc(ulHeight,
           WinQuerySysValue ( HWND_DESKTOP, SV_CYTITLEBAR ));
  GetSnapHeight:=ulHeight;
end;

Function GetSnapWidth(ulWidth:ULONG):ULONG;
begin
  Inc(ulWidth,
           WinQuerySysValue ( HWND_DESKTOP, SV_CXSIZEBORDER ) * 2);
  GetSnapWidth:=ulWidth;
end;


(*****************************************************************************)
(*Window Procedure of Startup Dialogue *)

function StartupDlgProc(Window: HWnd; Msg: ULong; Mp1,Mp2: MParam): MResult;cdecl;
var
  swpos       :SWP;                  (* Window position                      *)
begin
  case msg of
    WM_INITDLG:
      begin
        GetParams;

        WinSendDlgItemMsg(Window,ID_SOUND,BM_SETCHECK,longint(parms.Sound),0);
        If parms.Joystick then
          WinSendDlgItemMsg(Window,ID_JOY,BM_SETCHECK,1,0)
        else
          WinSendDlgItemMsg(Window,ID_MOUSE,BM_SETCHECK,1,0);
        WinSendDlgItemMsg(Window,ID_TEST,BM_SETCHECK,longint(parms.Test),0);
        WinSendDlgItemMsg(Window,ID_WAVE,BM_SETCHECK,longint(parms.Wave),0);
        WinSendDlgItemMsg(Window,ID_TRANS,BM_SETCHECK,longint(not parms.NoTrans),0);
        WinSendDlgItemMsg(Window,ID_FOCUS,BM_SETCHECK,longint(parms.PauseNoFocus),0);

        WinQueryWindowPos (Window , swpos );

        WinSetWindowPos ( Window, HWND_TOP,
                          (screenx div 2) - (swpos.cx div 2),
                          (screeny div 2) - (swpos.cy div 2),
                                    0,
                                    0,
                                    SWP_MOVE or SWP_ACTIVATE or SWP_SHOW );


      end; (*SetSysMenu(Window);*)
    WM_COMMAND:
      begin
        case USHORT(mp1) of
          DID_OK:
            begin
              parms.Sound:=Boolean(WinSendDlgItemMsg(Window,ID_SOUND,BM_QUERYCHECK,0,0));
              parms.Joystick:=Boolean(WinSendDlgItemMsg(Window,ID_JOY,BM_QUERYCHECK,0,0));
              parms.Test:=Boolean(WinSendDlgItemMsg(Window,ID_TEST,BM_QUERYCHECK,0,0));
              parms.Wave:=Boolean(WinSendDlgItemMsg(Window,ID_WAVE,BM_QUERYCHECK,0,0));
              parms.NoTrans:= not Boolean(WinSendDlgItemMsg(Window,ID_TRANS,BM_QUERYCHECK,0,0));
              parms.PauseNoFocus:=Boolean(WinSendDlgItemMsg(Window,ID_FOCUS,BM_QUERYCHECK,0,0));
              WriteParams;
              WinDismissDlg(Window, 1);
            end;
          DID_CANCEL:
              WinDismissDlg(Window, 0);
          else
            WinDismissDlg(Window, 0);
        end;
      end;
    else begin
      (* Let PM handle this message.
      *)
      StartupDlgProc:=WinDefDlgProc ( Window, msg, mp1, mp2 );
      exit;
    end;
  end;
  StartupDlgProc:=0;
end;

(*****************************************************************************)
(*Window Procedure of Dive-Window *)


function MyWindowProc(Window: HWnd; Msg: ULong; Mp1,Mp2: MParam): MResult;cdecl;
var
  point_l     :POINTL;               (* Point to offset from Desktop         *)
  swpos       :SWP;                  (* Window position                      *)
  h_rgn       :HRGN;                 (* Region handle                        *)
  h_ps        :HPS;                  (* Presentation Space handle            *)
  h_psPal     :HPS;                  (* Presentation Space handle            *)
  rcls        :array[0..49] of RECTL;(* Rectangle coordinates                *)
  rgnCtl      :RGNRECT;              (* Processing control structure         *)
  pwin_data    :P_WINDATA;            (* Pointer to window data               *)
  SetupBlitter:SETUP_BLITTER;        (* structure for DiveSetupBlitter       *)
  pPal        :PLONG;

  NoRectl     :PRECTL;
  PaletteColors:ULONG;

  sizl:SIZEL;

  rc:Ulong;

begin
  NoRectl:=Nil;
  pwin_data := P_WINDATA(WinQueryWindowULong (Window, 0));
  if ( pwin_data<>nil) or (msg=WM_CREATE) then begin
    case msg of
      WM_CREATE:
        begin
          (*Set Up Physical Palette*)
          sizl.cx:=0;
          sizl.cy:=0;
          h_dc:=WinOpenWindowDC(Window);
          h_psPal:=GpiCreatePS(h_ab,h_dc,sizl,
                            PU_PELS or GPIF_DEFAULT or GPIT_MICRO or GPIA_ASSOC);

          h_pal:=GpiCreatePalette(h_ab,LCOL_PURECOLOR (*or LCOL_OVERRIDE_DEFAULT_COLORS*), LCOLF_CONSECRGB,
                                  200,
                                  ULONG(pPalette^));
          rc:=GpiSelectPalette(h_psPal,h_pal);

          WinRealizePalette(Window,h_psPal,PaletteColors);
        end;
      WM_COMMAND:
        case USHORT(mp1) of
          ID_SNAP:
            begin  (*Snap Window to Dive Buffer Size*)
              WinSetWindowPos ( pwin_data^.hwndFrame, HWND_TOP,
                                    100, 100,
                                    GetSnapWidth(pwin_data^.ulWidth),
                                    GetSnapHeight(pwin_data^.ulHeight),
                                    SWP_SIZE or SWP_ACTIVATE or SWP_SHOW );
            end;
          ID_SNAP2:
            begin (*Snap Window to Size & height * 2*)
              WinSetWindowPos ( pwin_data^.hwndFrame, HWND_TOP,
                                    100, 100,
                                    GetSnapWidth(pwin_data^.ulWidth*2),
                                    GetSnapHeight(pwin_data^.ulHeight*2),
                                    SWP_SIZE or SWP_ACTIVATE or SWP_SHOW );
            end;
          ID_SNAPFULL:
            begin
              (*Snap to Full Size but retain (approx) aspect ratio*)

              WinSetWindowPos ( pwin_data^.hwndFrame, HWND_TOP,
                            0, screeny div 2-
                            ((pwin_data^.ulHeight*screenx) div pwin_data^.ulWidth) div 2,
                            screenx,
                            (pwin_data^.ulHeight*screenx) div pwin_data^.ulWidth,
                            SWP_MOVE or SWP_SIZE or SWP_ACTIVATE or SWP_SHOW );
            end;
          ID_EXIT:
            (* Post to quit the dispatch message loop.
             *)
            WinPostMsg ( Window, WM_QUIT, 0, 0 );
          ID_PAUSE:
            begin
              PauseDisplay:=not PauseDisplay;
              If PauseDisplay then begin
                SetMainWindowTitle('Game PAUSED');
                PausePlayBack;
              end
              else begin
                SetMainWindowTitle('Game resumed');
                ResumePlayBack;
              end;
            end;
          ID_NEWTEXT:
            (* Write new text string to the title bar
             *)
            WinSetWindowText ( pwin_data^.hwndFrame, PCHAR(mp2));
          else
            (* Let PM handle this message.
             *)
            MyWindowProc:=WinDefWindowProc ( Window, msg, mp1, mp2 );
            exit;
        end;
      WM_VRNDISABLED:
        DiveSetupBlitter ( pwin_data^.hDive, nil );
      WM_VRNENABLED:
        begin
          h_ps := WinGetPS ( Window );
          if h_ps<>0 then begin
            h_rgn := GpiCreateRegion ( h_ps, 0, NoRectl^ );
            if h_rgn<>0 then begin
              (* NOTE: If mp1 is zero, then this was just a move message.
              ** Illustrate the visible region on a WM_VRNENABLE.
              *)
              (*WinQueryUpdateRegion  (Window,h_rgn);*)
              WinQueryVisibleRegion ( Window, h_rgn );
              rgnCtl.ircStart     := 0;
              rgnCtl.crc          := 50;
              rgnCtl.ulDirection  := 1;

              (* Get the all ORed rectangles
              *)
              if GpiQueryRegionRects ( h_ps, h_rgn, NoRectl^, rgnCtl, rcls[0]) then
              begin
                (* Now find the window position and size, relative to parent.
                *)
                WinQueryWindowPos ( pwin_data^.hwndClient, swpos );

                (* Convert the point to offset from desktop lower left.
                *)
                point_l.x := swpos.x;
                point_l.y := swpos.y;
                WinMapWindowPoints ( pwin_data^.hwndFrame,
                                     HWND_DESKTOP, point_l, 1 );

                (* Tell DIVE about the new settings.
                *)
                SetupBlitter.ulStructLen := sizeof ( SETUP_BLITTER );
                SetupBlitter.fccSrcColorFormat := pwin_data^.fccColorFormat;
                SetupBlitter.ulSrcWidth := pwin_data^.ulWidth;
                SetupBlitter.ulSrcHeight := pwin_data^.ulHeight(* shr 1*);
                SetupBlitter.ulSrcPosX := 0;
                SetupBlitter.ulSrcPosY := 0;
                SetupBlitter.fInvert := FALSE (*TRUE*);
                SetupBlitter.ulDitherType := 0;

                if coltranslate then
                  SetupBlitter.fccDstColorFormat := FOURCC_SCRN
                else
                  SetupBlitter.fccDstColorFormat := FOURCC_LUT8;

                SetupBlitter.ulDstWidth := swpos.cx;
                SetupBlitter.ulDstHeight := swpos.cy(*shr 1*);

                OutputWinSize:=' x:'+StrInt(SwPos.cx)+' y:'+Strint(SwPos.cy);

                SetupBlitter.lDstPosX := 0;
                SetupBlitter.lDstPosY := 0;
                SetupBlitter.lScreenPosX := point_l.x;
                SetupBlitter.lScreenPosY := point_l.y;
                SetupBlitter.ulNumDstRects := rgnCtl.crcReturned;
                SetupBlitter.pVisDstRects := @rcls;
                rc:=DiveSetupBlitter ( pwin_data^.hDive, @SetupBlitter );
              end
              else
                DiveSetupBlitter ( pwin_data^.hDive, nil );

              GpiDestroyRegion( h_ps, h_rgn );
            end;
           (* WinReleasePS( h_ps );*)
          end;
        end;
      WM_CHAR:
        begin
          (* Character input: first two byte of message is the character code.
          *)
          if (CharMsgMp1(Mp1).fs and kc_KeyUp) = 0 then { Key is Down }
            if USHORT(mp2 shr 16) = VK_F3 then
              WinPostMsg ( Window, WM_QUIT, 0, 0 )
            else
              if USHORT(mp2 shr 16) = VK_F1 then
                 WinPostMsg ( Window, WM_COMMAND, ID_SNAP, 0 )
              else
                if USHORT(mp2 shr 16) = VK_F2 then
                   WinPostMsg ( Window, WM_COMMAND, ID_SNAP2, 0 )
                else
                  if USHORT(mp2 shr 16) = VK_F4 then
                     WinPostMsg ( Window, WM_COMMAND, ID_SNAPFULL, 0 )
                  else
                    if USHORT(mp2 shr 16) = VK_F5 then
                       WinPostMsg ( Window, WM_COMMAND, ID_PAUSE, 0 )
                    else
                      if USHORT(mp2 shr 16) = VK_F10 then
                         WinPostMsg ( Window, WM_COMMAND, ID_START, 0 );
        end;
      WM_FOCUSCHANGE:
        if parms.PauseNoFocus then
          if(mp2 and 1)=1 then begin
            PauseDisplay:=FALSE;
            ResumePlayback;
            SetMainWindowTitle('Game resumed');
          end
          else begin
            SetMainWindowTitle('Game PAUSED ');
            PauseDisplay:=TRUE;
            PausePlayback; (*Pause Playback if window loses focus*)
          end;
      WM_REALIZEPALETTE:
        begin
          h_ps := WinBeginPaint ( pwin_data^.hwndFrame, 0 , Nil);

          GetMem(pPal,sizeof(LONG) * pwin_data^.ulnumColor);

          GpiQueryRealColors ( h_ps, 0, 0, pwin_data^.ulnumColor, pPal^ );

          if coltranslate then
            DiveSetDestinationPalette ( pwin_data^.hDive, 0,
                                        pwin_data^.ulnumColor,
                                        PBYTE(pPal));
          FreeMem(pPal,sizeof(LONG) * pwin_data^.ulnumColor);

          WinEndPaint ( h_ps );
         end;
      WM_CLOSE:
      begin
        (* Post to quit the dispatch message loop.
        *)
        GpiSelectPalette(h_psPal,0);
        GpiDeletePalette(h_pal);
        GpiDestroyPS(h_psPal);

        WinPostMsg ( Window, WM_QUIT, 0, 0 );
      end;
      else
        begin
          (* Let PM handle this message.
          *)
          MyWindowProc:=WinDefWindowProc ( Window, msg, mp1, mp2 );
          exit;
        end;
    end;
  end
  else begin
    (* Let PM handle this message.
    *)
    MyWindowProc:=WinDefWindowProc ( Window, msg, mp1, mp2 );
    exit;
  end;
  MyWindowProc:=0;
end;




(****************************************************************************)
(* Main Program *)

var
  tidBlitThread:TID;         (*Thread ID for Blitter Thread*)
  tidGame:TID;               (*Thread ID for Game-Control Thread*)
  tidTimer:TID;              (*Thread ID for Timer Thread*)
  tidStickXy:TID;            (*Thread ID for Joystick Thread*)

  h_mq:HMQ;                  (* Message queue handle                 *)
  q_msg:QMSG;                (* Message from message queue           *)

  h_ps:HPS;   (*Paint Section ??*)

  flCreate:ULONG;           (* Window creation control flags        *)
  i:ULONG;                  (* Index for Buffers to init            *)
  pPal:PLONG;               (* Pointer to system physical palette   *)

  rc:ULONG;

  NullPtr:Pointer;

  WinPosx,WinPosy:integer;

  hwndDlg:HWND;

begin
  (* Initialize the presentation manager, and create a message queue.
  *)
  h_ab := WinInitialize ( 0 );
  h_mq := WinCreateMsgQueue ( h_ab, 0 );

  InitDreiD;

  InitArcSin;

(*  Parm_JoyStick:=FindParam('JOY') or FindParam('JOYSTICK');
  Parm_Sound:=FindParam('SOUND');
  Parm_Test:=FindParam('TEST');
  Parm_Up:=FindParam('UP');
  Parm_Wave:=FindParam('WAVE');
  Parm_NoTrans:=FindParam('NOTRANS');*)


(*Init Race Vars*)
  Fillchar(cars,sizeof(cars),#0);

  GetGround;
  linemode:=FALSE;



  SetPaletteArray(pPalette);

  (*Get Screensize*)
  Screenx:=WinQuerySysValue ( HWND_DESKTOP, SV_CXSCREEN);
  Screeny:=WinQuerySysValue ( HWND_DESKTOP, SV_CYSCREEN);

  (*Create Window + Class *)
  WinRegisterClass ( h_ab, pszMyWindow, MyWindowProc, 0, sizeof(ULONG) );

  flCreate:=FCF_TASKLIST or FCF_SYSMENU or FCF_TITLEBAR or
                  FCF_SIZEBORDER or FCF_MINMAX or FCF_MENU or FCF_SCREENALIGN(*or FCF_SHELLPOSITION*);

  win_Data.hwndFrame:=WinCreateStdWindow ( HWND_DESKTOP,
                                           WS_VISIBLE, flCreate,
                                           pszMyWindow,
                                           pszTitleText,
                                           WS_SYNCPAINT or WS_VISIBLE,
                                           0, ID_MAINWND,
                                           win_Data.hwndClient);

  if WinDlgBox( HWND_DESKTOP,
             win_Data.hwndFrame,
             StartupDlgProc,
             0,
             ID_STARTUPDLG,
             Nil)=0 then begin
    WinDestroyMsgQueue ( h_mq );
    WinTerminate ( h_ab );
    Halt(0);
  end;

  coltranslate:=not parms.NoTrans;

  NullPtr:=Nil;

  (* Get a linear address to the screen.
  *)
  rc:=DiveOpen ( win_Data.hDive, FALSE, NullPtr );
  if rc<>0 then begin
    WinMessageBox( HWND_DESKTOP, HWND_DESKTOP,
       'Dive Open Error!',
       'Error!', 0, MB_OK or MB_MOVEABLE );
    WinDestroyMsgQueue ( h_mq );
    WinTerminate ( h_ab );
    Halt(1);
  end;

  (*Start Joystick-Thread*)
  If parms.Joystick then begin
    DosCreateThread(tidStickXY,@GetStickXY,0,0,8192);

    DosSetPriority ( PRTYS_THREAD, PRTYC_TIMECRITICAL,
                        0, tidStickXY );
    DosSleep(500);
  end;



  (* Initialize Buffers
  *)
  if AllocImgBuffer (win_Data )<>0 then begin
     WinMessageBox( HWND_DESKTOP, HWND_DESKTOP,
                    'Failed to initialize Frame Buffers.',
                    'Error!', 0, MB_OK or MB_MOVEABLE );
     WinDestroyMsgQueue ( h_mq );
     WinTerminate ( h_ab );
     Halt(1);
  end;


  WinSetWindowULong (win_Data.hwndClient, 0, ULONG(@win_Data));


  WinPosX:=ScreenX div 2 - (GetSnapWidth(win_Data.ulWidth) div 2);
  WinPosY:=ScreenY div 2 - (GetSnapHeight(win_Data.ulHeight) div 2);
(*  If parms.Up then
    WinPosY:=400;*)

  (* Set the size of the window *)
  WinSetWindowPos ( win_data.hwndFrame, HWND_TOP,
                                    WinPosX, WinPosY,
                                    GetSnapWidth(win_Data.ulWidth),
                                    GetSnapHeight(win_Data.ulHeight),
                                    SWP_MOVE or SWP_SIZE or SWP_ACTIVATE or SWP_SHOW );


(*  WinPostMsg ( win_Data.hwndClient, WM_COMMAND, ID_SNAP, 0 );*)

  (* Turn on visible region notification.
  *)

  WinSetVisibleRegionNotify ( win_Data.hwndClient, TRUE );  (*!!!!*)


  (* set the flag for the first time simulation of palette of bitmap data
  *)
  win_Data.fChgSrcPalette := FALSE;

  (* Send an invalidation message to the client.
  *)
  WinPostMsg ( win_Data.hwndFrame, WM_VRNENABLED, 0, 0 );

  (*Start SoundEngine;*)
  SoundInit;
  if parms.sound then begin
    LoadWav('car.wav',MainWav);
    LoadWav('impact.wav',CrashWav);

    InitVoice(0,MainWav,1,TRUE); (*Players Engine*)
    SetVoiceVol(0,100,100);
    SetVoiceRate(0,15000);

    InitVoice(1,MainWav,1,TRUE); (*Skid*)
    SetVoiceVol(1,80,80);
    SetVoiceRate(1,100000);

    InitVoice(2,CrashWav,1,FALSE); (*Crash, no Looping*)
    SetVoiceVol(2,100,100);
    SetVoiceRate(2,3000);
  end;
  (*Start Timer-Thread + Create Semaphore*)

  DosCreateEventSem(PCHAR(0),TimerEvent,DC_SEM_SHARED,FALSE);

  DosCreateThread(tidTimer,@TimerThread,0,0,8192);

  DosSetPriority ( PRTYS_THREAD, PRTYC_TIMECRITICAL,
                      -1, tidTimer );

  (*Create Game-Blitter Synchronization Semaphore*)
  DosCreateEventSem(PCHAR(0),BlitEvent,DC_SEM_SHARED,FALSE);

  (*Create Game-Blitter Synchronization Semaphore*)
  DosCreateMutexSem(PCHAR(0),SimDataAccess,DC_SEM_SHARED,FALSE);

  (*Start Game-Control-Thread *)
  If parms.Test then
    DosCreateThread ( tidGame,@Tester,0, 0, 8192)
  else
    DosCreateThread ( tidGame,@RaceGame,0, 0, 8192);

  DosSetPriority ( PRTYS_THREAD, PRTYC_REGULAR,
                      0, tidGame );

  (*Start Blitter-Thread *)

  if DosCreateThread ( tidBlitThread,
                       @BlitterThread,
                       ULONG(@win_Data), 0, 16384)<>0 then begin
    WinSetVisibleRegionNotify ( win_Data.hwndClient, FALSE ); (*!!!!*)

    DiveFreeImageBuffer ( win_Data.hDive, ulImage );

    DiveClose ( win_Data.hDive );

    WinDestroyWindow ( win_Data.hwndFrame );
    WinDestroyMsgQueue ( h_mq );
    WinTerminate ( h_ab );
    Halt(1);
  end;

  (* Set the priority of the blitting thread
  *)
  DosSetPriority ( PRTYS_THREAD, (*PRTYC_REGULAR*) PRTYC_IDLETIME,
                      20, tidBlitThread );


  (* While there are still messages, dispatch them.
  *)
  while WinGetMsg ( h_ab, q_msg, 0, 0, 0 ) do
    WinDispatchMsg ( h_ab, q_msg );

  (* Set the variable to end the running thread, and wait for it to end.
  *)
  SoundDone;

  PauseDisplay:=FALSE;
  ulToEnd := 1;
  DosWaitThread ( tidBlitThread, DCWW_WAIT );

  (* Turn off visible region notificationm tidy up, and terminate.
  *)

  WinSetVisibleRegionNotify ( win_Data.hwndClient, FALSE ); (*!!!!*)

  (* Free the buffers allocated by DIVE and close DIVE
  *)
  DiveFreeImageBuffer ( win_Data.hDive, ulImage );

  DiveClose ( win_Data.hDive );

(*   for  ( i = 0; i < pwin_data^.ulMaxFiles; i++ )
       DosFreeMem ( pPalette[i] );*)

   (* Process for termination
   *)
  DosWaitThread ( tidTimer, DCWW_WAIT );
  DosCloseEventSem(TimerEvent);



  WinDestroyWindow ( win_Data.hwndFrame );
  WinDestroyMsgQueue ( h_mq );
  WinTerminate ( h_ab );
  Halt(0);
end.


