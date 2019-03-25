(*******************************************************)
(*               Voice-Mixer Unit                      *)
(*******************************************************)

unit sounddrv;

(************************************************************************)
interface

{$S-}

uses Os2Def, Os2Base, Os2PmApi, Use32;

const
  currentblock :integer=0;
  SoundWatchdog:longint=0; (*This count HAS to be increased by an external
                             process at least BLOCKS_PER_SEC times per second
                             to indicate an eventual sound-mixing overload.*)

  SoundPaused  :boolean=FALSE; (*Indicates Paused Sound, do not changed directly*)


Procedure SoundInit;

Function  LoadWav(FileName:String;var WavIdx:Integer):boolean;
Function  StartPlayBack:boolean;
Function  StopPlayBack:boolean;
Function  PausePlayback:boolean;
Function  ResumePlayback:boolean;

Procedure InitVoice(VoiceIdx,WavIDx,Prio:Integer;Loop:Boolean);
Procedure SetVoiceVol(VoiceIdx,LVolume,RVolume:Integer); (*Set Volume*)
Procedure SetVoiceRate(VoiceIdx,Rate:Integer); (*Playback Samples Per Sec*)
Function  VoiceIsPlaying(VoiceIdx:Integer):boolean;
Procedure PlayVoice(VoiceIdx:Integer);
Procedure StopVoice(VoiceIdx:Integer);

Procedure SetGlobalVolume(Volume:Integer);

Procedure SoundDone;

Procedure ShowWave(var videobuffer;col,xmax,ymax:integer);


(************************************************************************)
implementation

uses MciOS2;

(************************************************************************)
(* Needed Structures and Constants                                      *)
const
  MIX_RATE                =11025; (*Mixed & Playback Samples per Sec*)
  MIX_BITS                =8;     (*You have to change the Mix-Voices *)
                                  (*Function to enable other Formats  *)

  MIX_VOICES              =4;     (*Maximum Number of Voices actually mixed*)

  BLOCKS_PER_SEC          =10; (*Set Block-Length*)
  CUE_POS                 =50; (*Cuepoint Position in Buffer in % *)
  BLOCK_LENGTH            =MIX_RATE*(MIX_BITS div 8) div BLOCKS_PER_SEC;
  NUMBER_OF_BLOCKS        =2;  (*Data-Blocks in the Playlist*)
  PLAYLIST_COMMANDS       =NUMBER_OF_BLOCKS*3; (*SETCUEP,DATA_OP,BRANCH*)

  MAX_VOICES              =16; (*Max-Number of Processed (not mixed) voices*)
  MAX_WAV                 =16; (*Max-Number of loaded Wav-Files*)

(************************************************************************)

type
  (* SoundInfo holds information about a Wav *)
  SoundInfo = record
    sFileName       :String;
    Address         :^LONG;
    ulSize          :ULONG;
    ulSamplesPerSec :ULONG;
    usBitsPerSample :USHORT;
    ulLoopOfs       :ULONG; (*-1 for no Looping*)
  end;

  (* SoundDevice holds information about the Sound Device *)
  Sound_Device=record
    usSoundDeviceID:USHORT;
    ulSamplesPerSec:ULONG;
    usBitsPerSample:USHORT;
  end;


  (* A Playlist is a sequence of machine-code like instructions for mmpm *)
  (* In this case it alternatively sends notification-messages and plays *)
  (* parts of the wave-buffer                                            *)
  play_list_structure = record      (* playlist structure *)
    ulCommand:ULONG;
    ulOperandOne:ULONG;
    ulOperandTwo:ULONG;
    ulOperandThree:ULONG;
  end;

  (* 32Bit unsigned Fixed-Point Number *)
  fix = record case boolean of
          true:(fix:ULong);             (* Whole Number             *)
          false:(f:system.word;i:system.word);
                 (*fract       int*)
        end;

  (* Info about a certain Voice to be mixed by the Unit*)
  Voice_Info = record
    MyWave            : Integer; (* Associated Wave Idx      *)
    StartAddr         : Integer; (* Copied Wave information  *)
    Size              : Integer;
    origSamplesPerSec : Integer;
    LoopPos           : Integer;

    origHz            : Integer; (*Sample Frequency in Hertz (to come)*)

    Playing           : Boolean; (*Application currently wants to play Voice*)
    Mixed             : Boolean; (*This Voice is actually Playing*)
    Priority          : Integer; (*Voice Playback Priority*)

    VolumeLeft,
    VolumeRight       : Integer; (*Volumes*)

    nowSamplesPerSec  : Integer; (*Set Playback SamplesPerSeconds*)
    dx                : Fix;     (*Stepwidth for current SamplesPerSec*)
    SourceOfs         : Fix;     (*Current Offset in Source*)
  end;

(************************************************************************)
const
  hwndSound : HWND =0;   (* Window-Handle of Sound-Message Receiver*)
  LastWatchDog : LONGINT = 0;

var
  habSound  : HAB;       (* Anchor Block Handle                    *)
  tidSound  : TID;       (* Sound Thread-ID                        *)

  h_mq:HMQ;
  q_msg:qmsg;

  DriverOpen     : boolean;
  PlaybackActive : boolean;

  GlobalVolume : Integer;
                         (*Playback-Buffer, mixing goes here*)
  Buffer : array[0..BLOCK_LENGTH*NUMBER_OF_BLOCKS] of byte;
                         (*Info about Voices to Play*)
  Voice : array [0..MAX_VOICES-1] of Voice_Info;
                         (*Info about Wav-Files loaded*)
  WavFiles : array [0..MAX_WAV-1] of SoundInfo;

  playlist: array [0..PLAYLIST_COMMANDS] of play_list_structure;

  NumWavs     : integer;   (* # of loaded Wav-Files*)
  NumVoices   : integer;   (* # of currently enabled Voices*)
                           (* # of Entries in Playlist*)
  Playlist_Entries : Integer;
{  BlockLen         : Integer; (*Length of one Playback Data-Block*)}

                         (*MCI Info Structures*)
  SoundDevice: Sound_Device;
  mciOpenParameters:MCI_OPEN_PARMS;  (* Open structure.       *)
  mmAudio_Header:MMAUDIOHEADER;      (* Contains info for SET *)

  ulError : Ulong; (*Variable for error processing*)

  (*************************************************************************)
  (* Private Functions *)

  Procedure MixVoices(PrevBlock:Integer);forward;
  Function  SoundWndProc(Window: HWnd; Msg: ULong; Mp1,Mp2: MParam): MResult;cdecl;forward;
  Procedure MCIERROR;forward;
  Procedure InitPlaylist;forward;
  Procedure SoundThread(param1:Ulong);forward;
  Function  OpenSoundDevice:Boolean;forward;
  Procedure AdjustSoundDevice;forward;
  Procedure CloseSoundDevice;forward;

  (*************************************************************************)


(*********** Tool Functions**********)

Function Strint(x:longint):string;
var s:string[20];
begin
  Str(x,s);
  StrInt:=s;
end;

Procedure SetPSZLen(var s:string);
var i:integer;
begin
  i:=1;
  while (i<256) and (s[i]<>#0) do
    inc(i);
  if (i<256) then
    s[0]:=chr(i);
end;



(*****************************************************************************
 *                     Init SoundDriver Unit                                 *
 ****************************************************************************)


Procedure ShowWave(var videobuffer;col,xmax,ymax:integer);
var
  x:integer;
type
  buffarr = array[0..65000] of byte;

begin
  if xmax>BLOCK_LENGTH div 2 then xmax:=BLOCK_LENGTH div 2;
  for x:=0 to xmax-1 do
    buffarr(videobuffer)[x+xmax*(buffer[currentblock*BLOCK_LENGTH+(x*2)] div 2)]:=col;
end;


Procedure SoundInit;
var
  i:integer;
begin
  GlobalVolume:=100;
  NumWavs:=0;
  NumVoices:=0;
  PlayBackActive:=FALSE;
  DriverOpen:=FALSE;
  tidSound:=0;
  Fillchar(Buffer,SizeOf(Buffer),#0);

  InitPlaylist;

  FillChar(Voice,sizeof(Voice),#0);

  if hwndSound=0 then begin
    habSound := WinInitialize (0);
    h_mq := WinCreateMsgQueue(habSound, 0);
    WinRegisterClass(habSound,
                     'Sound Driver',
                     SoundWndProc,
                     CS_SAVEBITS or CS_MOVENOTIFY or CS_CLIPCHILDREN
                     or CS_CLIPSIBLINGS,
                     0);
    hwndSound := WinCreateWindow(HWND_OBJECT,
                                 'Sound Driver',
                                 '',0, 0, 0, 0, 0,
                                 HWND_OBJECT,
                                 HWND_BOTTOM,
                                 0, Nil, Nil);

    DosCreateThread ( tidSound,@SoundThread,0, 0, 16384);
    DosSetPriority ( PRTYS_THREAD, (*PRTYC_TIMECRITICAL*)PRTYC_FOREGROUNDSERVER,
                        0, tidSound );
  end;
  If OpenSoundDevice then begin
    DriverOpen:=TRUE;
  end;
end;

(*****************************************************************************
 *                     Close SoundDriver                                     *
 ****************************************************************************)


Procedure SoundDone;
begin
  If PlaybackActive then
    StopPlayBack;

  If DriverOpen then begin
  end;

  If hwndSound<>0 then
    WinPostMsg( hWndSound, WM_QUIT, 0, 0 );
end;

(*****************************************************************************
 *                     InitVoice, Initialize a Voice Structure               *
 ****************************************************************************)

Procedure InitVoice(VoiceIdx,WavIDx,Prio:Integer;Loop:Boolean);
begin
  If (WavIdx<NumWavs) and (VoiceIdx<Max_Voices) then
    With Voice[VoiceIdx] do begin
      Playing:=FALSE;
      Mixed:=FALSE;

      MyWave:=WavIdx;


      With WavFiles[MyWave] do begin
        StartAddr:=Ulong(Address);
        Size:=ulSize;
        origSamplesPerSec:=ulSamplesPerSec;
        If Loop then
          LoopPos:=0
        else
          LoopPos:=-1;
      end;

      Priority:=Prio; (*Currently Priority is simply = 1/VoiceIdx*)
                      (*Lowest idx = highest priority            *)
      VolumeLeft:=256;
      VolumeRight:=256;
      nowSamplesPerSec:=origSamplesPerSec;

      SourceOfs.Fix:=0;
      dx.f:=0;
      dx.i:=1;

    end;
end;

(*****************************************************************************
 *                     SetVoiceVol, Set Voice Volume                         *
 ****************************************************************************)
Procedure SetVoiceVol(VoiceIdx,LVolume,RVolume:Integer);
begin
  If (VoiceIdx<Max_Voices) then
    With Voice[VoiceIdx] do begin
      if lVolume>100 then
        lVolume:=100;
      if lVolume<0 then
        lVolume:=0;

      if RVolume>100 then
        RVolume:=100;
      if RVolume<0 then
        RVolume:=0;


      VolumeLeft:=(256*LVolume) div 100;
      VolumeRight:=(256*RVolume) div 100;
    end;
end;

(*****************************************************************************
 *                     SetVoiceRate, Set Voice Playback Rate                 *
 ****************************************************************************)
Procedure SetVoiceRate(VoiceIdx,Rate:Integer); (*Playback Samples Per Sec*)
var adjust:integer;
begin
  If (VoiceIdx<Max_Voices) then
    if Voice[VoiceIdx].origSamplesPerSec>0 then
      With Voice[VoiceIdx] do begin
        adjust:=0;
        if Rate<50 then
          Rate:=50;
                     (*adjust anything bigger 32755*)
        while (rate>=1 shl (adjust+15)) and (adjust<16) do
          inc(adjust);

        nowSamplesPerSec:=Rate;

        dx.i:=Rate shr adjust; (*should put a crit-sec here ;-)  *)
        dx.f:=0;

        dx.fix:=(dx.fix div origSamplesPerSec) shl adjust; (**)
      end;
end;

(*****************************************************************************
 *                     Return, whether Voice is currently playing or not     *
 ****************************************************************************)

Function VoiceIsPlaying(VoiceIdx:Integer):boolean;
begin
 If (VoiceIdx<Max_Voices) then
   VoiceIsPlaying:=Voice[VoiceIdx].Playing
 else
   VoiceIsPlaying:=FALSE;
end;

(*****************************************************************************
 *                     Start Playing Voice                                   *
 ****************************************************************************)

Procedure PlayVoice(VoiceIdx:Integer);
begin
  If (VoiceIdx<Max_Voices) then
    With Voice[VoiceIdx] do begin
      If not Voice[VoiceIdx].Playing then begin
        Playing:=TRUE;
        Inc(NumVoices);
      end;
      SourceOfs.fix:=0;
    end;
end;

(*****************************************************************************
 *                     Stop Playing Voice                                    *
 ****************************************************************************)

Procedure StopVoice(VoiceIdx:Integer);
begin
  If (VoiceIdx<Max_Voices) then
    With Voice[VoiceIdx] do
      If  Voice[VoiceIdx].Playing then begin
        Playing:=FALSE;
        Dec(NumVoices);
      end;
end;



(****************************************************************************)
Procedure SetGlobalVolume; (*not yet implemented*)
begin
end;


(****************************************************************************
 * LoadWav       Reads a Wavefile into Memory and Sets up the Info-Structure*
 ***************************************************************************)
Function LoadWav(FileName:String;var WavIdx:Integer):boolean;
var
  hmmioFileHandle:HMMIO;            (* Handle to the audio file handle.    *)
  ulRC:ULONG;                       (* Return code from mmioGetHeader.     *)
  ulBytesRead:ULONG;                (* Returned from mmioGetHeader.        *)
  lMMIOResults:LONG;                (* Results from the MMIO calls.        *)

  usSoundFileID:USHORT;
  Error:Boolean;
(*  StrName:PChar;*)

  i:integer;
begin
  WavIdx:=NumWavs;
  Inc(NumWavs);
  Error:=FALSE;
  With WavFiles[WavIdx] do begin
    sFileName:=FileName+#0;


(*    StrName:=@SoundFiles[usSoundFileID].sFileName[1];*)
    hmmioFileHandle:=mmioOpen(@SFileName[1],Nil,MMIO_READ or MMIO_DENYNONE );

    Error:=hmmioFileHandle=0;
    if not Error then begin
        (* Get the header information from the waveform file. *)
      ulRC:=mmioGetHeader (hmmioFileHandle,mmAudio_Header,sizeof (MMAUDIOHEADER),ulBytesRead,0,0);
      Error:=ulRc<>MMIO_SUCCESS;
      if not Error then begin  (*Assigning Infos from Audio file*)
        ulSize:=mmAudio_Header.mmXWAVHeader.XWAVHeaderInfo.ulAudioLengthInBytes;
        ulSamplesPerSec:=mmAudio_Header.mmXWAVHeader.WAVEHeader.ulSamplesPerSec;
        usBitsPerSample:=mmAudio_Header.mmXWAVHeader.WAVEHeader.usBitsPerSample;

        (* Get Mem-Buffer for Wav-File*)
        GetMem(Address,ulSize);

        FillChar(Address^,ulSize,#0);

          (* Move the data from the wave file into the buffer. *)
        lMMIOResults:=mmioRead(hmmioFileHandle,
                               char(Address^),
                               ulSize);

        Error:=lMMIOResults=MMIO_ERROR;

        if not Error then begin

          for i:=0 to ulSize-1 do  (*Prepare Wav for Mixing, shift it *)
            Mem[ulong(Address)+i]:=Mem[ulong(Address)+i] div MIX_VOICES;
        end                        (*Perhaps it's better (but slower),
                                     to shift it after mixing*)
        else
          FreeMem(Address,ulSize);

      end;
      lMMIOResults:=mmioClose(hmmioFileHandle,0 );
    end;

    If Error then begin
      LoadWav:=FALSE;
      WavIdx:=-1;
      Dec(NumWavs);
    end;

  end;
end;

(*****************************************************************************
 *                     InitPlaylist                                          *
 ****************************************************************************)

Procedure InitPlaylist;
var
  block           :integer;

begin
  FillChar(Playlist,sizeof(Playlist),#0);

(*  blocklen:=SizeOf(Buffer) div round(MIX_BUFFER_SECS*BLOCKS_PER_SEC);*)

(*  PlayList_Entries:=;*)

  for block:=0 to NUMBER_OF_BLOCKS-1 do begin
    With playlist[block*3] do begin
{      ulCommand:=MESSAGE_OPERATION;   (*Send Message to WindowProc*)

      ulOperandTwo:=block; (*signals next played data_block*)}
      ulCommand:=CUEPOINT_OPERATION;
(*      ulCommand:=NOP_OPERATION;*)
      ulOperandOne:=Block;          (*Cuepoint at 3/4 played buffer*)
      ulOperandTwo:=(3000*CUE_POS) div (BLOCKS_PER_SEC*100); (*MMTIME Unit = 1/3000 sec*)
    end;

    With playlist[block*3 + 1] do begin
      ulCommand:=DATA_OPERATION; (*Datablock, indicating what to play*)

      ulOperandOne:=Ulong(@Buffer)+(Block*BLOCK_LENGTH); (*Address of Buffer*)
      ulOperandTwo:=BLOCK_LENGTH;
    end;

    With playlist[block*3 + 2] do begin
      ulCommand:=BRANCH_OPERATION;
      If block=NUMBER_OF_BLOCKS-1 then
        ulOperandTwo:=0            (*Jump to Start of List*)
      else
        ulOperandTwo:=(block+1)*3; (*Jump to Next Block (redundant)     *)
                                   (*I hope, this will enable the next  *)
                                   (*Data block to be edited while this *)
                                   (*Block is played.*)
    end;

  end;
end;

(*****************************************************************************
 *                     Error Handling                                        *
 ****************************************************************************)


Procedure MCIERROR;
var s:string;
begin
 if ulError<>0 then begin
   mciGetErrorString(ulError,@s[1],255 );
   SetPSZLen(s);
(*   ShowMessageBox (s);*)
   s:=s;
(*   ShowMessageBox ('Error '+Strint(ulError));*)
 end;
end;




(*******************************************************)
(*                    SoundThread                      *)
(*******************************************************)
Procedure SoundThread(param1:Ulong);
begin

  while WinGetMsg ( habSound, q_msg, 0, 0, 0 ) do
    WinDispatchMsg ( habSound, q_msg );

  WinDestroyWindow( hwndSound );
  WinDestroyMsgQueue( h_mq );
  WinTerminate (habSound);
end;

(*******************************************************)
(*                SoundWndProc                         *)
(*******************************************************)

function SoundWndProc(Window: HWnd; Msg: ULong;
                                   Mp1,Mp2: MParam): MResult;
begin
(*  if msg=MM_MCIPLAYLISTMESSAGE then
    MixVoices(mp2)*)
  if msg=MM_MCICUEPOINT then begin
    if SoundWatchDog-LastWatchDog>=-10 then begin
      if LastWatchDog<SoundWatchDog then
        LastWatchDog:=SoundWatchDog;
      Inc(LastWatchDog);

      MixVoices(Mp2);
    end
    else
      PausePlayback;
  end
  else begin
    SoundWndProc:=WinDefWindowProc ( Window, msg, mp1, mp2 );
    exit;
  end;
  SoundWndProc:=0;
end;


(*************************************************************************)
(* MixVoices, Mixes Voices for the next Data-Block                       *)
(*            This Routine uses Intel Assembler to Speed up Mixing a bit *)
(*                                                                       *)
(*            The Assembler part adds each Voice, frequency-scaled to    *)
(*            the Data-Block. There are (currently) two almost identical *)
(*            parts. One applies the volume setting (scaling down) and   *)
(*            the other does not (=full volume).                         *)
(*                                                                       *)
(*            Frequency Scaling is done with fixed point-numbers:        *)
(*            The Field OfsTemp (fixed-point) is incremented by the      *)
(*            pre-calculated field DXTEMP (delta-x). The Integer-Part of *)
(*            OfsTemp ([OfsTemp+2]) is taken for the address calculation *)
(*                                                                       *)
(*************************************************************************)
var
  DxTemp,                      (*Global Variables speed up Considerably*)
  LoopTemp,OfsTemp:Ulong;      (*because of direct addressing.         *)
                               (*(Local Vars do [bp+idx])              *)
  VolTemp,SizeTemp:SmallWord;
  EndAddr:Ulong;

const
  PreviousBlock:integer=1;

Procedure MixVoices(PrevBlock:Integer);
var
  VoiceIdx:Integer;
  MixNum:Integer;
  BlockNum:Integer;
  SourceAddr,BuffAddr,DestAddr:Ulong;
  p:PChar;

begin
  PrevBlock:=PreviousBlock;
  If PreviousBlock=1 then
    PreviousBlock:=0
  else
    PreviousBlock:=1;

  currentblock:=PrevBlock; (*global (debugging) info *)

  If PrevBlock=NUMBER_OF_BLOCKS-1 then
    BlockNum:=0
  else
    BlockNum:=PrevBlock+1;

  BuffAddr:=Ulong(@Buffer)+(BlockNum*BLOCK_LENGTH);
  p:=Pointer(BuffAddr);

  MixNum:=0;
  VoiceIdx:=0;

  FillChar(p^,BLOCK_LENGTH,#0);

  While (MixNum<Mix_Voices) and (VoiceIdx<Max_Voices) do begin
    With Voice[VoiceIdx] do begin
      If Playing then begin
(*        If VoiceIdx=2 then begin
          DxTemp:=1;
        end;*)
        DestAddr:=BuffAddr;
        EndAddr:=BuffAddr+BLOCK_LENGTH;

        OfsTemp:=SourceOfs.Fix;
        SourceAddr:=StartAddr;
        DxTemp:=dx.Fix;
        SizeTemp:=Size;
        LoopTemp:=LoopPos;
        VolTemp:=VolumeLeft;

        If VolTemp=256 then begin (*Full Volume, no scaling*)
          (*While DestAddr<EndAddr do begin*)
          asm
            mov edi,[DestAddr]
            mov ebx,[DxTemp]
            mov dx,[SizeTemp]

          @loop1:
            cmp edi,[EndAddr]
            jge @endloop

            mov esi,[SourceAddr]
            movsx ecx,Word Ptr [OfsTemp+2]
            add esi,ecx
            (*SourceAddr:=StartAddr+SourceOfs.i;*)

            mov al,[esi]
            add [edi],al
            (*Inc(Mem[DestAddr],Mem[StartAddr+SourceOfs.i]);*)
            inc edi
            (*Inc(DestAddr);*)
            add [OfsTemp],ebx
            (*Inc(SourceOfs.Fix,dx.fix);*)
            cmp Word Ptr [OfsTemp+2],dx
            (*If SourceOfs.i>=Size then begin*)
            jl @loop1
            mov ecx,[LoopTemp]
            cmp ecx,-1
            (*If LoopPos<>-1 then begin*)
            je @endloop
            mov cx,[SizeTemp]
            sub cx,Word Ptr [LoopTemp]
            sub Word Ptr [OfsTemp+2],cx
            (*Dec( SourceOfs.i,(SizeTemp-LoopTemp)*)
            cmp Word Ptr [OfsTemp+2],dx
            (*just to Avoid gpfs if delta gets bigger than sample length*)
            jl @loop1
            mov [OfsTemp],0

            jmp @loop1
          @endloop:
            mov [DestAddr],edi
          end;
        end
        else begin

          asm  (*With Volume Scaling*)
            mov edi,[DestAddr]
            mov ebx,[DxTemp]
            mov dx,[SizeTemp]

          @loop1:
            cmp edi,[EndAddr]
            jge @endloop

            mov esi,[SourceAddr]
            movsx ecx,Word Ptr [OfsTemp+2]
            add esi,ecx
            (*SourceAddr:=StartAddr+SourceOfs.i;*)

            xor ax,ax
            mov al,[esi]

            db 66h,0Fh,0AFh,05h
            dd VolTemp
            (*imul ax,[VolTemp]*) (* (ax*volume) div 256 *)
            shr ax,8

            add [edi],al
            (*Inc(Mem[DestAddr],Mem[StartAddr+SourceOfs.i]);*)
            inc edi
            (*Inc(DestAddr);*)
            add [OfsTemp],ebx
            (*Inc(SourceOfs.Fix,dx.fix);*)
            cmp Word Ptr [OfsTemp+2],dx
            (*If SourceOfs.i>=Size then begin*)
            jl @loop1
            mov ecx,[LoopTemp]
            cmp ecx,-1
            (*If LoopPos<>-1 then begin*)
            je @endloop
            mov cx,[SizeTemp]
            sub cx,Word Ptr [LoopTemp]
            sub Word Ptr [OfsTemp+2],cx
            (*Dec( SourceOfs.i,(SizeTemp-LoopTemp)*)
            cmp Word Ptr [OfsTemp+2],dx
            (*just to Avoid gpfs if delta gets bigger than sample length*)
            jl @loop1
            mov [OfsTemp],0

            jmp @loop1
          @endloop:
            mov [DestAddr],edi
          end;
        end;
        Voice[VoiceIdx].SourceOfs.Fix:=OfsTemp;

        If DestAddr<>EndAddr then begin
          Playing:=false;
          DestAddr:=EndAddr;
        end;

        Inc(MixNum);
      end;
    end;
    Inc(VoiceIdx);
  end;
end;


(******************************************************************************
 *                     OpenSoundDevice                                        *
 *****************************************************************************)
Function OpenSoundDevice:boolean;
const
  ulOpenFlags:ULONG = MCI_WAIT         or MCI_OPEN_PLAYLIST or
                      MCI_OPEN_TYPE_ID or MCI_OPEN_SHAREABLE;

begin
  (* Open the correct waveform device for the waves with MCI_OPEN *)

  mciOpenParameters.pszDeviceType := PSZ(MCI_DEVTYPE_WAVEFORM_AUDIO);
(*  mciOpenParameters.pszDeviceType:= @DevType[1];*)

  (* The address of the buffer containing the waveform file. *)
  mciOpenParameters.pszElementName := PSZ(@PlayList[0]);

  mciOpenParameters.hwndCallback  := hwndSound(*HWND(0)*);
  mciOpenParameters.pszAlias      := Nil;

  (* Open the waveform file in the playlist mode. *)
  ulError:=mciSendCommand(0,                  (* We don't know the device yet.        *)
                          MCI_OPEN,           (* MCI message.                         *)
                          ulOpenFlags,        (* Flags for the MCI message.           *)
                          mciOpenParameters,  (* Parameters for the message.          *)
                          0 );                (* Parameter for notify message.        *)
  (* save device ID *)
  SoundDevice.usSoundDeviceID := mciOpenParameters.usDeviceID;

  OpenSoundDevice:=ulError=0;
end;


(*****************************************************************************
 *                     AdjustSoundDevice                                     *
 ****************************************************************************)
Procedure AdjustSoundDevice;
var
  mwspWaveFormParameters:MCI_WAVE_SET_PARMS; (* Waveform parameters.       *)
begin
  (* Fill the structure with zeros. *)
  Fillchar(mwspWaveFormParameters,sizeof(mwspWaveFormParameters),#0);

  SoundDevice.ulSamplesPerSec := MIX_RATE;
  SoundDevice.usBitsPerSample := MIX_BITS;

  (* copy samps/sec *)
  mwspWaveFormParameters.ulSamplesPerSec := SoundDevice.ulSamplesPerSec;
  mwspWaveFormParameters.usBitsPerSample := SoundDevice.usBitsPerSample;
  mwspWaveFormParameters.ulAudio         := MCI_SET_AUDIO_ALL;

  ulError:=mciSendCommand(SoundDevice.usSoundDeviceID, (* Device to play the waves.     *)
                          MCI_SET,                     (* MCI message.                  *)
                          MCI_WAIT or
                          MCI_WAVE_SET_SAMPLESPERSEC or(* Flags for the MCI message.    *)
                          MCI_WAVE_SET_BITSPERSAMPLE ,
                          mwspWaveFormParameters,      (* Parameters for the message.   *)
                          0);                          (* Parameter for notify message. *)

  MCIERROR;
end;

(*****************************************************************************
 *   PausePlayBack. Pauses the Sound playback.
 ****************************************************************************)
Function PausePlayback:boolean;
var ulError:ULONG;
begin
  PausePlayBack:=FALSE;
  if PlayBackActive then begin
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID, (* Device to play the waves.     *)
                            MCI_PAUSE,                   (* MCI message.                  *)
                            0,                           (* Flags for the MCI message.    *)
                            mciOpenParameters,           (* Parameters for the message.   *)
                            0);              (* Parameter for notify message. *)
    If ulError=0 then begin
      PausePlayBack:=TRUE;
      SoundPaused:=TRUE;
    end;
  end;
end;

(*****************************************************************************
 *   ResumePlayBack. Resumes the Sound playback.
 ****************************************************************************)
Function ResumePlayback:boolean;
var ulError:ULONG;
begin
  ResumePlayBack:=FALSE;
  if PlayBackActive and SoundPaused then begin
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID, (* Device to play the waves.     *)
                            MCI_RESUME,                  (* MCI message.                  *)
                            0,                           (* Flags for the MCI message.    *)
                            mciOpenParameters,           (* Parameters for the message.   *)
                            0);              (* Parameter for notify message. *)
    If ulError=0 then begin
      ResumePlayBack:=TRUE;
      SoundPaused:=FALSE;
    end;
  end;
end;

(*****************************************************************************
 *   StartPlayBack. Starts Playing the Playlist (&Mixing)                    *
 ****************************************************************************)
Function StartPlayback:boolean;
begin
  StartPlayBack:=FALSE;
  If DriverOpen then begin
    AdjustSoundDevice;
    PlayBackActive:=TRUE;
    SoundPaused:=FALSE;

                (*Pre-Mix the First Block (signal for previous=last block)*)
    WinPostMsg(hwndSound,MM_MCICUEPOINT,
                             NUMBER_OF_BLOCKS-1,NUMBER_OF_BLOCKS-1);
    (*Stop Previous Playing*)
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID, (* Device to play the waves.     *)
                            MCI_STOP,                    (* MCI message.                  *)
                            MCI_WAIT,                    (* Flags for the MCI message.    *)
                            mciOpenParameters,           (* Parameters for the message.   *)
                            0);              (* Parameter for notify message. *)

    (* rewind sound *)
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID,
                            MCI_SEEK,
                            MCI_TO_START,
                            mciOpenParameters,
                            0);
    (* play sound *)
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID,
                            MCI_PLAY,
                            (*MCI_WAIT*)0,
                            mciOpenParameters,
                            0);
    If ulError=0 then
      StartPlayBack:=TRUE;
  end
  else
    PlayBackActive:=FALSE;
end;

(*****************************************************************************
 *   StopPlayBack. Stops Playing the Playlist (& Mixing)                     *
 ****************************************************************************)
Function StopPlayback:boolean;
begin
  StopPlayBack:=FALSE;
  PlayBackActive:=FALSE;
  If DriverOpen then begin
    (*Stop Previous Playing*)
    SoundPaused:=FALSE;
    ulError:=mciSendCommand(SoundDevice.usSoundDeviceID, (* Device to play the waves.     *)
                            MCI_STOP,                    (* MCI message.                  *)
                            MCI_WAIT,                    (* Flags for the MCI message.    *)
                            mciOpenParameters,           (* Parameters for the message.   *)
                            0);              (* Parameter for notify message. *)
    If ulError=0 then
      StopPlayBack:=TRUE;
  end;
end;

(******************************************************************************)
(*                          CloseSoundDevice                                  *)
(******************************************************************************)
Procedure CloseSoundDevice;
var
  usCounter:SmallInt;
  NilPtr:Pointer;
begin
  NilPtr:=Nil;

  (* close sound device *)
  ulError:=mciSendCommand(SoundDevice.usSoundDeviceID,  (* Device to play the chimes.    *)
                          MCI_CLOSE,                    (* MCI message.                  *)
                          MCI_WAIT,                     (* Flags for the MCI message.    *)
                          NilPtr^,                      (* Parameters for the message.   *)
                          0);                           (* Parameter for notify message. *)
  MCIERROR;
end;


(*******************************************************)
(*                 ShowMessageBox                      *)
(*******************************************************)
Procedure ShowMessageBox (message:string);
begin
  if message[length(message)]<>#0 then
    message:=message+#0;
  WinMessageBox (HWND_DESKTOP, HWND_DESKTOP,
                 @message,'Sound Driver',0,MB_OK);
end;

begin
  GlobalVolume:=100;
  NumWavs:=0;
  NumVoices:=0;
  PlayBackActive:=FALSE;
  DriverOpen:=FALSE;
  tidSound:=0;
  FillChar(Voice,sizeof(Voice),#0);
end.
