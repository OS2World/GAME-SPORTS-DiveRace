(***************************************************************************\
*
* Module Name: DIVE.H
*
* OS/2 2.1 Multimedia Extensions Display Engine API data structures
*
* Copyright (c) International Business Machines Corporation 1993, 1994
*                         All Rights Reserved
*
*
\****************************************************************************)


unit Dive;

{$Cdecl+,AlignRec-,OrgName+}
{$L DIVE.LIB}

{$L PMWIN2.LIB}

interface

uses OS2DEF,Use32;

const
  MAX_DIVE_INSTANCES    =64;

type
  FOURCC=ULONG;
  HDIVE=ULONG ;

const
  DIVE_SUCCESS                    =$00000000;
  DIVE_ERR_INVALID_INSTANCE       =$00001000;
  DIVE_ERR_SOURCE_FORMAT          =$00001001;
  DIVE_ERR_DESTINATION_FORMAT     =$00001002;
  DIVE_ERR_BLITTER_NOT_SETUP      =$00001003;
  DIVE_ERR_INSUFFICIENT_LENGTH     =$00001004;
  DIVE_ERR_TOO_MANY_INSTANCES      =$00001005;
  DIVE_ERR_NO_DIRECT_ACCESS        =$00001006;
  DIVE_ERR_NOT_BANK_SWITCHED       =$00001007;
  DIVE_ERR_INVALID_BANK_NUMBER     =$00001008;
  DIVE_ERR_FB_NOT_ACQUIRED         =$00001009;
  DIVE_ERR_FB_ALREADY_ACQUIRED     =$0000100a;
  DIVE_ERR_ACQUIRE_FAILED          =$0000100b;
  DIVE_ERR_BANK_SWITCH_FAILED      =$0000100c;
  DIVE_ERR_DEACQUIRE_FAILED        =$0000100d;
  DIVE_ERR_INVALID_PALETTE         =$0000100e;
  DIVE_ERR_INVALID_DESTINATION_RECTL       =$0000100f;
  DIVE_ERR_INVALID_BUFFER_NUMBER   =$00001010;
  DIVE_ERR_SSMDD_NOT_INSTALLED     =$00001011;
  DIVE_ERR_BUFFER_ALREADY_ACCESSED =$00001012;
  DIVE_ERR_BUFFER_NOT_ACCESSED     =$00001013;
  DIVE_ERR_TOO_MANY_BUFFERS        =$00001014;
  DIVE_ERR_ALLOCATION_ERROR        =$00001015;
  DIVE_WARN_NO_SIZE                =$00001100;

  DIVE_BUFFER_SCREEN               =$00000000;
  DIVE_BUFFER_GRAPHICS_PLANE       =$00000001;
  DIVE_BUFFER_ALTERNATE_PLANE      =$00000002;

(* Notes:
      Associated/Allocated memory buffers start at:      0x00000010

      Specifing DIVE_BUFFER_GRAPHICS_PLANE results in the image being
            transferred to the graphics plane.
      Specifing DIVE_BUFFER_ALTERNATE_PLANE results in the image being
            transferred to the alternate (e.g. overlay) plane.  If your
            hardware doesn't support such a plane, this is an error.
      Specifing DIVE_BUFFER_SCREEN will result in the image being
            transferred to either the graphics plane buffer or the alternate
            plane buffer based on if an alternate buffer exists and based on
            the suitability the overlay plane to accelerate the scaling of
            the image.  If DIVE chooses to use the alternate buffer, it
            will also paint the overlay "key" color on the graphics plane.
            This automatic painting does not occur if the alternate plane
            is explicitly specified.
*)

type

  DIVE_CAPS=record

    ulStructLen           :Ulong;(* Set equal to sizeof(DIVE_CAPS)          *)
    ulPlaneCount          :Ulong;(* Number of defined planes.               *)

  (* Info returned in the following fields pertains to ulPlaneID.           *)
    fScreenDirect         :Bool; (* TRUE if can get addressability to vram. *)
    fBankSwitched         :Bool; (* TRUE if vram is bank-switched.          *)
    ulDepth               :Ulong;(* Number of bits per pixel.               *)
    ulHorizontalResolution:Ulong;(* Screen width in pixels.                 *)
    ulVerticalResolution  :Ulong;(* Screen height in pixels.                *)
    ulScanLineBytes       :Ulong;(* Screen scan line size in bytes.         *)
    fccColorEncoding      :FourCC;(* Colorspace encoding of the screen.      *)
    ulApertureSize        :Ulong; (* Size of vram aperture in bytes.         *)

    ulInputFormats        :Ulong;  (* Number of input color formats.          *)
    ulOutputFormats       :Ulong;  (* Number of output color formats.         *)
    ulFormatLength        :Ulong;  (* Length of format buffer.                *)
    pFormatData           :Pointer;(* Pointer to color format buffer FOURCC's.*)
  end;

  PDRIVE_CAPS=^DIVE_CAPS;


(* Notes:
      DiveSetupBlitter may be called with a structure length at any of the
      breaks below (i.e. 8, 28, 32, 52, 60, or 68):
*)

  SETUP_BLITTER=record
    ulStructLen         :Ulong; (* Set equal to sizeof(SETUP_BLITTER)      *)
    fInvert             :Bool;  (* TRUE if we are to invert image on blit. *)

    fccSrcColorFormat   :FourCC;(* Color format of source data.            *)
    ulSrcWidth          :Ulong; (* Source width in pixels.                 *)
    ulSrcHeight         :Ulong; (* Source height in pixels.                *)
    ulSrcPosX           :Ulong; (* Source start X position.                *)
    ulSrcPosY           :Ulong; (* Source start Y position.                *)

    ulDitherType        :Ulong; (* Where 0 is no dither, 1 is 2x2 dither.  *)

    fccDstColorFormat   :FourCC;(* Color format of destination data.       *)
    ulDstWidth          :Ulong; (* Destination width in pixels.            *)
    ulDstHeight         :Ulong; (* Destination height in pixels.           *)
    lDstPosX            :Long;  (* Destination start X position.           *)
    lDstPosY            :Long;  (* Destination start Y position.           *)

    lScreenPosX         :Long;  (* Destination start X position on screen. *)
    lScreenPosY         :Long;  (* Destination start Y position on screen. *)

    ulNumDstRects       :ULong; (* Number of visible rectangles.           *)
    pVisDstRects        :PRectl;(* Pointer to array of visible rectangles. *)
  end;

  PSETUP_BLITTER=^SETUP_BLITTER;



Function DiveQueryCaps (var DiveCaps:DIVE_CAPS;
                        ulPlaneBufNum:Ulong):Ulong;

Function DiveOpen ( var hDiveInst:HDIVE;
                    fNonScreenInstance:Bool;
                    var pFrameBuffer:Pointer):Ulong;

Function DiveSetupBlitter (hDiveInst:HDIVE;
                           pSetupBlitter:PSETUP_BLITTER):Ulong;

Function DiveBlitImage ( hDiveInst:HDIVE;
                         ulSrcBufNumber:Ulong;
                         ulDstBufNumber:Ulong):Ulong;

Function DiveClose ( hDiveInst:HDIVE ):Ulong;

Function DiveAcquireFrameBuffer ( hDiveInst:Ulong;
                                  var rectlDst:RECTL ):Ulong;

Function DiveSwitchBank ( hDiveInst:HDIVE;
                          ulBankNumber:Ulong):Ulong;

Function DiveDeacquireFrameBuffer ( hDiveInst:HDIVE ):Ulong;

Function DiveCalcFrameBufferAddress ( hDiveInst:HDIVE;
                                      var rectlDest:RECTL;
                                      var pDestinationAddress:Pointer;
                                      var ulBankNumber:Ulong;
                                      var ulRemLinesInBank:Ulong):Ulong;

(* Notes on DiveAllocImageBuffer:
      If ImageBuffer is not NULL, the buffer is associated rather than
      allocated.  If ImageBuffer is not NULL and the buffer number
      pointed to by pulBufferNumber is non-zero, a new buffer pointer is
      associated with the buffer number.  Even though no memory is
      allocated by DiveAllocImageBuffer when user-allocated buffers are
      associated, DiveFreeImageBuffer should be called to release the
      buffer association to avoid using up available buffer indexes.
      The specified line size will be used if a buffer is allocated in
      system memory, or if a user buffer is associated.  If the
      specified line size is zero, the allocated line size is rounded up
      to the nearest DWORD boundry.
*)

Function DiveAllocImageBuffer ( hDiveInst:HDIVE;
                                var ulBufferNumber:Ulong;
                                fccColorSpace:FOURCC;
                                ulWidth:ULong;
                                ulHeight:Ulong;
                                ulLineSizeBytes:Ulong;
                                ImageBuffer:Pointer):Ulong;

Function DiveFreeImageBuffer ( hDiveInst:HDIVE;
                               ulBufferNumber:Ulong):Ulong;

Function DiveBeginImageBufferAccess ( hDiveInst:HDIVE;
                                      ulBufferNumber:Ulong;
                                      var pbImageBuffer:Pointer;
                                      var ulBufferScanLineBytes:Ulong;
                                      var ulBufferScanLines:Ulong):Ulong;

Function DiveEndImageBufferAccess ( hDiveInst:HDIVE;
                                    ulBufferNumber:Ulong):Ulong;



(* Notes on palettes:
      Neither DiveSetSourcePalette nor DiveSetDestinationPalette API's will set
      the physical palette.  If your application MUST set the PHYSICAL palette,
      try using no more than 236 entries (the middle 236: 10-245, thus leaving
      the top and bottom 10 entries for the Workplace Shell).  If your
      application MUST use ALL 256 entries, it must do so as a full-screen
      (i.e. maximized) application.  Remember, No WM_REALIZEPALETTE message
      will be sent to other running applications, meaning they will not redraw
      and their colors will be all wrong.  It is not recommended that a
      developer use these commands:

   To set physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            GpiCreateLogColorTable ( hps, LCOL_PURECOLOR | LCOL_REALIZABLE,
                           LCOLF_CONSECRGB, 0, 256, (PLONG)plRGB2Entries );
            DiveSetPhysicalPalette ( hDiveInst, hdc );
            Gre32EntrY3 ( hdc, 0L, 0x000060C6L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );

   To reset physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            Gre32EntrY3 ( hdc, 0L, 0x000060C7L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );
*)


Function DiveSetDestinationPalette ( hDiveInst:HDIVE;
                                     ulStartIndex:Ulong;
                                     ulNumEntries:Ulong;
                                     pbRGB2Entries:Pointer):Ulong;

Function DiveSetSourcePalette ( hDiveInst:HDIVE;
                                ulStartIndex:Ulong;
                                ulNumEntries:Ulong;
                                pbRGB2Entries:Pointer):Ulong;


(*Additional PMWIN Functions*)

Function WinQueryVisibleRegion (h_wnd:HWND; h_rgn:HRGN):Ulong;
Function WinSetVisibleRegionNotify (h_wnd:HWND; fEnable:BOOL):BOOL;



implementation

Function DiveQueryCaps;external;
Function DiveOpen;external;
Function DiveSetupBlitter;external;
Function DiveBlitImage;external;
Function DiveClose;external;
Function DiveAcquireFrameBuffer;external;
Function DiveSwitchBank;external;
Function DiveDeacquireFrameBuffer;external;
Function DiveCalcFrameBufferAddress;external;
Function DiveAllocImageBuffer;external;
Function DiveFreeImageBuffer;external;
Function DiveBeginImageBufferAccess;external;
Function DiveEndImageBufferAccess;external;
Function DiveSetDestinationPalette;external;
Function DiveSetSourcePalette;external;

Function WinQueryVisibleRegion;external;
Function WinSetVisibleRegionNotify;external;

end.
