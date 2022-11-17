#Include Once"windows.bi"


Dim shared As HWND hwnd 
''using fbsound for audio for now WIP
'#include "../inc/fbsound.bi"

'the nes system starts here//////////
'#Include Once  "nes/old/NES.bi" ' WITHOUT AUDIO



Dim shared CPU_CHOOSE as bool = false 'TRUE is old cpu olc6502.bi and olc6502.bas
'type cpu_type as olc6502
Type cpu_type as CPU 'NesJS CPU

Dim shared PPU_CHOOSE as bool = false 'TRUE is old PPU olc2C02.bi and olc2C02.bas
'type ppu_type as olc2C02
Type ppu_type as PPU

'dim shared CART_CHOOSE as bool = false 'TRUE is old cart cartridge.bi and cartridge.bas
'type cart_type as ????
'type cart_type as ????

Dim Shared keysenable As bool

'WITH AUDIO///////////////////////
#Include Once  "nes\NES.bi"

'#include Once "NES\bus.bas"
'#include Once "NES\BUS_OLDFORTESTING.bas"


'#include Once "NES\Audio.bas"
'/////////////////////////////////


'///////////////////////////////////

Dim shared nes as bus

Dim shared audio_hndler as AudioHandler ptr 
	

Dim shared elapsedMS as float
Dim shared as uint64_t end1 
Dim shared as uint64_t start1

Dim shared frmenums as uint32_t

Dim shared emurun as boolean = false

'
'pattables(0)=ImageCreate(128,128,RGB(0,0,0),32)
'pattables(1)=ImageCreate(128,128,RGB(0,0,0),32)
#Define GET_X_LPARAM(LPARAM) LoWord(LPARAM)
#Define GET_Y_LPARAM(LPARAM) HiWord(LPARAM)

   
#Include Once"crt.bi"
#Include Once "win/commctrl.bi"
#Include Once  "fbgfx.bi"
#Include "file.bi"
#Include "filesaveopen.bas"

#Include "resources.bi"


#Define SCREEN_WIDTH  640								'	// We want a 800 pixel width resolution
#Define SCREEN_HEIGHT 480								'	// We want a 600 pixel height resolution



#Define KEYDOWN(vk_code) (IIf(GetAsyncKeyState(vk_code) And &H8000), 1, 0)
#Define KEYUP(vk_code)   (IIf(GetAsyncKeyState(vk_code) And &H8000),0, 1)

Const DEF_CX = 256 
Const  DEF_CY = 240

Using fb
Dim Shared fResidualTime As float
Dim Shared fElapsedTime As float
Dim Shared fElapsedstart As float

Dim SHARED ff As Integer
Dim SHARED fps As Integer
Dim SHARED start As Single
Dim SHARED bEmulationRun As bool = false
Dim SHARED bSingleFrame As bool = FALSE


Dim Shared isFpressed As bool = FALSE

Dim Shared ispressed As bool = FALSE

Dim Shared cart As String
Dim Shared pic1 As String
Dim Shared zoom_scrn As Integer = 2



Dim Shared nesrect As rect => (0,0,256*2 ,240*2) '*1.45

Dim Shared bFullScreen As bool = FALSE 								'	// Create a boolean and set it to false.  If we choose full screen, set it to TRUE

	 Dim Shared hmenu As HMENU  
	  Dim Shared hViewsub As HMENU
	  
	  Dim Shared hFilesub As HMENU
	  Dim Shared hFile_Recent_sub As HMENU

	  Dim Shared hView_Zoom_sub As HMENU
	  
	  Dim Shared my_pattables As uint32_t Ptr'(128*128) 
	  Dim Shared As HMENU hFile_VidRecord_sub 
			Dim Shared As  HMENU hFile_AudRecord_sub 
	  
fElapsedstart = Timer

Declare Function ChangeToFullScreen(width1 As Integer ,height1 As  Integer ) As BOOL


Declare sub audioloop()
Declare sub debug1()

audio_hndler = new AudioHandler(48000,AUDIO_S16SYS,2048,16)
audio_hndler->start_aud()

 dim shared thread1 as any ptr 
 
'thread1 =  threadcreate(@audioloop)


Dim shared quit as boolean = false

Dim shared muted as bool

MMapTemplate(UINT16T ,String)

Dim shared mapasm As  MAPNODEUINT16TSTRING Ptr
Dim shared map As  TMAPUINT16TSTRING



Dim Shared pat_imgdata(512*480) As uInteger


Dim Shared nme_imgdata(512*480) As UInteger



Sub drawtile(imgdata As UByte Ptr, x As Integer,y As Integer, num As Integer, col As Integer)
	
  for i as integer = 0 To 8 - 1
  	  '    // for each row
       dim lp As UByte = getmapper()->ppuPeak(num * 16 + i) 
       Dim hp As UByte = getmapper()->ppuPeak(num * 16 + i + 8) 
		for j as integer = 0 To 8 - 1
		
	 
			 dim shift As UByte = 7 - j
			 
			 Dim pixel As UByte = (lp shr shift) and 1
			 
		
			 
			 pixel Or= ((hp shr shift) and 1) Shl 1
			 
		 	 dim pind As UByte = IIf (pixel = 0, 0, col * 4 + pixel)
			 
			 
			 
			Dim As Integer index = ((y + i) * 512 + (x + j)) * 4 
			
			Dim As UByte Ptr _color = Cast(UByte Ptr,@palScreen(nes.ppu.readPalette(pind) and &H3f)) '@Cast(UByte Ptr,@nes.ppu.nesPal(0,0))[0] 
			
			
			
			
	      imgdata[index] = _color[0]  '// r
         imgdata[index + 1] = _color[1]  '// g
         imgdata[index + 2] = _color[2]  '// b
         imgdata[index + 3] = 255  '// a
			
		Next
  Next

End Sub 


Sub fillrct(buf As UInteger Ptr, x1 As Integer,y1 As Integer,x2 As Integer,y2 As Integer,col As UByte Ptr)
	
	
	       
         For y As Integer = 0 To y2-1 
      	For x As Integer = 0 To x2-1 
        
                       pat_imgdata(((y +y1)*512) + (x+x1)) =  rgba(col[2],col[1],col[0], 255) 'palScreen(nes.ppu.readPalette(0) and &H3f)
       
       
        	
        	Next
        Next
	
	
	
	
End Sub





Sub drawPatternsPals()
 
	
	For x As integer = 0 To 16 -1
		For y As Integer = 0 To 16 -1
		   drawTile(@pat_imgdata(0), x * 8, y * 8, y * 16 + x, 0) 
		Next
	Next
	
		For x As integer = 0 To 16 -1
		For y As Integer = 0 To 16 -1
		   drawTile(@pat_imgdata(0),128 + (x * 8), y * 8, 256 + (y * 16 + x), 0) 
		Next
	Next
		
	
      For i As Integer = 0 To 16-1
       Dim  col As UByte Ptr = Cast(UByte Ptr,@palScreen(nes.ppu.readPalette(i) and &H3f)) 
       fillrct(@pat_imgdata(0),i*16,128,16,16,col)
       col = Cast(UByte Ptr,@palScreen(nes.ppu.readPalette(i+16) and &H3f)) 
		 fillrct(@pat_imgdata(0),i*16,144,16,16,col)
       
      Next
	
End Sub



Sub drawNametables()


	
	For x As Integer = 0 To 64 - 1
			For y As Integer = 0 To 60 - 1
				dim as UInteger ry = y + iif(y >= 30, 2, 0)
				Dim As UInteger tileNumAdr	= &H2000 + iif(ry > 31 , &H800 , 0) + iif(x > 31 , &H400 , 0)
				tileNumAdr += ((ry and &H1f) shl 5) + (x And &H1f) 
            Dim As UByte tileNum = getmapper()->ppuPeak(tileNumAdr)
				Dim  As UInteger attAdr = &H23c0 + iif(ry > 31 , &H800 , 0) + iif(x > 31 , &H400 , 0)
				attAdr += ((ry and &H1c) Shl 1) + ((x and &H1c) shr 2)
				Dim As UByte atr = getmapper()->ppuPeak(attAdr)
				
				
        if((ry and &H2) > 0) Then  
         ' // bottom half
          atr Shr= 4 
        End If
        
        atr and= &Hf 
        if((x and &H2) > 0) Then
          '// right half
          atr Shr= 2 
        endif
        atr And= &H3 

        drawTile(@nme_imgData(0), x * 8, y * 8, tileNum + iif(nes.ppu.bgPatternBase = 0, 0, 256), atr) 
				
				
			Next
	Next

End Sub


		
	Function hex1 (n As uint32_t,  d As uint8_t) As  string
	
		 Dim s As String = String(d, "0")
		 Dim i As Integer 
		
		'for (int i = d - 1; i >= 0; i--, n >>= 4)
		i = d-1
		While i >= 0
			s[i] = Asc("0123456789ABCDEF", (n And &Hf)+1)  '[n And &HF]
				n shr= 4
			   i-=1
		Wend
		'	s[i] = "0123456789ABCDEF"[n & &HF];
		'Next
		
		return s
	End Function


Sub drawram(x1 as integer,y1 as integer,naddr as uint16_t,nRows as integer,ncolumns as integer)
	
	dim nramx as integer = x1
	dim nramy as integer = y1
	color 255
	for row as integer = 0 to nRows-1
		
		dim sOffset as string = "$" + hex1(naddr,4)+":"
		
		for col as integer = 0 to ncolumns-1
			
			sOffset += " " + hex1(nes._read(nAddr), 2)
	 
		naddr+=1
		Next
		locate nramy,nramx
		print sOffset
		
		
		nRamy+=1
	Next
	
	
End Sub



Sub drawcode(x as integer ,y as integer ,nLiplayer as integer,old as bool)
	
	








Dim nOff as uint16_t



'CHANGE WHEN NEEDED////////////////

If old then
	 nOff=nes.cpu.pc
	else
	 nOff=nes.cpu.br(nes.cpu.pc)
	end if 
'//////////////////////////////////	 
	 
	 
	mapasm = map.findaddr(nOff)
	dim nLiney as Integer = (nLiplayer shr 1) * 1 + y
	
	color 255
	if mapasm <> null then
		color 10
		locate nLineY,x
			
	  print *mapasm->nData
	  'nOff+=1
	  color 255
	  while nliney < (nLiplayer *1) + y
	

	
	nOff+=1
	mapasm = map.findaddr(nOff)
	
	
	if mapasm <> null then
	nLineY += 1
	
	locate nLineY,x
	 print *mapasm->nData
	elseif nOff  = &HFFFF then
			nLineY += 1
	
	locate nLineY,x
	 print

	end if 
	 
	 
	wend 
	end if
	
	
		'mapasm = map.findaddr(player.cpu.pc)
	
	nliney = (nLiplayer shr 1) * 1 + y
	
	
	
	'CHANGE WHEN NEEDED////////////////
 if old then
	 nOff=nes.cpu.pc
	else
	nOff=nes.cpu.br(nes.cpu.pc)
	end if 
	'//////////////////////////////////
	
	
	if mapasm <> null then

	  
	  while nliney > y
	
	
	
	nOff-=1
	mapasm = map.findaddr(nOff)
	
	
	if mapasm <> null  then
	nLineY -= 1	
	locate nLineY,x	 
	print *mapasm->nData
	elseif nOff = &HFFFF then
	nLineY -= 1	
	locate nLineY,x	 
	print 
	'	nOff-=1
	 'beep
	end if 
	 
	 
	  wend 
	end if

	

	

'next i
	
	
	
End Sub  



Sub drawOAM(x1 as integer,y1 as integer)
	dim s as string
	'nes.ppu.oamRam(i*4+3)
	LOCATE y1, x1
	
	'26
	for i as integer = 0 to 27-1
		
	s = hex(i,2) + ": (" & str(nes.ppu.oamRam(i*4+3)) & _
						", " & str(nes.ppu.oamRam(i*4+0)) + ") " & _
						"ID: " & hex(i*4+1,2) & _
						" AT: " & hex(nes.ppu.oamram(i*4+2),2) 	
		
		'
		's = hex(i,2) + ": (" & str(pOAM[i*4+3]) & _
		'				", " & str(pOAM[i*4+0]) + ") " & _
		'				"ID: " & hex(pOAM[i*4+1],2) & _
		'				" AT: " & hex(pOAM[i*4+2],2) 	
		'	
		'
		
		
	LOCATE y1+i, x1
	print s	
	Next
	
	

End Sub






Sub DrawCpu (x1 AS INTEGER, y1 AS INTEGER,old as bool)


    LOCATE y1+1, x1

Color 255
'11


'CHANGE WHEN NEEDED/////////////////////////////////

If old then
    Print " status: ";
    Color(IIf(nes.cpu.status And olc6502.N,10,12))
    PRINT " N ";
      Color(IIf(nes.cpu.status And olc6502.V,10,12))
    PRINT " V ";
       Color(IIf(nes.cpu.status And olc6502.U,10,12))
    PRINT " - ";
      Color(IIf(nes.cpu.status And olc6502.B,10,12))
    PRINT " B ";
      Color(IIf(nes.cpu.status And olc6502.D,10,12))
    PRINT " D ";
    Color(IIf(nes.cpu.status And olc6502.I,10,12))
    PRINT " I ";
      Color(IIf(nes.cpu.status And olc6502.Z,10,12))
    PRINT " Z ";
        Color(IIf(nes.cpu.status And olc6502.C,10,12))
    PRINT " C "
    Color 255

Else
    Print " status: ";
    Color(IIf(NES.CPU.N,10,12))
    PRINT " N ";
      Color(IIf(NES.CPU.V,10,12))
    PRINT " V ";
    color 30
       'Color(IIf(true,10,12))
    PRINT " - ";
    color 30
     ' Color(IIf(true,10,12))
    PRINT " - ";
      Color(IIf(NES.CPU.D,10,12))
    PRINT " D ";
    Color(IIf(NES.CPU.I,10,12))
    PRINT " I ";
      Color(IIf(NES.CPU.Z,10,12))
    PRINT " Z ";
        Color(IIf(NES.CPU.C,10,12))
    PRINT " C "
    Color 255
End if
'///////////////////////////////////////////////////////




 'Locate , x

   ' PRINT " ";getflag(N);" ";getflag(V);" ";getflag(U);" "; getflag(B);" ";getflag(D);" ";getflag(I);" "; getflag(Z);" ";getflag(C)
  
 '   Locate , x


   
 
'CHANGE WHEN NEEDED/////////////////////////////////////////////
If old then
    Locate y1+2, x1
    PRINT " PC: $"; LTRIM$(hex1(nes.cpu.PC , 4)) + " [" + LTRIM$(STR$(nes.cpu.PC)) + "]"
    Locate y1+3, x1
    PRINT " A: $"; LTRIM$(hex1(nes.cpu.A, 2)) + " [" + LTRIM$(STR$(nes.cpu.A)) + "]"
    Locate y1+4 , x1
    PRINT " X: $"; LTRIM$(hex1(nes.cpu.X, 2)) + " [" + LTRIM$(STR$(nes.cpu.X)) + "]"
    Locate y1+5, x1
    PRINT " Y: $"; LTRIM$(hex1(nes.cpu.Y, 2)) + " [" + LTRIM$(STR$(nes.cpu.Y)) + "]"
    LOCATE y1+6, x1
    PRINT " Stack P: $"; LTRIM$(hex1(nes.cpu.stkp, 4)) + " [" + LTRIM$(STR$(nes.cpu.stkp)) + "]"
   
    'LOCATE y1, x1+50
 else   
        Locate y1+2, x1
    PRINT " PC: $"; LTRIM$(hex1(nes.cpu.br(nes.cpu.PC), 4)) + " [" + LTRIM$(STR$(nes.cpu.br(nes.cpu.PC))) + "]"
    Locate y1+3, x1
    PRINT " A: $"; LTRIM$(hex1(nes.cpu.r(nes.cpu.A), 2)) + " [" + LTRIM$(STR$(nes.cpu.r(nes.cpu.A))) + "]"
    Locate y1+4 , x1
    PRINT " X: $"; LTRIM$(hex1(nes.cpu.r(nes.cpu.X), 2)) + " [" + LTRIM$(STR$(nes.cpu.r(nes.cpu.X))) + "]"
    Locate y1+5, x1
    PRINT " Y: $"; LTRIM$(hex1(nes.cpu.r(nes.cpu.Y), 2)) + " [" + LTRIM$(STR$(nes.cpu.r(nes.cpu.Y))) + "]"
    LOCATE y1+6, x1
    PRINT " Stack P: $"; LTRIM$(hex1(nes.cpu.r(nes.cpu.SP), 4)) + " [" + LTRIM$(STR$(nes.cpu.r(nes.cpu.SP))) + "]"
   
    'LOCATE y1, x1+50
End if   
    
'//////////////////////////////////////////////////////////////////////////////////////    
    
    
   
    
    
    
    'print "ppustatus: " & bin(nes._read(&H2002),8)
    
    'print "cycles: " & nes.cpu.getcycles()' str(nes.nSystemClockCounter) 
    
     ' LOCATE y1+1, x1+50
    ' print   *mydata1'"banks: " & str(prgbanksptr[4])
    
    
  '  LOCATE y1+1, x1+50
 'print "vblank: " & nes.ppu.invblank


End Sub
'declare Sub Process2 Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)





'Sub Process2 Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)
''
''while not(nes._clock) 
''	
''Wend
'
'
'	'
'	'	    if(that->inputReadPos + 2048 > that->inputBufferPos) then
'   '   'we overran the buffer
'   '  'log("Audio buffer overran");
'   '   that->inputReadPos = that->inputBufferPos - 2048 
'	'    end if
'	'    
'   ' if(that->inputReadPos + 4096 < that->inputBufferPos) then
'   '   '// we underran the buffer
'   '   '//log("Audio buffer underran");
'   '  that->inputReadPos += 2048
'   ' end if
'   ' 
'	'For n as integer = 0 to (audlen/2)-1  
'   ''that->x+=.010
'	'
'	'	dim sample as Sint16 ptr = cast(Sint16 ptr,audbuffer) 
'	'	
'	'	
'	'	'that->sample = sin((that->x*4))*0.20'(rnd*2-1)*0.15
'	'	
'	'	that->intSample = cast(Sint32, that->inputbuffer(that->inputReadPos and &HFFF) * that->maxAmplitude)
'	'	
'	'	sample[n]  = cast(Sint16,that->intSample)
'	'	
'	'	
'	'
'	'	that->inputReadPos+=1
'
'	'Next n
'	
'End Sub

Sub fill_pal_rect(hdc As hdc,x1 As Integer,y1 As Integer,pal1 As uint8_t ,pix As uint8_t ) 
	
		'fillrect(hdc,@Type<rect>(x1,y1,x1+6,y1+6),createsolidbrush(Cast(COLORREF,GetColourFromPaletteRamBGR(pal1,pix))))
	Dim br As hbrush '= createsolidbrush(GetColourFromPaletteRamBGR(pal1,pix))
	
	fillrect(hdc,@Type<rect>(x1,y1,x1+12,y1+12),br)
	
 DeleteObject(br)
	
	
End Sub


Sub fill_pal_rect2(hdc As hdc,x1 As Integer,y1 As Integer,x2 As Integer,y2 As Integer,col as uint32_t  ) 
	
		'fillrect(hdc,@Type<rect>(x1,y1,x1+6,y1+6),createsolidbrush(Cast(COLORREF,GetColourFromPaletteRamBGR(pal1,pix))))
	Dim br As hbrush = createsolidbrush(Cast(COLORREF,bgr(getRvalue(col),getGvalue(col),getBvalue(col))))
	fillrect(hdc,@Type<rect>(x1,y1,x1+x2,y1+y2),br)
	
 DeleteObject(br)
	
	
End Sub

Function caculateLineBytes(width1 As uint32_t)  As uint32_t
    '//********* Four-byte Alignment**********
    return Int((24 * width1 + 31)/32) *4
    '//********* Four-byte Alignment**********
End Function



Sub temp_apu_clock
	
	               
		if (nes.apu.framecounter = 29830 and nes.apu.step5Mode = 0) or _
		nes.apu.framecounter = 37282 then
		nes.apu.frameCounter = 0
	end if
	''
	 nes.apu.frameCounter+=1 ' works similar to nesjs's framecounter
	''
   nes.apu.handleFrameCounter()
	''
	nes.apu.cycleTriangle()
	nes.apu.cyclePulse1()
   nes.apu.cyclePulse2()
	nes.apu.cycleNoise()
	''''this.cycleDmc()

	'''
	nes.apu._output(nes.apu._outputOffset) = nes.apu.mix():nes.apu._outputOffset +=1
			if nes.apu._outputOffset = 29781 then
		nes.apu._outputOffset = 29780
	end if
	
	
	
	
End Sub


'sub temp_clock1()
'	
'	
'	do
'	
'
'	ppu_clock() 
'
'
'		if (nes.apu.framecounter = 29830 and nes.apu.step5Mode = 0) or _
'		nes.apu.framecounter = 37282 then
'		nes.apu.frameCounter = 0
'	end if
'
'	 nes.apu.frameCounter+=1 ' works similar to nesjs's framecounter
'
'   nes.apu.handleFrameCounter()
'
'	nes.apu.cycleTriangle()
'	nes.apu.cyclePulse1()
'   nes.apu.cyclePulse2()
'	nes.apu.cycleNoise()
'	''''this.cycleDmc()
'
'	nes.apu._output(nes.apu._outputOffset) = nes.apu.mix():nes.apu._outputOffset +=1
'			if nes.apu._outputOffset = 29781 then
'		nes.apu._outputOffset = 29780
'	end if
'	
'
'	if (nes.nSystemClockCounter mod 3 =  0) Then
'		If nes.dma_transfer Then
'		'	
'			If nes.dma_dummy Then
'				
'				if (nes.nSystemClockCounter mod 2 =  1) Then
'					nes.dma_dummy = FALSE
'				End If
'		'		
'			Else
'					if (nes.nSystemClockCounter mod 2 =  0) Then
'					nes.dma_data = nes._read(nes.dma_page shl 8 or nes.dma_addr)
'					Else
'				
'						pOAM[nes.dma_addr] = nes.dma_data
'						nes.dma_addr+=1
'				   		
'				      if nes.dma_addr = 0 Then
'							nes.dma_transfer = FALSE
'							nes.dma_dummy = TRUE
'						EndIf
'		
'		 	
'				
'		      End If
'			
'		
'	End If
'     
'		Else
'			
'			nes.cpu._clock()		
'	
'		End If
'
'
'	End If 
'		
'  'dim bAudioSampleReady as bool = false
'  'this.dAudiotime += this.dAudioTimePerNESClock
'  '	if (dAudioTime >= dAudioTimePerSystemSample) then
'		'this.dAudioTime -= this.dAudioTimePerSystemSample 
'		'this.dAudioSample = apu.mix() 
'		'bAudioSampleReady = true 
'  '	end if 
'  '
'	If (getmapper()->irqState()) Then
'
'		GetMapper()->irqClear()
'		'nes.cpu.irq()		
'	End If
'	'
'	if (ppu_nmi) Then
'	
'		ppu_nmi = false 
'	'	nes.cpu.nmi() 
'	End If
'
'	nes.nSystemClockCounter+=1
'
'					Loop while Not(frame_complete)
'
'End Sub



Sub SaveNESScrn(path As String,width1 as integer,height1 as integer,scrndata As uint32_t Ptr = NULL)
	
Dim bmpheader As BITMAPFILEHEADER
Dim bmpinfoheader As BITMAPINFOHEADER

bmpheader.bftype = &H4D42
bmpheader.bfReserved1 = 0
bmpheader.bfReserved1 = 0
bmpheader.bfOffBits = &H36

bmpinfoheader.bisize = SizeOf(bmpinfoheader) 
bmpinfoheader.biwidth = width1
bmpinfoheader.biheight = -height1
bmpinfoheader.biXPelsPerMeter = 5000 
bmpinfoheader.biYPelsPerMeter = 5000 
bmpinfoheader.biClrUsed = 0 
bmpinfoheader.biClrImportant = 0 
bmpinfoheader.biPlanes = 1
bmpinfoheader.biBitCount = 24

Dim linebytes As uint32_t = caculateLineBytes(bmpinfoheader.biwidth)
bmpheader.bfsize = SizeOf(BitmapFileHeader) + sizeof(BitmapInfoHeader) + lineBytes * Abs(bmpinfoheader.biheight)



bmpinfoheader.biSizeImage =Int((bmpinfoheader.biwidth  * bmpinfoheader.biBitCount/8)) * Abs(bmpinfoheader.biheight)

Dim savebmpfile As Integer = FreeFile
	Open path For Binary As savebmpfile
	
			
		Put # savebmpfile, ,*Cast(UByte Ptr,@bmpheader),SizeOf(BITMAPFILEHEADER) 
		Put # savebmpfile, ,*Cast(UByte Ptr,@bmpinfoheader),SizeOf(BITMAPINFOHEADER)
		
		
		
Dim pixels As uint8_t Ptr
pixels = New uint8_t[bmpinfoheader.biSizeImage]
'
'Print scrndata[0]
'Print scrndata[1]
'Print scrndata[2]
'Print scrndata[3]
Dim pos1 As uint32_t = 0
For i As uint32_t = 0 To bmpinfoheader.biSizeImage-1 Step 3 
	

	pixels[i+2] = RGBA_R(scrndata[pos1]) ' red

	pixels[i+1] = RGBA_G(scrndata[pos1])'green

	pixels[i+0] = RGBA_B(scrndata[pos1])'blue
pos1+=1
Next








Put # savebmpfile,,pixels[0],bmpinfoheader.biSizeImage







Close savebmpfile


Delete pixels
pixels = NULL


Dim bmpinfo As BITMAPINFOHEADER 
	
	
End Sub


Sub init_rom
Dim bmp As String

Dim s As String

			
	

Print "choose a rom"


	
	'return true '@this.vRAMStatic


 

While cart = ""
 
	
	 cart = file_opensave(NULL)
	 
	If cart = "" Then 
	Print "invalid rom file"
	print " "

	input "select another? y/n: ",s
	If s  = "Y" Or s  = "y" Then
   Print "choose a rom"
	ElseIf s  = "N" Or s  = "n" Then	
	print " "
	
	Print  "press Any key To Quit"
	sleep
	if hwnd <> null then

	  SendMessage(hwnd, WM_CLOSE, 0, 0)
    'return
	 else
	 Sleep
	 End
	End If
	end if
	end if
Wend
 
' sleep
'return

'Print bmp
'	bmp = file_opensave(NULL)
	'Print bmp
	
	
	'dim sram_save as integer = FreeFile

	'	open "Earthbound Zero.batt" for binary as sram_save
	'		' 
	'		 'put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
	'		'
	'		close sram_save

   'if fileexists("Earthbound Zero.batt") then
   '	
   '	kill ("Earthbound Zero.batt") 
   'EndIf
   '
	'		dim sram_save as integer = FreeFile
	'		'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" &
	'		 open  "Earthbound Zero.batt" for binary as sram_save
	'		' 
	'		' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
	'		'
	'		close sram_save
	'	
	'	




'mkdir("C:\NESTEA_BATTSAVES")		
'chdir("C:\NESTEA_BATTSAVES")	


   'if fileexists("Earthbound Zero.batt") then
   '	
   '	kill ("Earthbound Zero.batt") 
   'EndIf
   '
	'		dim sram_save as integer = FreeFile
	'		'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" &
	'		 open  "Earthbound Zero.batt" for binary as sram_save
	'		' 
	'		' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
	'		'
	'		close sram_save
	'	
	'	



setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
'Print cart
'Input   "enter rom path or drag file here: ", cart

insert_cartridge(cart,hwnd)
setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
Input   "fullscreen Y/N?: ", s 
Cls



If s  = "Y" Or s  = "y" Then
	
	bfullscreen = TRUE
	'FreeConsole()

ElseIf s  = "N" Or s  = "n" Then

bfullscreen = FALSE
EndIf

bEmulationRun = false 'true

		If ImageValid = TRUE Then
	Print "CRC: " &  crcfromfile(cart)
	Print "loaded rom!"
	Print "MAGIC NUM: "; header.Name1
	Print "MIRROR MODE: "; IIf(cart_mirror = horizontal,"HORIZONTAL","VERTICAL")
	Print "BATTERY BACKED: "; IIf(bat_saves,"YES","NO")
	Print "ROM BANKS: " & Str(header.prg_rom_chunks)
	Print "PRG ROM SIZE: " & Str(vPRGMemory.size())
	Print "CHRROM BANKS: " & str(header.chr_rom_chunks)
	Print "CHRROM SIZE:" & Str(vCHRMemory.size())
	Print "MAPPER: " & Str(nmapperid)
	Print "PRGRAM SIZE: " & Str(header.prg_ram_size)
	Print header.tv_system1
	Print header.tv_system2
	Print header.unused
	'Print pmapper->prgand
	'Print pmapper->chrand
	Print pmapper->board
	
	if bat_saves then
		
	print "battery loaded"
	print	
		
	EndIf
	
 	else
	Print "rom not found or isnt valid"
	Print 
	Print  "press Any key To Quit"
	
		Sleep
		End
		EndIf

'insert_cartridge("roms/kung fu.nes")
'insert_cartridge("donkey.nes")
'insert_cartridge("Burger Time (U) [!].nes")
'insert_cartridge("Felix the Cat (U).nes")
'insert_cartridge("Felix the Cat (U).nes")
'insert_cartridge("roms/Kirby's Adventure (USA).nes")
'insert_cartridge("roms/Volleyball.nes")
'insert_cartridge("roms/SMB3.nes")
'insert_cartridge("roms/SMB2.nes")
'insert_cartridge("Ren & Stimpy Show, The (U).nes")
'insert_cartridge("California Games (U).nes")
'insert_cartridge("roms/SMB.nes")
'insert_cartridge("ice climber.nes")
'insert_cartridge("roms/Megaman1.nes")

'init_rom

'mapAsm = disassemble(&H0000, &HFFFF) '&H35



  
	



nes._reset(true)





'nes.SetSampleFrequency(44100)

'audio_hndler = new AudioHandler(44100,AUDIO_S16SYS,4096,16,@Process2)
	
 if muted = false	then
 audio_hndler->start_aud()
 else
  audio_hndler->stop_aud()
 end if


End Sub


Function KEYPRESSED(vk_code As Integer) As bool
	static Iskeyup(&HFF) As bool
	  
	If keysenable = TRUE Then
	 If  IIf(GetAsyncKeyState(vk_code) And &H8000, 1, 0) And iskeyup(vk_code) = TRUE Then
		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, TRUE)
	Return TRUE
	 
	ElseIf IIf(GetAsyncKeyState(vk_code) And &H8000, 0, 1) And iskeyup(vk_code)= FALSE Then
		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, true)		
		Return FALSE
	End If
		
	EndIf
		

	
	Return FALSE			
End Function

Sub frames_per_sec(hwnd As hwnd)
    ff  = ff + 1


    IF TIMER - start  >= 1 THEN fps = ff : ff  = 0: start  = TIMER
 SetConsoleTitle "OLC-NES-NESTEA-FB:"+" FPS:" + STR (fps )
 Dim titles1 As String
 titles1="OLC-NES-NESTEA-FB-V0.1:"&" FPS:" & STR (fps )
setwindowtext(hwnd,titles1 )

End Sub

Sub DrawNesScrn(dc As hDC,ImageData() As uint32_t,x1 As int16_t,y1 As int16_t, destwdth As int16_t, desthght As  int16_t)

 'Dim BI As BITMAP
 Dim BI As BITMAPINFO
 
 With BI.bmiHeader
        .biWidth = 256
        .biHeight = -240
        .biSize  = 40
        .biBitCount  = 32
        .biPlanes = 1
 End With
 

'StretchDIBits dc, 0, 0, destwdth, desthght, 0, 0, 341, 261 ,@ImageData(0, 0, 0), @bi,0,SRCCOPY
StretchDIBits dc, x1, y1, destwdth, desthght, 0, 0, 256, 240 ,@ImageData(0), @bi,0,SRCCOPY


End Sub

Sub DrawPatTable(dc As hDC,ImageData As uint32_t Ptr,x1 As int16_t,y1 As int16_t, destwdth As int16_t, desthght As  int16_t)

 'Dim BI As BITMAP
 Dim BI As BITMAPINFO
 
 With BI.bmiHeader
        .biWidth = 128
        .biHeight = -128
        .biSize  = 40
        .biBitCount  = 32
        .biPlanes = 1
 End With
 

'StretchDIBits dc, 0, 0, destwdth, desthght, 0, 0, 341, 261 ,@ImageData(0, 0, 0), @bi,0,SRCCOPY
StretchDIBits dc, x1, y1, destwdth, desthght, 0, 0, 128, 128 ,ImageData[0], @bi,0,SRCCOPY


End Sub


Sub win_menu(hwnd As HWND)
	

	  
			hmenu = CreateMenu()
			hFilesub = CreatePopupMenu()
			hViewsub = CreatePopupMenu()
			
			hFile_Recent_sub = CreatePopupMenu()
			hFile_VidRecord_sub = CreatePopupMenu()
			hFile_AudRecord_sub = CreatePopupMenu()
			hView_Zoom_sub = CreatePopupMenu()
			
			AppendMenu(hmenu,MF_POPUP,hFilesub,"&file")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_LOADROM,"&load rom")
				
					AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_RESET,"&Reset NES")
					AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_HARDRESET,"&Hard Reset NES")
					  AppendMenu(hFilesub,MF_UNCHECKED,IDM_FILE_PAUSE,"&Pause NES")
					  	AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SINGLEFRAME,"&Single Frame")
							AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_CYCLE,"&Cycle")
				AppendMenu(hFilesub,MF_SEPARATOR,0,"-")
				AppendMenu(hFilesub,MF_POPUP Or MF_GRAYED,hFile_Recent_sub,"&Recent roms") ' WIP
							AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
									AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
											AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
													AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
															AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
															
				AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")			
							
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SAVEPIC ,"&Save NES Screen")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_PATTABLE01 ,"&Save Pat table left")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_PATTABLE02 ,"&Save Pat table right")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_NAMETABLES ,"&Save Nametables")
				'AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_PATTABLE02 ,"&Save Palette")
				AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")
				
				AppendMenu(hFilesub,MF_POPUP Or MF_GRAYED,hFile_VidRecord_sub,"&Record Movie") ' WIP
				AppendMenu(hFilesub,MF_POPUP Or MF_GRAYED,hFile_AudRecord_sub,"&Record Audio") ' WIP
				
				AppendMenu(hFilesub,MF_UNCHECKED,IDM_FILE_MUTEAUDIO ,"&Mute Audio")
				  'AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SAVEPIC ,"&Save Frames")
				
													
			AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")
			AppendMenu(hFilesub,MF_POPUP,IDM_FILE_EXIT,"&Exit")
							
							
			AppendMenu(hmenu,MF_POPUP,hViewsub,"&View")				
							AppendMenu(hViewsub,MF_POPUP,hView_Zoom_sub,"&Zoom")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,103,"1x")
									AppendMenu(hView_Zoom_sub,MF_CHECKED,104,"2x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,105,"3x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,106,"4x")				
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED Or MF_GRAYED,107,"&Fullscreen")
				SetMenu(hwnd,hmenu)			
							
End Sub

Sub nesmain(hwnd As HWND,rct As rect)
	
'fElapsedTime = Timer-fElapsedstart
' fElapsedstart = timer
'
'
'		controller(0) = &H00 
'		controller(0) Or= IIf((GetAsyncKeyState(vk_X) And &H8000), &H80, &H00)    ' // A Button
'		controller(0) Or= IIf((GetAsyncKeyState(vk_Z) And &H8000), &H40, &H00)     ' // B Button
'		controller(0) Or= IIf((GetAsyncKeyState(vk_A) And &H8000), &H20, &H00)     ' // Select
'		controller(0) Or= IIf((GetAsyncKeyState(vk_S) And &H8000), &H10, &H00)     ' // Start
'		controller(0) Or= Iif((GetAsyncKeyState(vk_UP) And &H8000), &H08, &H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_DOWN) And &H8000), &H04, &H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_LEFT) And &H8000),&H02,&H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_RIGHT) And &H8000),&H01, &H00)
'		
'
'          'print pmapper->GetIrqReloadVal()
'				if (bEmulationRun) Then
'					
'					
'					
'				  
'					if (fResidualTime > 0.0f) Then
'						fResidualTime -= fElapsedTime
'					Else
'						fResidualTime += (1.0f / 60.0f) - fElapsedTime
'
'					Do:  bus_clock2:  Loop while Not(frame_complete) 
'					frame_complete = FALSE
'					frames_per_sec(hwnd)		
'					End If
'					
'					Else
'						
'					If bsingleframe = TRUE Then
'
'				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),0,1)
'				frame_complete = FALSE
'				bSingleFrame = FALSE
'				
'					end If
'
'			If  KEYPRESSED(vk_f)  Then
'				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),0,1)
'				frame_complete = FALSE
'			End if	
'				
'		If  KEYPRESSED(vk_space)  Then
'				Do:  bus_clock2:  Loop while IIf(complete(),0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),1,0)
'		End if	
'				EndIf
'				
'		If  KEYPRESSED(vk_r)  Then
'			
'			'if pmapper <> null then
'			''pmapper->setbattery ' load batter data
'			'end if
'			'
'
'			
'			 bus_reset(false)
'		End If
'				If  KEYPRESSED(vk_h)  Then
'			
'			'if pmapper <> null then
'			''pmapper->setbattery ' load batter data
'			'end if
'			'
'
'			
'			 bus_reset(true)
'				End If
'				
'				
'				
'		
'			'	If  KEYPRESSED(vk_b)  Then ' save battery data
'			'
'			'
'			'
'			''if pmapper <> null then
'			''		dim sram_save as integer = FreeFile
'			''
'			''open "kirbyadventure.batt" for binary as sram_save
'			''' 
'			'' put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
'			'''
'			''close sram_save
'			''end if
'			''
'
'			'
'			' pmapper->getbattery
'			' 
'			'	End If
'		'
'		'If  KEYPRESSED(vk_n)  Then ' load battery data
'		'	
'		'	
'		'	
'	
'		'	pmapper->setbattery
'		'	end if
'		'	
'
'		'			
'						
'						
'      If  KEYPRESSED(vk_e)  Then
'			 bEmulationRun = Not(bEmulationRun)
'				checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
'      End If
'		
'	
'		
'		
'	
'
'	
'	'fillrect(dc,@Type<rect>(640-64,0,640,480),Cast(HBRUSH,GetStockObject(WHITE_BRUSH)))
'		
'	'fillrect(dc,@Type<rect>(640-128,0,640,480),Cast(HBRUSH,GetStockObject(WHITE_BRUSH)))
'
'
'	'fill_the_rect(dc,384,0,0,1)
'	'fill_the_rect(dc,384+128,0,0,2)
'	'
'	'my_pattables = GetPatternTable(0,0) 
'	'DrawPatTable(dc,@my_pattables,384,480-128,128,128)
'	'my_pattables = GetPatternTable(1,0) 
'	'DrawPatTable(dc,@my_pattables,384+128,480-128,128,128)
'	
'	
'		Dim As hdc dc
'	dc = GetDC(hwnd)
''	
''	
''		Dim posx As integer
''	Dim posy As integer
''	Dim pal1 As uint8_t
''	pal1 = 0
''		For p As Integer = 0 To 4-1
''		
''		
''		For s As Integer = 0 To 4-1
''			
''		
''			
''			'fill_pal_rect(dc,(256+10)+p*(14*5),(480-128)+0*14,s,p)
''			fill_pal_rect(dc,266 + p * (12 * 4) + s * 12,(480-128)+0*14,p,s)
''fill_pal_rect(dc,266 + p * (12 * 4) + s * 12,(480-128)+1*14,p+4,s)
''
''
''		Next
''		Next
''	
''	my_pattables = GetPatternTable(0,0) 
''	DrawPatTable(dc,@my_pattables,0,480-128,128,128)
''	my_pattables = GetPatternTable(1,0) 
''	DrawPatTable(dc,@my_pattables,132,480-128,128,128)
''	
'	
'	
'	
'	
'	
'	'fill_the_rect(dc,512+16,0,0,1)
'	'fill_the_rect(dc,512+32,0,0,2)
'	'fill_the_rect(dc,512+48,0,0,3)
'	'
'	'fill_the_rect(dc,512+24,0,1,0)
'	'fill_the_rect(dc,512+30,0,1,1)
'	'fill_the_rect(dc,512+36,0,1,2)
'	'fill_the_rect(dc,512+42,0,1,3)
'	'
'	'fill_the_rect(dc,512+48,0,2,0)
'	'fill_the_rect(dc,512+54,0,2,1)
'	'fill_the_rect(dc,512+60,0,2,2)
'	'fill_the_rect(dc,512+66,0,2,3)
'	'
'	'
'	'fill_the_rect(dc,512+72,0,3,0)
'	'fill_the_rect(dc,512+78,0,3,1)
'	'fill_the_rect(dc,512+84,0,3,2)
'	'fill_the_rect(dc,512+90,0,3,3)
'	'
'	'
'	'
'	'fill_the_rect(dc,512,16,4,0)
'	'fill_the_rect(dc,512+16,16,4,1)
'	'fill_the_rect(dc,512+32,16,4,2)
'	'fill_the_rect(dc,512+48,16,4,3)
'	'
'	'fill_the_rect(dc,512+64,16,5,0)
'	'fill_the_rect(dc,512+70,16,5,1)
'	'fill_the_rect(dc,512+86,16,5,2)
'	'fill_the_rect(dc,512+102,16,5,3)
'	'
'	'fill_the_rect(dc,512+118,16,6,0)
'	'fill_the_rect(dc,512+134,16,6,1)
'	'fill_the_rect(dc,512+150,16,6,2)
'	'fill_the_rect(dc,512+166,16,6,3)
'	'
'	'
'	'fill_the_rect(dc,512+182,16,7,0)
'	'fill_the_rect(dc,512+198,16,7,1)
'	'fill_the_rect(dc,512+204,16,7,2)
'	'fill_the_rect(dc,512+220,16,7,3)
'	'
'	
'	'
'	'fillrect(dc,@Type<rect>((640-128)+posx,0+posy,((640-128)+6)+posx,6+posy),createsolidbrush(GetColourFromPaletteRamBGR(0,2)))
'		'fillrect(dc,@Type<rect>((640-128)+posx+6,0+posy,((640-128)+6)+posx+6,6+posy),createsolidbrush(GetColourFromPaletteRamBGR(0,1)))
'
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	
'	
'	if bfullscreen then
'DrawNesScrn(dc,mydata(),((0+128)-64),0,rct.right,rct.bottom)
'else
'DrawNesScrn(dc,mydata(),0,0,rct.right,rct.bottom)
'	end if
'
'
'
'
'
'
'
'	ReleaseDC(hwnd,dc)
'	
'
'	
	
	
End Sub

Sub nesmain_with_audio(hwnd As HWND,rct As rect)
fElapsedTime = Timer-fElapsedstart
 fElapsedstart = timer


     '	 start1 =  SDL_GetPerformanceCounter() 
     
		nes.controller(0) = &H00 
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_X) And &H8000), &H80, &H00)    ' // A Button
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_Z) And &H8000), &H40, &H00)     ' // B Button
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_A) And &H8000), &H20, &H00)     ' // Select
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_S) And &H8000), &H10, &H00)     ' // Start
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_UP) And &H8000), &H08, &H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_DOWN) And &H8000), &H04, &H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_LEFT) And &H8000),&H02,&H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_RIGHT) And &H8000),&H01, &H00)


	'screensync()
	
	If  bEmulationRun then
			 If (fResidualTime > 0.0f) Then
				 		fResidualTime -= fElapsedTime
				 	Else
					 	fResidualTime += (1.0f / 60.0f) - fElapsedTime
						
						
						
					'pmapper->GetBattery()	
						
					Do   
						nes._clock()  
					Loop while not(iif(nes.ppu._line = 240 and nes.ppu.dot = 0,true,false))
					nes.ppu.SetFrame()
					
					' frmenums +=1
				'	 SaveNESScrn("C:\nes_frames\batman_returns_"& str(frmenums)&".bmp",256,240,@nes.ppu._mydata(0))
			   '	 Do:  nes._clock():  Loop while Not(frame_complete) 
					
	
						nes.getsamples(audio_hndler->sampleBuffer(), audio_hndler->samplesPerFrame)
					   audio_hndler->nextBuffer()
			 
					 'frame_complete = FALSE
					frames_per_sec(hwnd)		
	End If
			
	'else
					
	 										
					
	 		
					
	End if	
	
	'if bfullscreen then
	'	   DrawNesScrn(dc,nes.ppu._mydata(),((0+128)-64),0,rct.right,rct.bottom)
  ''DrawNesScrn(dc,mydata(),((0+128)-64),0,rct.right,rct.bottom)
	'else
	 '	  DrawNesScrn(dc,nes.ppu._mydata(),0,0,rct.right,rct.bottom)
  ''DrawNesScrn(dc,mydata(),0,0,rct.right,rct.bottom)
	'end if

	'ReleaseDC(hwnd,dc)



		
		
			If  KEYPRESSED(vk_space)  Then
	'bEmulationRun = Not(bEmulationRun)
	'						checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
	'				if bEmulationRun = false then
	'					
	'					audio_hndler->stop_aud
	'				else
	'					audio_hndler->start_aud
	'				EndIf
	'		EndIf
    	emurun = Not(emurun)
				bEmulationRun = emurun
							checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(emurun,MF_UNCHECKED,MF_CHECKED))
					if bEmulationRun = false then
						
						audio_hndler->stop_aud
					else
						audio_hndler->start_aud
					EndIf
			EndIf
			
			
			
			
						If  KEYPRESSED(vk_c)  Then
	'bEmulationRun = Not(bEmulationRun)
	'						checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
	'				if bEmulationRun = false then
	'					
	'					audio_hndler->stop_aud
	'				else
	'					audio_hndler->start_aud
	'				EndIf
	 'nes._clock()
	' Do:  nes._clock():  Loop while IIf(nes.cpu.complete() = 0,1,0) 
	
	 ' Do:  nes._clock():  Loop while IIf(nes.cpu.complete(),1,0) 
	
	'Do:  nes._clock():  Loop while  iif((nes.cpu.getcycles() = 0 and (nes.inDma = false) and (nes.cpu.getcycles() mod 3) = 0)= 0,true,false) 
	
			
'				Do:  bus_clock2:  Loop while iif(complete(),1,0)
	
	
	
	
	  Do:  nes._clock():  Loop while IIf(nes.cpu.complete(),false,true)

	
	
			EndIf
									If  KEYPRESSED(vk_f)  Then
					Do   
						nes._clock()  
					Loop while not(iif(nes.ppu._line = 240 and nes.ppu.dot = 0,true,false))
					nes.ppu.SetFrame()
					'Do:  nes._clock():  Loop while IIf(nes.cpu.complete(),0,1)

		
	
									EndIf
									
									
					If  KEYPRESSED(vk_r)  Then
			
			'if pmapper <> null then
			''pmapper->setbattery ' load batter data
			'end if
			'
 nes._reset(false)
			
			' bus_reset(false)
		End If
				If  KEYPRESSED(vk_h)  Then
			
			'if pmapper <> null then
			''pmapper->setbattery ' load batter data
			'end if
			'
           nes._reset(true)
			
			 'bus_reset(true)
				End If
	
	   ' // draw palette

	
	
	
	
		Dim As hdc dc
	dc = GetDC(hwnd)
	
	
	'fill_pal_rect2(dc,0, 0, 16, 16) 
	
   ' for  i as uint8_t = 0 to 16-1
   '  dim _col as uint32_t =  nes.ppu.palScreen( nes.ppu.readPalette(i) and &H3f)
   ''  ctx.fillStyle = `rgba(${col[0]}, ${col[1]}, ${col[2]}, 1)`;
   '   fill_pal_rect2(dc,i * 16, 0, 16, 16,_col) 
   '  _col =  nes.ppu.palScreen( nes.ppu.readPalette(i + 16) and &H3f)
   ' ' ctx.fillStyle = `rgba(${col[0]}, ${col[1]}, ${col[2]}, 1)`;
   '   fill_pal_rect2(dc,i * 16, 16, 16, 16,_col) 
   ' next
	
	 'ScreenSync
	'Wait &h3da, &h8
	
	if bfullscreen then
		'screensync()
		   DrawNesScrn(dc,nes.ppu._mydata(),((0+128)-64),0,rct.right,rct.bottom)
  'DrawNesScrn(dc,mydata(),((0+128)-64),0,rct.right,rct.bottom)
	else
		'screensync()
 	 	  DrawNesScrn(dc,nes.ppu._mydata(),0,0,rct.right,rct.bottom)
  'DrawNesScrn(dc,mydata(),0,0,rct.right,rct.bottom)
	end if


 '  setbkMode(dc,TRANSPARENT)
 ' '   settextcolor(dc,RGB(255,255,255))
 '' textoutA(dc,0,0,"state saved",len("state saved"))
 '
 'settextcolor(dc,BGR(0,0,0))
 ' textoutA(dc,0,0,"state saved",len("state saved"))
 ' 
 ' settextcolor(dc,BGR(255,255,255))
 ' textoutA(dc,0,0,"state saved",len("state saved"))



	ReleaseDC(hwnd,dc)

'do
'		QueryPerformanceCounter(CPtr(Any Ptr, @curtimer))
'					'curtimer = SDL_GetTicks()
'
'		timertemp = Abs(Cast(ULongInt, curtimer) - Cast(ULongInt, lasttimer))
'	
'    	If timertemp >= Cast(ULongInt, timerfreq / 60) Then  exit do
''
'loop
'     	
'     	lasttimer = curtimer
'
'
'





	
'SwitchToThread()


'
	 
'	 cls
' ' drawcpu(0,0)
' 'print Getmapper()->lastread''mirroring
''print Getmapper()->prgand
' 'print nes.ppu.spritecount
' 
' 
' drawOAM(0,0)
' 
' 'print nes.ppu.paletteRam(2)
' 
'   'drawcode(2,8,11)
'	  PCopy
'	
' end1 	= SDL_GetPerformanceCounter()

    ' dim elapsed  as float = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency())
    '   elapsedMS    = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency()) * 1000.0f

 '     SDL_Delay(int(16.666f - elapsedMS))
	
	
End Sub








Function myloadbmp(dc As hdc, path As String,mydata1 As ULong ptr = 0) As BOOLEAN
	
Dim databuf(1) As UByte Ptr 
Dim pixels As UByte Ptr
	
Dim bmpheader As BITMAPFILEHEADER Ptr
Dim bmpinfo As BITMAPINFOHEADER Ptr
Dim pal_4bit(0 To 15) As RGBQUAD 
Dim pal_8bit(0 To 255) As RGBQUAD
	
Dim As Integer bmpfile1 = FreeFile 
 
Open path For Binary As bmpfile1
  

databuf(0) = New UByte[SizeOf(BITMAPFILEHEADER)]
databuf(1) = New UByte[SizeOf(BITMAPINFOHEADER)]
 
 
Get #bmpfile1,,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
Get #bmpfile1,,databuf(1)[0],SizeOf(BITMAPINFOHEADER) 

bmpheader = Cast(BITMAPFILEHEADER Ptr,databuf(0))
bmpinfo  = Cast(BITMAPINFOHEADER Ptr,databuf(1))

If bmpheader->bfType <> &H4D42 Then
Print "invalid bitmap"
EndIf

pixels = New UByte[bmpInfo->biSizeImage]


'Uint8 tmpRGB = 0; // Swap buffer
'	for (unsigned long i = 0; i < bmpInfo->biSizeImage; i += 3)
'	{
'		tmpRGB        = pixels[i];
'		pixels[i]     = pixels[i + 2];
'		pixels[i + 2] = tmpRGB;
'	}

'If bmpinfo->biBitCount = 4 Then
'	Get #bmpfile1,,pal1(0),16
'Else
'	Seek #bmpfile1,bmpheader->bfOffBits
'End If

'Print Seek(bmpheader)
'Seek #bmpfile1,bmpheader->bfOffBits
'Sleep


Get #bmpfile1,,*pixels,bmpInfo->biSizeImage 

	Close bmpfile1


StretchDIBits dc,0,0,bmpinfo->biWidth,bmpinfo->biHeight,0,0,bmpinfo->biWidth,bmpinfo->biHeight,@pixels[0],bmpinfo,DIB_RGB_COLORS,SRCCOPY



	Delete[] databuf(0)
	Delete[] databuf(1)
	Delete[] pixels 
	
	databuf(0) = NULL
	databuf(1) = NULL

Return TRUE
			
End Function

Function mysavebmp(path As String,mydata1 As ULong ptr = 0) As BOOLEAN
	
   
Dim databuf(1) As UByte Ptr 
Dim pixels As UByte Ptr
	
Dim bmpheader As BITMAPFILEHEADER Ptr
Dim bmpinfo As BITMAPINFOHEADER Ptr
Dim pal_4bit(0 To 15) As RGBQUAD 
Dim pal_8bit(0 To 255) As RGBQUAD
	
Dim As Integer bmpfile1 = FreeFile 
 
Open path For Binary As bmpfile1
  

databuf(0) = New UByte[SizeOf(BITMAPFILEHEADER)]
databuf(1) = New UByte[SizeOf(BITMAPINFOHEADER)]
 

Get #bmpfile1,,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
Get #bmpfile1,,databuf(1)[0],SizeOf(BITMAPINFOHEADER) 

bmpheader = Cast(BITMAPFILEHEADER Ptr,databuf(0))
bmpinfo  = Cast(BITMAPINFOHEADER Ptr,databuf(1))

If bmpheader->bfType <> &H4D42 Then
Print "invalid bitmap"
EndIf

pixels = New UByte[bmpInfo->biSizeImage]


'Uint8 tmpRGB = 0; // Swap buffer
'	for (unsigned long i = 0; i < bmpInfo->biSizeImage; i += 3)
'	{
'		tmpRGB        = pixels[i];
'		pixels[i]     = pixels[i + 2];
'		pixels[i + 2] = tmpRGB;
'	}

'If bmpinfo->biBitCount = 4 Then
'	Get #bmpfile1,,pal1(0),16
'Else
'	Seek #bmpfile1,bmpheader->bfOffBits
'End If

'Print Seek(bmpheader)
'Seek #bmpfile1,bmpheader->bfOffBits
'Sleep


Get #bmpfile1,,*pixels,bmpInfo->biSizeImage 

	Close bmpfile1
			
			
	
	Dim savebmpfile As Integer = FreeFile
	
	
		   If FileExists("C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\testrewrite.bmp") Then
   	Kill("C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\testrewrite.bmp")
		   End If
			
	Open "testrewrite.bmp" For Binary As savebmpfile
	
			
		Put # savebmpfile, ,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
		
		
		Put 	# savebmpfile,,databuf(1)[0],SizeOf(BITMAPINFOHEADER)
		
		Put # savebmpfile,,pixels[0],bmpInfo->biSizeImage
			
			
			
			
			
		
	Delete[] databuf(0)
	Delete[] databuf(1)
	Delete[] pixels 
	
	databuf(0) = NULL
	databuf(1) = NULL	
	'pixels  = NULL		
End Function

Sub free_rom()

	erase(mydata)

	bat_saves = 0

	
	
	if pmapper <> null then
	pmapper->getbattery()	
		
		
	delete pmapper
	pmapper = null
	end if
	
	cls
	
	'bus_reset
	
	
	
End Sub

Function WndProc(hWnd As HWND, msg As  UINT, wParam As WPARAM, lParam As LPARAM) As Integer
 
	   
	  Dim hdc As HDC
	  Dim ps As PAINTSTRUCT
	  Dim rct As RECT
	
	Select Case ( Msg )
	
	
	case WM_ACTIVATE
		select case wParam
			
			case WA_INACTIVE
			 audio_hndler->stop_aud
			 keysenable = FALSE
			'audio_hndler->stop_aud
          emurun = bEmulationRun
          bEmulationRun = false
			case else
				 keysenable = TRUE
					 'If muted = false and bEmulationRun = true then
						'	audio_hndler->start_aud 
						'
					''	else
						'audio_hndler->stop_aud 
						'	end if
						
							  If muted = false and emurun = true then
							audio_hndler->start_aud 
						
						else
						audio_hndler->stop_aud 
							end if
						
            bEmulationRun = emurun
			
		End Select
	
	
					case WM_KEYDOWN':								'	// If we pressed a key
			Select Case (wParam) 							'	// Check what the wParam holds.  The wParam holds which key we pressed.
				 
				case VK_ESCAPE':							'	// If we pressed the ESCAPE key.
					if(bFullScreen) Then						'	// If we went to full screen mode
					 										'// Calling the same function with NULL and 0 reset the screen settings and resolution
						ChangeDisplaySettings(NULL, 0)';
						showcursor 1
					End If
					SendMessage(hwnd, WM_CLOSE, 0, 0)';		'// Close the program
					'break;
			End Select
			
			
		case WM_CREATE

		Case WM_SIZE
			
			Return 0		
			
	Case WM_MOVING
	'audio_hndler->stop_aud
				 audio_hndler->stop_aud
			 
			'audio_hndler->stop_aud
          emurun = bEmulationRun
          bEmulationRun = false
	
	
	
	 
	case WM_EXITSIZEMOVE
			 'If muted = false and bEmulationRun = true then
							'audio_hndler->start_aud 
					'	
					'	else
					'	audio_hndler->stop_aud 
							'		 end if
   							  If muted = false and emurun = true then
							audio_hndler->start_aud 
						
						else
						audio_hndler->stop_aud 
							end if
						
            bEmulationRun = emurun
		   		
		case WM_QUIT	
			
		Case WM_CLOSE
		audio_hndler->stop_aud 
		
					If(bFullScreen) Then						'	// If we went to full screen mode
					 										'// Calling the same function with NULL and 0 reset the screen settings and resolution
						ChangeDisplaySettings(NULL, 0)';
					End If
					

							
						
				 
	 
		       'if pmapper <> null then
		       ' pmapper->getbattery()	
		       'EndIf
				 
		
				'	delete pmapper
					'pmapper = null
				 	free_rom	
					
					
				 '	threadwait(thread1)
					 'quit = true
			DestroyWindow(hWnd)
			
				Case WM_SIZE
			
			'
		Case WM_DESTROY
			'ImageDestroy nesscrn
         'ImageDestroy pattables(0)
         'ImageDestroy pattables(1)
       
			'pmapper->getbattery()
			PostQuitMessage(NULL)
		   ExitProcess(0)
         End	
         
         

		Case WM_MOUSEMOVE

		Case WM_SIZING

		Case WM_ERASEBKGND
		
		hdc = getdc(hwnd)
		fillrect(hdc,@Type<rect>(0,0,640,480),Cast(HBRUSH,GetStockObject(BLACK_BRUSH)))
		releasedc(hwnd,hdc)
			Return  1 
			
			case WM_INITMENU
         
			audio_hndler->stop_aud
		 
			
	   case WM_EXITMENULOOP
					
							 If muted = false and bEmulationRun = true then
							audio_hndler->start_aud 
						
						else
						audio_hndler->stop_aud 
							end if
   
		 
		Case WM_COMMAND
		
		

			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					
					Select Case LoWord(wParam)
						
						
						
						
						
						
						Case IDM_FILE_LOADROM
							'cart = ComDlgFileName(hwnd, "Open Source File", "C:\", "*.NES|*.NES", OFN_FORCESHOWHIDDEN)
			dim sram_save as integer = FreeFile
			
			'open "kirbyadventure.batt" for binary as sram_save
			' 
			' put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
			'
			'close sram_save
			       emurun = bEmulationRun
                bEmulationRun = false
		            
								cart = ""
	                     if vCHRMemory.clear then
	                    '	print"erased"
	                     End If
	                     
	                     if vPRGMemory.clear then
	                    ' print"erased"
	                     end if
	                   '  print pmapper->vRAMStatic.size()
	                     
	                       vCHRMemory.resize(0)
	                       vPRGMemory.resize(0)
	                    'sleep
							free_rom
							init_rom
					     bEmulationRun = emurun
							'pmapper->getbattery()
							'sleep
							 
							  'SendMessage(hWnd,WM_CLOSE,0,0)
							
							
							
						Case IDM_FILE_SAVEPIC
							
							pic1 = file_opensave(hwnd,FALSE)
								
		   If FileExists(pic1) Then
   	Kill(pic1)
		   End If
							'Print pic1
							'SaveNESScrn(pic1,256,240,@mydata(0))
						'SaveNESScrn(pic1,256,240,@nes.ppu._mydata(0))
						
						
						Case IDM_FILE_PATTABLE01
											pic1 = file_opensave(hwnd,FALSE)
								
		   If FileExists(pic1) Then
   	Kill(pic1)
		   End If
		   
		   drawPatternsPals()
		   	SaveNESScrn(pic1,512,480,@pat_imgdata(0))
		   	
							'Print pic1
							'SaveNESScrn(pic1,128,128,@mydata_pattables(0).mydata(0))
						Case IDM_FILE_PATTABLE02
											pic1 = file_opensave(hwnd,FALSE)
								
		   If FileExists(pic1) Then
   	Kill(pic1)
		   End If
		   
		   drawPatternsPals()
		   	SaveNESScrn(pic1,512,480,@pat_imgdata(0))
							
						Case IDM_FILE_NAMETABLES
							   pic1 = file_opensave(hwnd,FALSE)
		   					If FileExists(pic1) Then
		   						Kill(pic1)
							End If
							drawNametables()
							
							SaveNESScrn(pic1,512,480,@nme_imgdata(0))
							
						Case IDM_FILE_EXIT
							
							
							
							
							
							SendMessage(hWnd,WM_CLOSE,0,0)
							
							
						Case IDM_FILE_MUTEAUDIO

						muted = not(muted)
						checkmenuitem(hFilesub,IDM_FILE_MUTEAUDIO ,IIf(muted,MF_CHECKED,MF_UNCHECKED))
						
							 If muted = false then
							 	
							audio_hndler->start_aud 
						 
						else
						audio_hndler->stop_aud
						end if 
						

							
						Case IDM_FILE_RESET
							'bus_reset(false)
							nes._reset(false)
						Case IDM_FILE_HARDRESET
							nes._reset(true)
							
							
						Case IDM_FILE_SINGLEFRAME
							bSingleFrame = TRUE
						Case IDM_FILE_CYCLE
				'			
				'Do:  bus_clock2:  Loop while IIf(complete(),0,1)
				'Do:  bus_clock2:  Loop while iif(complete(),1,0)
				  nes._clock()
				
		
						Case IDM_ZOOM_1X
							
					
								zoom_scrn = 1
									nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
						
						Case IDM_ZOOM_2X
							
							zoom_scrn = 2
							nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
							
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
			
						Case IDM_ZOOM_3X
							
							zoom_scrn = 3
								nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
	   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
						
						Case IDM_ZOOM_4X
							zoom_scrn = 4
							nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
		   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
		
					   Case IDM_ZOOM_FULLSCREEN
								
		   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_CHECKED)
			
			ChangeToFullScreen(SCREEN_WIDTH, SCREEN_HEIGHT)
			
			ShowCursor 0
			
			'ShowWindow(hWnd, SW_HIDE)
			'UpdateWindow(hWnd)
			
			

							
						Case IDM_FILE_PAUSE
				
					'		bEmulationRun = Not(bEmulationRun)
					'		checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
					'if bEmulationRun = false then
					'	
					'	audio_hndler->stop_aud
					'else
					'	audio_hndler->start_aud
					'	
					'EndIf
				
				  		emurun = Not(emurun)
				  		bEmulationRun = emurun
							checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(emurun,MF_UNCHECKED,MF_CHECKED))
					if emurun= false then
						
						audio_hndler->stop_aud
					else
						audio_hndler->start_aud
						
					EndIf
				
				 	'bEmulationRun = Not(bEmulationRun)
					'		checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
					'if bEmulationRun = false then
					'	
					'	audio_hndler->stop_aud
					'else
					'	audio_hndler->start_aud
					'EndIf
							
					End Select
         End Select
		Case Else 
		
		return DefWindowProc(hWnd, Msg, wParam, lParam) 
 
	End Select
	
	Return 0
	
End Function

Function WinMain(hInstance As HINSTANCE, hPrevInstance As HINSTANCE, lpCmdLine As LPSTR, nShowCmd As Integer) As Integer
  Dim As WNDCLASSEX wcex
  Dim As MSG msg
  Dim As string classname="OLC-NES-NESTEA-FB"
  Dim As HANDLE hIconLib
  
  Dim bdone As bool = FALSE
  Dim screenSize  As SIZE
  Dim As LONG winX, winY 

	ZeroMemory(@wcex, sizeof(WNDCLASSEX)) 
	ZeroMemory(@msg, sizeof(MSG)) 

	wcex.cbSize = sizeof(WNDCLASSEX) 
	wcex.hbrBackground = Cast(HBRUSH,GetStockObject(WHITE_BRUSH)) 
	wcex.hCursor = LoadCursor(hInstance, IDC_ARROW) 
	wcex.hIcon   = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1)) 
	wcex.hInstance = hInstance 
	wcex.lpfnWndProc = @WndProc 
	wcex.lpszClassName =  StrPtr(classname) 
	wcex.style = CS_HREDRAW Or CS_VREDRAW 
	wcex.cbWndExtra = 0 
	wcex.cbClsExtra = 0 
	wcex.lpszMenuName = NULL 

	if ((RegisterClassEx(@wcex))) = 0 then  
		return -1 
	End If
	
	
 Dim winRect As RECT => (0,0,256*zoom_scrn,240*zoom_scrn)


If bfullscreen = FALSE Then
 AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)

hwnd = CreateWindowEx( _
		NULL, _
		 wcex.lpszClassName, _
		"OLC-NES-NESTEA-FB-V0.1", _
			WS_OVERLAPPED Or WS_SYSMENU Or WS_CAPTION or WS_MINIMIZEBOX  or WS_CLIPCHILDREN or WS_CLIPSIBLINGS, _
		300,300, _
		winrect.right-winrect.left, _
		winrect.bottom-winrect.top, _
		0, _
		0, _
		hInstance, _
		0) 


win_menu(hwnd)
Else
		
		ChangeToFullScreen(SCREEN_WIDTH, SCREEN_HEIGHT)
			
			ShowCursor 0
	'dwStyle = WS_POPUP or WS_CLIPSIBLINGS Or WS_CLIPCHILDREN
	    hwnd = CreateWindow (StrPtr(classname), _		'			// window class name 
						 "Full Screen App", _		  		'	// window's Title    
						 WS_POPUP or WS_CLIPSIBLINGS Or WS_CLIPCHILDREN, _						'	// window style		 
						 0, _								'	// initial x position
						 0, _									'// initial y position
						 SCREEN_WIDTH, _						'// initial x size - Our resolution width
						 SCREEN_HEIGHT, _					'    // initial y size - Our resolution height	 
						 NULL, _								'// Pass NULL for the parent window
						 NULL, _								'// Pass NULL for a menu
						 hInstance, _						 '   // Pass in our hInstance
						 NULL) 							'	// Pass NULL to the wndProc, we don't want to pass it any variables address's.


'enableWindow(hwnd,true)		
'SetActiveWindow(hwnd)

End If

'nes.cpu.pc = &Hfdb7
ShowWindow(hWnd, SW_SHOW)
UpdateWindow(hWnd)
SetActiveWindow(hwnd)

'Dim As hdc dc = getdc(hwnd)
'
'	myloadbmp(dc,"redtest.bmp")
'releasedc(hwnd,dc)

'mysavebmp("C:\Users\Gamer\Desktop\OLC-FREEBASIC-NESTEA--main\imagetest1.bmp")

audio_hndler->start_aud


map = nes.cpu.disassemble(&H0000, &HFFFF)

  Dim As ULongInt curtimer, lasttimer, timerfreq 
  
Dim timertemp As ULongInt

'QueryPerformanceFrequency(CPtr(Any Ptr, @timerfreq))


		'dim sram_save as integer = FreeFile
		'	
		'	open "EARTH.batt" for binary as #1
		'	 
		''	 put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
		'	
		'	close #1
		
		


PCopy
'thread1 =  threadcreate(@debug1)
	while (bDone = FALSE)  

		if (PeekMessage(@msg, NULL, 0, 0, PM_REMOVE)) then 
			TranslateMessage(@msg) 
			DispatchMessage(@msg) 

			if (msg.message = WM_QUIT)  Then
				bDone = TRUE 
			End If
		 
		Else
'Dim As hdc dc = getdc(hwnd)
	'myloadbmp(dc,"C:\Users\Gamer\Desktop\OLC-FREEBASIC-NESTEA--main\index.bmp")
'releasedc(hwnd,dc)
  'cls
  

  	'nesmain(hWnd,NesRect)
	nesmain_with_audio(hWnd,NesRect)
	
	
		End If
  
	Wend
	
	ShowCursor 1
	
	Return msg.wParam 

	'DestroyWindow( hWnd )
	'UnregisterClass(wcex.lpszClassName, hInstance)
'	Return 0
	
End Function

Sub init() 
	
'Dim bmp As String
'
'Dim s As String
'
'Print "choose a rom"
'
'While cart = ""
'	
'	
'	cart = file_opensave(NULL)
'	If cart = "" Then 
'	Print "invalid rom file"
'		
'		
'	EndIf
'Wend
'
''Print bmp
''	bmp = file_opensave(NULL)
'	'Print bmp
'	
'	
'
'setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
''Print cart
''Input   "enter rom path or drag file here: ", cart
'insert_cartridge(cart)
'setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
'Input   "fullscreen Y/N?: ", s 
'Cls
'
'
'
'If s  = "Y" Or s  = "y" Then
'	
'	bfullscreen = TRUE
'	'FreeConsole()
'
'ElseIf s  = "N" Or s  = "n" Then
'
'bfullscreen = FALSE
'EndIf
'		If ImageValid = TRUE Then
'	Print "CRC: " & "???? WORK IN PROGRESS"
'	Print "loaded rom!"
'	Print "MAGIC NUM: "; header.Name1
'	Print "MIRROR MODE: "; IIf(cart_mirror = horizontal,"HORIZONTAL","VERTICAL")
'	Print "ROM BANKS: " & Str(header.prg_rom_chunks)
'	Print "PRG ROM SIZE: " & Str(vPRGMemory.size())
'	Print "CHRROM BANKS: " & str(header.chr_rom_chunks)
'	Print "CHRROM SIZE:" & Str(vCHRMemory.size())
'	Print "MAPPER: " & Str(nmapperid)
'	Print "PRGRAM SIZE: " & Str(header.prg_ram_size)
'	Print header.tv_system1
'	Print header.tv_system2
'	Print header.unused
'	Print 
'	else
'	Print "rom not found or isnt valid"
'	Print 
'	Print  "press Any key To Quit"
'	
'		Sleep
'		End
'		EndIf
'
''insert_cartridge("roms/kung fu.nes")
''insert_cartridge("donkey.nes")
''insert_cartridge("Burger Time (U) [!].nes")
''insert_cartridge("Felix the Cat (U).nes")
''insert_cartridge("Felix the Cat (U).nes")
''insert_cartridge("roms/Kirby's Adventure (USA).nes")
''insert_cartridge("roms/Volleyball.nes")
''insert_cartridge("roms/SMB3.nes")
''insert_cartridge("roms/SMB2.nes")
''insert_cartridge("Ren & Stimpy Show, The (U).nes")
''insert_cartridge("California Games (U).nes")
''insert_cartridge("roms/SMB.nes")
''insert_cartridge("ice climber.nes")
''insert_cartridge("roms/Megaman1.nes")
'
''init_rom
'
'nes.cpu.disassemble(&H0000, &HFFFF) '&H35
'
'bus_reset()
Color 255
Print "			 WELCOME TO ";
Color 60
Print "NESTEA V0.1!"
Color 255
Print " "
Print "please note this emulator is for EDUCATIONAL and FUN purposes"
Print "and it's a working in progress, such audio bugs.It can only play 1 player for now with mapper 0,1,2,3,66 and 4 games"
Print "mapper 4 is incomplete i got the irq working but might need some fixing still. 
Print "should be able to run some mapper 4 games with it."

Print " "
Print "the controls are:"
Print "X - A"
Print "Z - B"
Print "A - SELECT"
Print "S - START"
Print" "
Print "press any key to continue"


Sleep
Cls
init_rom


End Sub

Function ChangeToFullScreen(width1 As Integer ,height1 As  Integer ) As BOOL
 
	dim dmSettings As DEVMODE								'	// Device Mode variable - Needed to change modes

	memset(@dmSettings,0,sizeof(dmSettings)) 			'// Makes Sure Memory's Cleared

	'// Get the current display settings.  This function fills our the settings.
	if(EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,@dmSettings) = 0) Then
	 
		'// Display error message if we couldn't get display settings
		MessageBox(NULL, "Could Not Enum Display Settings", "Error", MB_OK) 
		Return FALSE
	End If

	dmSettings.dmPelsWidth	= Width1';					// Set the desired Screen Width
	dmSettings.dmPelsHeight	= height1';					// Set the desired Screen Height
	dmSettings.dmFields = DM_PELSWIDTH Or DM_PELSHEIGHT';	// Set the flags saying we're changing the Screen Width and Height
	
	'// This function actually changes the screen to full screen
'	// CDS_FULLSCREEN Gets Rid Of Start Bar.
'	// We always want to get a result from this function to check if we failed
	dim result As Integer = ChangeDisplaySettings(@dmSettings,CDS_FULLSCREEN)';	

	'// Check if we didn't receive a good return message From the function
	if(result <> DISP_CHANGE_SUCCESSFUL) Then
 
	'	// Display the error message and quit the program
		MessageBox(NULL, "Display Mode Not Compatible", "Error", MB_OK) 
		PostQuitMessage(0) 

	End If
End Function

Sub palrect_pos(x1 As Integer,y1 As Integer)
	
	
	'For y As Integer = 0 To 1
	'	
	'	
	'	For x As Integer
	'		
	'		fill_pal_rect(dc,256*1.5,y1,0,0)
	'		
	'		
	'	Next
	'Next
	
	
	'
	'fill_pal_rect(dc,(256*1.5)+12,y1,0,1)
	'fill_pal_rect(dc,(256*1.5)+24,y1,0,2)
	

	
	
End Sub





init



 
WinMain(GetModuleHandle(NULL), NULL, COMMAND(), SW_NORMAL)
				


Sub audioloop()
 dim _apu as APU ptr
'audio_hndler = new AudioHandler(48000,AUDIO_S16SYS,2048,16)
'audio_hndler->start_aud()
'audio_hndler->audio_spec.callback = NULL

'dim frame1 as bool ptr
'
'frame1 = @frame_complete
'_apu = @nes.apu


	'do
	'
	'
	'
	'if frame_complete <> true then
	''
	'end if	
	'
	'
	'
	'loop	
	'_apu.ConnectBus(@businst)
	
	do 
	'
	'if frame1  = false then
	'_apu->apu_cycle
	'end if	
	'
	'nes.apu.apu_cycle
	'end if

	Loop until quit = true
	'threadwait(thread1)
	
End Sub


  
Sub debug1()
	
 	screen ,0,1
'
'width 64,27
	do
		
			cls
			
		'print hex(nes._read(&H4015),2)	
		'print hex(nes._read(0),2)	
		'print hex(nes._read(1),2)
		'print hex(nes._read(2),2)
		'print hex(nes._read(3),2)
		'print hex(nes._read(4),2)
		'print hex(nes._read(5),2)
		'print hex(nes._read(6),2)	
		'print hex(nes._read(7),2)
		'print hex(nes._read(8),2)
		'print hex(nes._read(9),2)
		'print hex(nes._read(10),2)
		'print hex(nes._read(11),2)
		'print hex(nes._read(12),2)
		'print hex(nes._read(13),2)
		'print hex(nes._read(14),2)
		'print hex(nes._read(15),2)
		
		drawram(0,14,&H00,1,16)	
	  drawram(0,16,&Hd0,1,16)	
	  drawram(0,18,&H1f0,1,16)
	  
	   drawram(0,20,&H8000,1,16)
	   	
    drawcpu(0,0,CPU_CHOOSE)
   drawcode(2,8,4,CPU_CHOOSE)
  
     
  
  'print Getmapper()->prgbank
 'print Getmapper()->lastread''mirroring
'print Getmapper()->prgand
 'print nes.ppu.spritecount
 
 
 'drawOAM(0,0)
 
 'print nes.ppu.paletteRam(2)
 

	PCopy
	Loop until quit = true
	
	
	
End Sub






