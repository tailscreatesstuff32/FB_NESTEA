
#pragma once



#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"
#Include Once "win/windef.bi"
#Include once "crt/string.bi"


#Include Once "fbgfx.bi"


'type as Bus _Bus


type PPU
	As uint32_t _mydata(256 * 240)
	
	paletteRam(&H20) as uint8_t
	
	
	Bus as _Bus ptr
	
	
	declare sub ConnectBus(n as any ptr)
	'oam memory
	oamRam(&H100) as uint8_t
	
	'sprite buffers	
	secondaryOam(&H20) as uint8_t
	spriteTiles(&H10) as uint8_t
	
	'_colorsoutput(256 * 240) as uint32_t
	
	
	_pixelOutput(256 * 240) as uint32_t
	
	t as uint32_t
	v as uint32_t
	w as uint32_t
	x as uint32_t
	
	_line as uint32_t
	dot as uint32_t
	evenFrame as boolean = true
	
	
	oamAddress as uint32_t
	readBuffer as uint32_t
	
	spriteZero as boolean = FALSE
	spriteOverflow as boolean = FALSE
	inVblank as boolean = FALSE
	
	 vramIncrement as uint32_t = 1 
    spritePatternBase as uint32_t = 0
    bgPatternBase as uint32_t = 0
    spriteHeight  as uint32_t = 8 
    slave as  boolean = FALSE
    generateNmi as  boolean = FALSE

   greyScale as  boolean = FALSE
   bgInLeft as  boolean = FALSE
   sprInLeft as  boolean = FALSE
   bgRendering as  boolean = FALSE
   sprRendering as  boolean = FALSE
   emphasis as uint32_t = 0 

    atl as uint32_t = 0 
    atr as uint32_t = 0 
    tl as uint32_t = 0 
    th as uint32_t = 0 
    spriteZeroIn  as  boolean = FALSE
    spriteCount as uint32_t = 0 
	
	declare constructor()
	
	declare sub cycle()
	declare sub _reset()
	declare sub evaluateSprites()
   declare sub readTileBuffers()
   declare sub generateDot()
	declare sub setFrame()'finalarray() as uint8_t)
	declare sub incrementVx()
	declare sub incrementVy()
	declare function readInternal(adr as uint32_t) as uint8_t
	declare sub writeInternal(adr as uint32_t,value as uint32_t) 
	declare function readPalette(adr as uint16_t) as uint32_t
	declare sub writePalette(adr as uint16_t, value as uint32_t) 
   
   declare function _write(adr as uint16_t, value as uint32_t) as uint8_T 
   declare function _read(adr as uint16_t) as uint8_t
   palScreen(&H40)As uint32_t
   nesPal(64,3) as uint8_t

  
   





   ppu_nmi as boolean


End Type


constructor PPU()
   palScreen(&H00) = BGR(84, 84, 84)
	palScreen(&H01) = BGR(0, 30, 116)
	palScreen(&H02) = BGR(8, 16, 144)
	palScreen(&H03) = BGR(48, 0, 136)
	palScreen(&H04) = BGR(68, 0, 100)
	palScreen(&H05) = BGR(92, 0, 48)
	palScreen(&H06) = BGR(84, 4, 0)
	palScreen(&H07) = BGR(60, 24, 0)
	palScreen(&H08) = BGR(32, 42, 0)
	palScreen(&H09) = BGR(8, 58, 0)
	palScreen(&H0A) = BGR(0, 64, 0)
	palScreen(&H0B) = BGR(0, 60, 0)
	palScreen(&H0C) = BGR(0, 50, 60)
	palScreen(&H0D) = BGR(0, 0, 0)
	palScreen(&H0E) = BGR(0, 0, 0)
	palScreen(&H0F) = BGR(0, 0, 0)

	palScreen(&H10) = BGR(152, 150, 152)
	palScreen(&H11) = BGR(8, 76, 196)
	palScreen(&H12) = BGR(48, 50, 236)
	palScreen(&H13) = BGR(92, 30, 228)
	palScreen(&H14) = BGR(136, 20, 176)
	palScreen(&H15) = BGR(160, 20, 100)
	palScreen(&H16) = BGR(152, 34, 32)
	palScreen(&H17) = BGR(120, 60, 0)
	palScreen(&H18) = BGR(84, 90, 0)
	palScreen(&H19) = BGR(40, 114, 0)
	palScreen(&H1A) = BGR(8, 124, 0)
	palScreen(&H1B) = BGR(0, 118, 40)
	palScreen(&H1C) = BGR(0, 102, 120)
	palScreen(&H1D) = BGR(0, 0, 0)
	palScreen(&H1E) = BGR(0, 0, 0)
	palScreen(&H1F) = BGR(0, 0, 0)

	palScreen(&H20) = BGR(236, 238, 236)
	palScreen(&H21) = BGR(76, 154, 236)
	palScreen(&H22) = BGR(120, 124, 236)
	palScreen(&H23) = BGR(176, 98, 236)
	palScreen(&H24) = BGR(228, 84, 236)
	palScreen(&H25) = BGR(236, 88, 180)
	palScreen(&H26) = BGR(236, 106, 100)
	palScreen(&H27) = BGR(212, 136, 32)
	palScreen(&H28) = BGR(160, 170, 0)
	palScreen(&H29) = BGR(116, 196, 0)
	palScreen(&H2A) = BGR(76, 208, 32)
	palScreen(&H2B) = BGR(56, 204, 108)
	palScreen(&H2C) = BGR(56, 180, 204)
	palScreen(&H2D) = BGR(60, 60, 60)
	palScreen(&H2E) = BGR(0, 0, 0)
	palScreen(&H2F) = BGR(0, 0, 0)

	palScreen(&H30) = BGR(236, 238, 236)
	palScreen(&H31) = BGR(168, 204, 236)
	palScreen(&H32) = BGR(188, 188, 236)
	palScreen(&H33) = BGR(212, 178, 236)
	palScreen(&H34) = BGR(236, 174, 236)
	palScreen(&H35) = BGR(236, 174, 212)
	palScreen(&H36) = BGR(236, 180, 176)
	palScreen(&H37) = BGR(228, 196, 144)
	palScreen(&H38) = BGR(204, 210, 120)
	palScreen(&H39) = BGR(180, 222, 120)
	palScreen(&H3A) = BGR(168, 226, 144)
	palScreen(&H3B) = BGR(152, 226, 180)
	palScreen(&H3C) = BGR(160, 214, 228)
	palScreen(&H3D) = BGR(160, 162, 160)
	palScreen(&H3E) = BGR(0, 0, 0)
	palScreen(&H3F) = BGR(0, 0, 0)









this._reset



End Constructor
