

dim Shared tblname(2,1024) As uint8_t 
dim Shared tblpattern(2,4096) As uint8_t 
dim Shared tblpalette(32) As uint8_t 
tblpalette(0)=1
dim Shared palScreen(&H40)As uint32_t
dim Shared sprScreen As Long
dim Shared sprNameTable(2) As Long 
dim Shared sprPatternTable(2) As Long 

Dim Shared nesscrn As Any Ptr '= ImageCreate(128,128,RGB(0,0,0),24)

Union PPUSTATUS

Type
	unused:5 As uint8_t
	sprite_overflow:1 As uint8_t 
	sprite_zero_hit:1 As uint8_t
	vertical_blank:1 As uint8_t
End Type

reg As uint8_t

End Union: dim Shared status As PPUSTATUS

Union PPUMASK

Type
	grayscale:1 As uint8_t
	render_background_left:1 As uint8_t 
	render_sprites_left:1 As uint8_t
	render_background:1 As uint8_t
	render_sprites:1 As uint8_t
	enhance_red:1 As uint8_t 
	enhance_green:1 As uint8_t
	enhance_blue:1 As uint8_t
End Type

reg As uint8_t

End Union: dim Shared mask As PPUMASK


Union PPUCTRL

Type 
	nametable_x:1 As uint8_t
	nametable_y:1 As uint8_t 
	increment_mode:1 As uint8_t
	pattern_sprite:1 As uint8_t
	pattern_background :1 As uint8_t
	sprite_size:1 As uint8_t 
	slave_mode:1 As uint8_t 'unused
	enable_nmi:1 As uint8_t
End Type

reg As uint8_t

End Union: dim Shared control As PPUCTRL

Union loopy_reg

Type 
	coarse_x:5 As uint16_t
	coarse_y:5 As uint16_t
	nametable_x:1 As uint16_t
	nametable_y:1 As uint16_t
	fine_y:3 As uint16_t
	unused:1 As uint16_t
End Type

reg As uint16_t

End Union







dim Shared vram_addr As loopy_reg
dim Shared tram_addr As loopy_reg

dim Shared fine_x As uint8_t

dim Shared address_latch As uint8_t
dim Shared ppu_data_buffer As uint8_t
Dim Shared ppu_address As uint16_t


dim Shared scanline As int16_t
dim Shared cycle As int16_t
'dim shared odd_frame As Boolean = FALSE

dim Shared  bg_next_tile_id As uint8_t
dim Shared  bg_next_tile_attrib As uint8_t
dim Shared  bg_next_tile_lsb As uint8_t
dim Shared  bg_next_tile_msb As uint8_t
dim Shared  bg_shifter_pattern_lo As uint16_t
dim Shared  bg_shifter_pattern_hi As uint16_t
dim Shared  bg_shifter_attrib_lo As uint16_t
dim Shared  bg_shifter_attrib_hi As uint16_t

dim Shared ppu_nmi As boolean = FALSE
dim shared frame_complete As boolean


dim shared lastread as uint32_t



'TODO cartridge shared pointer?



'Sleep

'//////////////////////////////////////////////////////////////////////////////

Declare  Sub IncrementScrollX()
Declare  Sub IncrementScrollY()
Declare  Sub TransferAddressX()
Declare  Sub TransferAddressY()
Declare  Sub UpdateShifters()
Declare  Sub LoadBackgroundShifters()

Declare Function ppuRead(addr1 As uint16_t ,rdonly As bool = FALSE) As uint8_t
Declare Sub ppuWrite(addr1 As uint16_t ,data1 As uint8_t)  


Declare Function ppu_cpuRead(addr1 As uint16_t ,rdonly As bool = FALSE) As uint8_t
Declare Sub ppu_cpuWrite(addr1 As uint16_t ,data1 As uint8_t)  



Declare Sub ppu_clock()
Declare Sub ppu_Reset()

   palScreen(&H00) = RGB(84, 84, 84)
	palScreen(&H01) = RGB(0, 30, 116)
	palScreen(&H02) = RGB(8, 16, 144)
	palScreen(&H03) = RGB(48, 0, 136)
	palScreen(&H04) = RGB(68, 0, 100)
	palScreen(&H05) = RGB(92, 0, 48)
	palScreen(&H06) = RGB(84, 4, 0)
	palScreen(&H07) = RGB(60, 24, 0)
	palScreen(&H08) = RGB(32, 42, 0)
	palScreen(&H09) = RGB(8, 58, 0)
	palScreen(&H0A) = RGB(0, 64, 0)
	palScreen(&H0B) = RGB(0, 60, 0)
	palScreen(&H0C) = RGB(0, 50, 60)
	palScreen(&H0D) = RGB(0, 0, 0)
	palScreen(&H0E) = RGB(0, 0, 0)
	palScreen(&H0F) = RGB(0, 0, 0)

	palScreen(&H10) = RGB(152, 150, 152)
	palScreen(&H11) = RGB(8, 76, 196)
	palScreen(&H12) = RGB(48, 50, 236)
	palScreen(&H13) = RGB(92, 30, 228)
	palScreen(&H14) = RGB(136, 20, 176)
	palScreen(&H15) = RGB(160, 20, 100)
	palScreen(&H16) = RGB(152, 34, 32)
	palScreen(&H17) = RGB(120, 60, 0)
	palScreen(&H18) = RGB(84, 90, 0)
	palScreen(&H19) = RGB(40, 114, 0)
	palScreen(&H1A) = RGB(8, 124, 0)
	palScreen(&H1B) = RGB(0, 118, 40)
	palScreen(&H1C) = RGB(0, 102, 120)
	palScreen(&H1D) = RGB(0, 0, 0)
	palScreen(&H1E) = RGB(0, 0, 0)
	palScreen(&H1F) = RGB(0, 0, 0)

	palScreen(&H20) = RGB(236, 238, 236)
	palScreen(&H21) = RGB(76, 154, 236)
	palScreen(&H22) = RGB(120, 124, 236)
	palScreen(&H23) = RGB(176, 98, 236)
	palScreen(&H24) = RGB(228, 84, 236)
	palScreen(&H25) = RGB(236, 88, 180)
	palScreen(&H26) = RGB(236, 106, 100)
	palScreen(&H27) = RGB(212, 136, 32)
	palScreen(&H28) = RGB(160, 170, 0)
	palScreen(&H29) = RGB(116, 196, 0)
	palScreen(&H2A) = RGB(76, 208, 32)
	palScreen(&H2B) = RGB(56, 204, 108)
	palScreen(&H2C) = RGB(56, 180, 204)
	palScreen(&H2D) = RGB(60, 60, 60)
	palScreen(&H2E) = RGB(0, 0, 0)
	palScreen(&H2F) = RGB(0, 0, 0)

	palScreen(&H30) = RGB(236, 238, 236)
	palScreen(&H31) = RGB(168, 204, 236)
	palScreen(&H32) = RGB(188, 188, 236)
	palScreen(&H33) = RGB(212, 178, 236)
	palScreen(&H34) = RGB(236, 174, 236)
	palScreen(&H35) = RGB(236, 174, 212)
	palScreen(&H36) = RGB(236, 180, 176)
	palScreen(&H37) = rgb(228, 196, 144)
	palScreen(&H38) = RGB(204, 210, 120)
	palScreen(&H39) = RGB(180, 222, 120)
	palScreen(&H3A) = RGB(168, 226, 144)
	palScreen(&H3B) = RGB(152, 226, 180)
	palScreen(&H3C) = RGB(160, 214, 228)
	palScreen(&H3D) = RGB(160, 162, 160)
	palScreen(&H3E) = RGB(0, 0, 0)
	palScreen(&H3F) = RGB(0, 0, 0)


Type sObjectAttributeEntry
	sy As  uint8_t
	id As  uint8_t
	attribute As  uint8_t
	sx As  uint8_t
End Type

Dim Shared oam_addr As uint8_t

Dim Shared spritescanline(8) As sObjectAttributeEntry
Dim Shared sprite_count As uint8_t
Dim Shared sprite_shifter_pattern_lo(8) As uint8_t
Dim Shared sprite_shifter_pattern_hi(8) As uint8_t



Dim Shared OAM(64) As sObjectAttributeEntry
Dim Shared pOAM As uint8_t ptr 
pOAM = Cast(uint8_t Ptr, @OAM(0))

 

Dim Shared  cpuram(2048) As uint8_t 


Dim Shared bSpriteZeroHitPossible As bool = FALSE
Dim Shared bSpriteZeroBeingRendered As bool = FALSE









