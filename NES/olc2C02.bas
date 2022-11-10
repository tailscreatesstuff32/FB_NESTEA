		''	// Reads from the NameTable ram get delayed one cycle,
		'	'// so output buffer which contains the data from the
		'	'// previous read request
		'	data1 = ppu_data_buffer 
		'	'// then update the buffer for next time
		'	ppu_data_buffer = ppuRead(vram_addr.reg) 
		'	'// However, if the address was in the palette range, the
		'	'// data is not delayed, so it returns immediately
		'	If (vram_addr.reg >= &H3F00)Then  data1 = ppu_data_buffer 
		'	'// All reads from PPU data automatically increment the nametable
		'	'// address depending upon the mode set in the control register.
		'	'// If set to vertical mode, the increment is 32, so it skips
		'	'// one whole nametable row  in horizontal mode it just increments
		'	'// by 1, moving to the next column
		'	'vram_addr.reg +=  (IIf(control.increment_mode,32 ,1) )
		'	
		#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"
#Include Once "win/windef.bi"
#Include once "crt/string.bi"
'
Dim Shared pattables(1) As Any ptr '=> {ImageCreate(128,128,RGB(0,0,0),32),ImageCreate(128,128,RGB(0,0,0),32)}



#Include Once "nes/olc2C02.bi"
#Include Once "fbgfx.bi"


Dim Shared odd_frame	 As bool
	
	
#define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )

Dim Shared col1 As ULong
Dim Shared As uint16_t bit_mux
Dim Shared  As uint8_t p0_pixel
Dim Shared  As uint8_t p1_pixel
Dim Shared As uint8_t bg_pixel
Dim Shared As uint8_t bg_pal0 
Dim Shared As uint8_t bg_pal1
Dim Shared As uint8_t bg_palette
'Dim Shared imagedata((341*261)-1) As Uint8_t
'Dim Shared imagedata(3,341,261) As Uint8_t



Type pattables
	
	 As uint32_t mydata(128 * 128) 
	
End Type

Dim Shared mydata_pattables(1) As pattables


Dim Shared As uint32_t mydata(256 * 240)
 
'Dim Shared mydata(256 * 256) As uint32_t

'Dim Shared mydata_pattable01(256 * 256) As uint32_t


Function flipbytes(b As uint8_t) As uint8_t
	b = (b And &HF0) Shr 4 Or (b And &H0F) Shl 4
	b = (b And &HCC) Shr 2 Or (b And &H33) Shl 2
	b = (b And &HAA) Shr 1 Or (b And &H55) Shl 1
	
Return b
End Function

Function GetColourFromPaletteRam( pal1 As uint8_t,  pixel As uint8_t) As Uint32_t
	'// This is a convenience function that takes a specified palette and pixel
	'// index and returns the appropriate screen colour.
	'// " &H3F00"       - Offset into PPU addressable range where palettes are stored
	'// "palette << 2" - Each palette is 4 bytes in size
	'// "pixel"        - Each pixel index is either 0, 1, 2 or 3
	'// "&  &H3F"       - Stops us reading beyond the bounds of the palScreen array
	'ppuRead( &H3F00 + (pal1 Shl 2) + pixel) And  &H3F
	return palScreen(ppuRead( &H3F00 + (pal1 Shl 2) + pixel) And  &H3F) 

	'// Note: We dont access tblPalette directly here, instead we know that ppuRead()
	'// will map the address onto the seperate small RAM attached to the PPU bus.
End Function

Function GetColourFromPaletteRamBGR( pal1 As uint8_t,  pixel As uint8_t) As Uint32_t
	'// This is a convenience function that takes a specified palette and pixel
	'// index and returns the appropriate screen colour.
	'// " &H3F00"       - Offset into PPU addressable range where palettes are stored
	'// "palette << 2" - Each palette is 4 bytes in size
	'// "pixel"        - Each pixel index is either 0, 1, 2 or 3
	'// "&  &H3F"       - Stops us reading beyond the bounds of the palScreen array
	'ppuRead( &H3F00 + (pal1 Shl 2) + pixel) And  &H3F
	Dim col1 As ULong = palScreen(ppuRead( &H3F00 + (pal1 Shl 2) + pixel) And  &H3F) 
	 
	return BGR(RGBA_R(col1), RGBA_G(col1), RGBA_B(col1))

	'// Note: We dont access tblPalette directly here, instead we know that ppuRead()
	'// will map the address onto the seperate small RAM attached to the PPU bus.
End Function



Sub GetPatternTable overload( i As uint8_t,  Pal1 As uint8_t,offx As uint16_t,offy As uint16_t)' As bool' Any Ptr

 Dim pixel As uint8_t
	for nTileY As uint16_t = 0 To 16-1  

		for nTileX As uint16_t = 0 To  16-1
		
	
			 Dim nOffset As uint16_t= nTileY * 256 + nTileX * 16

			for  row As uint16_t = 0 to 8 -1 
			 
		
				 Dim tile_lsb As uint8_t  = ppuRead(i *  &H1000 + nOffset + row +  &H0000)
				 Dim tile_msb As uint8_t = ppuRead(i *  &H1000 + nOffset + row +  &H0008)


				for col As  uint16_t = 0 to 8-1 
				 
						' pixel = (tile_lsb and &H01) shl 1 or (tile_msb And &H01)

		
				pixel = (tile_lsb and  &H01) + (tile_msb and  &H01) 

		
					tile_lsb shr= 1: tile_msb shr= 1 

		
					 
					PSet((nTileX * 8 + (7 - col))+offx,(nTileY * 8 + row)+offy),GetColourFromPaletteRam(pal1, pixel)
					
					
				Next
			Next
		Next
	Next
		'Return TRUE'nesscrn
End Sub 'Function 

Function GetPatternTable2 OverLoad( i As uint8_t,  Pal1 As uint8_t,zoom As Integer) As Any Ptr
Dim tempscrn1 As Any Ptr = ImageCreate(128*zoom,128*zoom,RGB(0,0,0),32)

 Dim pixel As uint8_t
	for nTileY As uint16_t = 0 To 16-1  

		for nTileX As uint16_t = 0 To  16-1
		
	
			 Dim nOffset As uint16_t= nTileY * 256 + nTileX * 16

			for  row As uint16_t = 0 to 8 -1 
			 
		
				 Dim tile_lsb As uint8_t  = ppuRead(i *  &H1000 + nOffset + row +  &H0000)
				 Dim tile_msb As uint8_t = ppuRead(i *  &H1000 + nOffset + row +  &H0008)


				for col As  uint16_t = 0 to 8-1 
				 
						' pixel = (tile_lsb and &H01) shl 1 or (tile_msb And &H01)

		
				pixel = (tile_lsb and  &H01) + (tile_msb and  &H01) 

		
					tile_lsb shr= 1: tile_msb shr= 1 

		
					 
				'	PSet pattables(i),((nTileX * 8 + (7 - col)),(nTileY * 8 + row)),GetColourFromPaletteRam(pal1, pixel)
					'Line pattables(i),((nTileX * 16 + (7 - col)),(nTileY * 16 + row))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(pal1, pixel),BF
					 Line tempscrn1,((nTileX * 8 + (7 - col))*zoom,(nTileY * 8 + row)*zoom)- (((nTileX * 8 + (7 - col)))*zoom,(nTileY * 8 + row)*zoom),GetColourFromPaletteRam(pal1, pixel),BF
				Next
			Next
		Next
	Next
 Return tempscrn1
End Function 






Function GetNameTable(i as uint8_t ) As Long
	'// As of now unused, but a placeholder for nametable visualisation in teh future
	return sprNameTable(i)

End Function


function ppuRead( addr1 As uint16_t , rdonly As bool = false) As uint8_t

	Dim data1 As uint8_t =  &H00
	
	'dim addr2 as uint16_t
 addr1 And=   &H3FFF 

If (cart_ppuRead( addr1, data1)) then


'	 


	ElseIf ( addr1 >=   &H0000 and  addr1 <=   &H1FFF) Then


data1 = tblPattern( (addr1  And &H1000) shr 12, addr1 And  &H0FFF)

	ElseIf ( addr1 >=  &H2000 And addr1 <=  &H3EFF) then

 	 addr1 and= &H0FFF 


if (cart_MirrorMode_NEW() = ONESCREEN_LO) then
	 'beep
	
	
		if (addr1 >= &H0000 and addr1 <= &H03FF) Then
				  data1 =tblName(0,addr1 And &H03FF) 
		end if
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
		 data1 	= tblName(0,addr1 And &H03FF)
			End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
		   data1 	= tblName(0,addr1 And &H03FF)
			End If	
				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			 data1 	= tblName(0,addr1 And &H03FF) 
			End If 
		 
'End if
elseif (cart_MirrorMode_NEW() = ONESCREEN_HI) then
	'	
	  ' beep
	
	

	if (addr1 >= &H0000 and addr1 <= &H03FF) Then
			 data1 = tblName(1,addr1 and &H03FF)  
end if
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
		 data1 	= tblName(1,addr1 And &H03FF)
End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
		 data1 	= tblName(1,addr1 And &H03FF)
End If	
				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			 data1 	= tblName(1,addr1 And &H03FF)  
End If 
'		 
	 	 
 	'Beep


elseIf (cart_MirrorMode_NEW() =  VERTICAL) Then
	
	'// Vertical
			If (addr1 >= &H0000 and addr1 <= &H03FF) Then
			data1 = 	tblName(0,addr1 and &H03FF) 
			End if
			If (addr1 >= &H0400 and addr1 <= &H07FF) Then
				data1 = tblName(1,addr1 And &H03FF) 
			End if 
			If (addr1 >= &H0800 And addr1 <= &H0BFF) Then
				data1 = tblName(0,addr1 And &H03FF)  
			End If
			If (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
				data1 = tblName(1,addr1 And &H03FF) 
			End If 

	elseif  (cart_MirrorMode_NEW()  = HORIZONTAL) then
'	'	
'	'		// Horizontal
'	
		If (addr1 >= &H0000 and addr1 <= &H03FF) Then
				data1 = tblName(0,addr1 and &H03FF) 
		end if																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																														
'																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																													endif
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
				data1 = tblName(0,addr1 And &H03FF) 
			End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
			data1 = tblName(1,addr1 And &H03FF) 
			End If	
'				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			  data1 	= tblName(1,addr1 And &H03FF)
			End If 
'' 	 
''	
end if

	elseif ( addr1 >=  &H3F00 and addr1 <=  &H3FFF) Then
	' 
      addr1 And= &H001F 
		if ( addr1 =  &H0010) Then  addr1 =  &H0000
		if ( addr1 =  &H0014)Then   addr1 =  &H0004
		if ( addr1 =  &H0018)Then   addr1 =  &H0008
		if ( addr1 =  &H001C)Then  addr1 =  &H000C
	data1 = tblPalette( addr1)  And  IIf(mask.grayscale, &H30,   &H3F) 
	

End If
	return data1
End Function 

Sub ppuWrite( addr1 As uint16_t , data1 As uint8_t) 

 addr1 And=  &H3FFF 

 if (cart_ppuwrite( addr1, data1)) then
	 



	elseif ( addr1 >=   &H0000 and  addr1 <=   &H1FFF) Then


 
  tblPattern( (addr1  And &H1000) shr 12, addr1 And  &H0FFF) = data1 

	ElseIf ( addr1 >=  &H2000 And addr1 <=  &H3EFF) Then
	
 	 	 addr1 and= &H0FFF 

if (cart_MirrorMode_NEW() = ONESCREEN_LO) then
	 'beep
	
	
		if (addr1 >= &H0000 and addr1 <= &H03FF) Then
				  tblName(0,addr1 And &H03FF) = data1 	
		end if
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
		    tblName(0,addr1 And &H03FF) = data1 	
			End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
		    tblName(0,addr1 And &H03FF)	= data1 
			End If	
				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			tblName(0,addr1 And &H03FF)  	= data1 
			End If 
		 '
'End if
	elseif (cart_MirrorMode_NEW() = ONESCREEN_HI) then
	'	
	  ' beep
	
	

	if (addr1 >= &H0000 and addr1 <= &H03FF) Then
				 tblName(1,addr1 and &H03FF) = data1  
			end if
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
		  tblName(1,addr1 And &H03FF) 	= data1 
End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
		 tblName(1,addr1 And &H03FF) = data1 	 
End If	
				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			tblName(1,addr1 And &H03FF) =  data1 
         End If 
		 
		 
 	'Beep

elseIf (cart_MirrorMode_NEW() =  VERTICAL) Then
	 'beep
	'// Vertical
			if (addr1 >= &H0000 and addr1 <= &H03FF) Then
			tblName(0,addr1 and &H03FF) = data1  	
			EndIf
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
			  tblName(1,addr1 And &H03FF) =	data1 
			End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
			  tblName(0,addr1 And &H03FF) =	data1  
			End If	 
			If (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
				 tblName(1,addr1 And &H03FF) = data1 
			End If 
	
	elseif (cart_MirrorMode_NEW() = HORIZONTAL) then
	'	
	'		// Horizontal
	' beep
		if (addr1 >= &H0000 and addr1 <= &H03FF) Then
				tblName(0,addr1 and &H03FF) = data1
			endif
			if (addr1 >= &H0400 and addr1 <= &H07FF) Then
			tblName(0,addr1 And &H03FF) = data1
			End if 
			if (addr1 >= &H0800 And addr1 <= &H0BFF) Then
		   tblName(1,addr1 And &H03FF) = data1
			End If	
				 
			if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
			 tblName(1,addr1 And &H03FF) =  data1  
			End If 
		 
		 End if
		 

		
 	'Beep
	' 
	elseif ( addr1 >=  &H3F00 and addr1 <=  &H3FFF) Then
	' 
      addr1 And= &H001F 
		if ( addr1 =  &H0010) Then  addr1 =  &H0000
		if ( addr1 =  &H0014)Then   addr1 =  &H0004
		if ( addr1 =  &H0018)Then   addr1 =  &H0008
		if ( addr1 =  &H001C)Then  addr1 =  &H000C
      tblPalette( addr1)=	data1    '/and& (mask.grayscale ?  &H30 :  &H3F) 
      

	End if
 
End Sub

sub TransferaddressX()
		

	 
	'	// Ony if rendering is enabled
		if (mask.render_background Or mask.render_sprites) Then
	 
			vram_addr.nametable_x = tram_addr.nametable_x 
			vram_addr.coarse_x    = tram_addr.coarse_x 
	 
	End If

	End Sub

sub TransferaddressY()

	'	// Ony if rendering is enabled
		if (mask.render_background Or mask.render_sprites) Then
	 
			vram_addr.fine_y      = tram_addr.fine_y 
			vram_addr.nametable_y = tram_addr.nametable_y 
			vram_addr.coarse_y    = tram_addr.coarse_y  
		End If
End Sub


Sub IncrementScrollX()
	
		'	// Note: pixel perfect scrolling horizontally is handled by the
		'// data1 shifters. Here we are operating in the spatial domain of
		'// tiles, 8x8 pixel blocks.

		'// Ony if rendering is enabled
		if (mask.render_background or mask.render_sprites) Then
		 
			'// A single name table is 32x30 tiles. As we increment horizontally
			'// we may cross into a neighbouring nametable, or wrap around to
			'// a neighbouring nametable
			if (vram_addr.coarse_x = 31) Then
			 
				'// Leaving nametable so wrap addr1ress round
		      vram_addr.coarse_x = 0 
				'// Flip target nametable bit
				vram_addr.nametable_x = Not(vram_addr.nametable_x)
			 
			else
			 
				'// Staying in current nametable, so just increment
				vram_addr.coarse_x+=1 
			 
		End If
	End If
End Sub




sub IncrementScrollY()
	

	
		'// Incrementing vertically is more complicated. The visible nametable
		'// is 32x30 tiles, but in memory there is enough room for 32x32 tiles.
		'// The bottom two rows of tiles are in fact not tiles at all, they
		'// contain the "attribute" information for the entire table. This is
		'// information that describes which palettes are used for different
		'// regions of the nametable.

		'// In addition, the NES doesnt scroll vertically in chunks of 8 pixels
		'// i.e. the height of a tile, it can perform fine scrolling by using
		'// the fine_y component of the register. This means an increment in Y
		'// first adjusts the fine offset, but may need to adjust the whole
		'// row offset, since fine_y is a value 0 to 7, and a row is 8 pixels high

		'// Ony if rendering is enabled
		if (mask.render_background Or mask.render_sprites) Then
		 
			'// If possible, just increment the fine y offset
			if (vram_addr.fine_y < 7) Then
			 
				vram_addr.fine_y+=1
			 
			else
			 
				'// If we have gone beyond the height of a row, we need to
				'// increment the row, potentially wrapping into neighbouring
				'// vertical nametables. Dont forget however, the bottom two rows
				'// do not contain tile information. The coarse y offset is used
				'// to identify which row of the nametable we want, and the fine
				'// y offset is the specific "scanline"

				'// Reset fine y offset
		      vram_addr.fine_y = 0 

				'// Check if we need to swap vertical nametable targets
				if (vram_addr.coarse_y =  29) Then
				 
					'// We do, so reset coarse y offset
					vram_addr.coarse_y = 0 
					'// And flip the target nametable bit
			      vram_addr.nametable_y = Not(vram_addr.nametable_y)'1 
				 
				elseif (vram_addr.coarse_y  = 31) Then
				 
					'// In case the pointer is in the attribute memory, we
					'// just wrap around the current nametable
					vram_addr.coarse_y = 0 
				 
				else
				 
					'// None of the above boundary/wrapping conditions apply
					'// so just increment the coarse y offset
			vram_addr.coarse_y+=1 
				 
			 End If
			 End If
		End If
	
End Sub

	sub UpdateShifters()
 

			if (mask.render_background) Then
		 
			'// Shifting background tile pattern row
			bg_shifter_pattern_lo Shl= 1 
			bg_shifter_pattern_hi shl= 1 

			'// Shifting palette attributes by 1
			bg_shifter_attrib_lo Shl=  1 
			bg_shifter_attrib_hi Shl=  1 
			
		End If
		
'foreground stuff//////////////////////////////////////////////		
		' add sprite shifters
		If mask.render_sprites And cycle >=1 And cycle < 258 Then
			
			For i As Integer = 0 To sprite_count  
				
				If spritescanline(i).sx > 0 Then
					spritescanline(i).sx-=1
				Else
					sprite_shifter_pattern_lo(i) Shl= 1 
		      	sprite_shifter_pattern_hi(i) shl= 1 
					
					
				EndIf
			Next
			
		EndIf 
'///////////////////////////////////////////////////////////////////

		
	End Sub


	

Sub ppu_reset()
	
	fine_x =  &H00
	address_latch =  &H00
	ppu_data_buffer =  &H00
	scanline = 0
	cycle = 0
	bg_next_tile_id =  &H00 
	bg_next_tile_attrib =  &H00 
	bg_next_tile_lsb =  &H00 
	bg_next_tile_msb =  &H00 
	bg_shifter_pattern_lo =  &H0000 
	bg_shifter_pattern_hi =  &H0000 
	bg_shifter_attrib_lo =  &H0000 
	bg_shifter_attrib_hi =  &H0000 
	status.reg =  &H00 
	mask.reg =  &H00 
	control.reg =  &H00 
	vram_addr.reg =  &H0000 
	tram_addr.reg =  &H0000 
	Erase tblpalette
	erase mydata
   erase	mydata_pattables(0).mydata
   erase	mydata_pattables(1).mydata

	
End Sub

sub LoadBackgroundShifters()
 
	'	// Each PPU update we calculate one pixel. These shifters shift 1 bit along
	'	// feeding the pixel compositor with the binary information it needs. Its
	'	// 16 bits wide, because the top 8 bits are the current 8 pixels being drawn
	'	// and the bottom 8 bits are the next 8 pixels to be drawn. Naturally this means
	'	// the required bit is always the MSB of the shifter. However, "fine x" scrolling
	'	// plays a part in this too, whcih is seen later, so in fact we can choose
		'// any one of the top 8 bits.
		bg_shifter_pattern_lo = (bg_shifter_pattern_lo And  &HFF00) Or bg_next_tile_lsb
		bg_shifter_pattern_hi = (bg_shifter_pattern_hi And  &HFF00) Or bg_next_tile_msb

	'	// Attribute bits do not change per pixel, rather they change every 8 pixels
	'	// but are synchronised with the pattern shifters for convenience, so here
	'	// we take the bottom 2 bits of the attribute word which represent which
	'	// palette is being used for the current 8 pixels and the next 8 pixels, and
	'	// "inflate" them to 8 bit words.
	bg_shifter_attrib_lo  = (bg_shifter_attrib_lo And  &HFF00) Or iif((bg_next_tile_attrib And &b01),  &HFF,  &H00) 
	bg_shifter_attrib_hi  = (bg_shifter_attrib_hi And  &HFF00) Or iif((bg_next_tile_attrib And &b10) ,  &HFF,  &H00) 
	End Sub

	Function  ppu_cpuRead( addr1 As uint16_t, rdonly As bool ) As uint8_t

	Dim  data1 As uint8_t =  &H00 

	if (rdonly) Then
	 	'	// Reading from PPU registers can affect their contents
		'// so this read only option is used for examining the
		'// state of the PPU without changing its state. This is
	'	// really only used in debug mode.
		Select Case  (addr1)
	 
		case  &H0000  '// Control
			data1 = control.reg
			
		case &H0001  '// Mask
			data1 = mask.reg 
			 
		Case &H0002 ' // Status
			data1 = status.reg 
		 
		case &H0003 '// OAM Address
			 
		Case &H0004  '// OAM Data
		 
		case &H0005  '// Scroll
		 
		case &H0006: ' // PPU Address
			 
		Case &H0007  '// PPU Data
			 
		End Select
	
	else
	
	'	// These are the live PPU registers that repsond
		'// to being read from in various ways. Note that not
		'// all the registers are capable of being read from
		'// so they just retu &HHx00
		Select case (addr1)
		 
			'// Control - Not readable
		case  &H0000 

		'	// Mask - Not Readable
		Case &H0001 

			'// Status
		Case &H0002 
			'// Reading from the status register has the effect of resetting
			'// different parts of the circuit. Only the top three bits
			'// contain status information, however it is possible that
			'// some "noise" gets picked up on the bottom 5 bits which
			'// represent the last PPU bus transaction. Some games "may"
			'// use this noise as valid data (even though they probably
			'// shouldn't)
			
			'for burger time...
		 ' status.vertical_blank = 1 
			data1 = (status.reg and &HE0) Or (ppu_data_buffer and &H1F) 

			'// Clear the vertical blanking flag
			status.vertical_blank = 0 

			'// Reset Loopy's Address latch flag
			address_latch = 0 
		 

			'// OAM Address
		Case &H0003 

			'// OAM Data
		Case &H0004 
		data1 = pOAM[oam_addr]

		'	// Scroll - Not Readable
		Case &H0005 

		'	// PPU Address - Not Readable
		Case &H0006 

			'// PPU Data
		Case &H0007 
		
		'data1 = ppu_address
		'ppu_data_buffer = ppuread(ppu_address)
		'If (ppu_address >= &H3F00) Then data1 = ppu_data_buffer
		'
		'ppu_address+=IIf(control.increment_mode, 32 , 1)
		
		
		
		
		
			'	// Reads from the NameTable ram get delayed one cycle,
			'// so output buffer which contains the data from the
			'// previous read request
			data1 = ppu_data_buffer 
			'// then update the buffer for next time
			ppu_data_buffer = ppuRead(vram_addr.reg) 
			'// However, if the address was in the palette range, the
			'// data is not delayed, so it returns immediately
			If (vram_addr.reg >= &H3F00)Then  data1 = ppu_data_buffer 
			'// All reads from PPU data automatically increment the nametable
			'// address depending upon the mode set in the control register.
			'// If set to vertical mode, the increment is 32, so it skips
			'// one whole nametable row  in horizontal mode it just increments
			'// by 1, moving to the next column
			vram_addr.reg +=  (IIf(control.increment_mode,32 ,1) )
			
		
		End Select
	End If

	return data1 
End Function



Sub ppu_cpuWrite( addr1 As uint16_t,  data1 as uint8_t) 
 
	Select Case (addr1)
	 
	case &H0000 ' // Control
		control.reg = data1 
		tram_addr.nametable_x = control.nametable_x 
		tram_addr.nametable_y = control.nametable_y 
		
		
		
	 
	case &H0001' // Mask
		mask.reg = data1
 
	case &H0002' // Status
 
	case &H0003': // OAM Address
   oam_addr = data1
	case &H0004  '// OAM Data
	 pOAM[oam_addr] = data1

	case &H0005  '// Scroll
		if (address_latch =  0) Then
	 
			'// First write to scroll register contains X offset in pixel space
			'// which we split into coarse and fine x values
			fine_x = data1 and &H07 
			tram_addr.coarse_x = data1 shr 3 
			address_latch = 1 
		 
		else
		 
			'// First write to scroll register contains Y offset in pixel space
			'// which we split into coarse and fine Y values
			tram_addr.fine_y = data1 and &H07 
			tram_addr.coarse_y = data1 shr 3 
			address_latch = 0 
		 End if
		
		
		
		
 
	case &H0006 '// PPU Address
	'	if (address_latch =  0) then
	'	 
	'		'// PPU address bus can be accessed by CPU via the ADDR and DATA
	'		'// registers. The fisrt write to this register latches the high byte
	'		'// of the address, the second is the low byte. Note the writes
	'		'// are stored in the tram register...
	'		tram_addr.reg = cast(uint16_t,(Data1 and &H3F) shl 8) or (tram_addr.reg and &H00FF) 
	'		address_latch = 1 
	'	 
	'	else
	'	
	'	'	// ...when a whole address has been written, the internal vram address
	'	'	'// buffer is updated. Writing to the PPU is unwise during rendering
	'		'// as the PPU will maintam the vram address automatically whilst
	'		'// rendering the scanline position.
	'		tram_addr.reg = (tram_addr.reg and &HFF00) Or Data1 
	'	
	'		vram_addr = tram_addr 
	'		address_latch = 0 
	'End If
	'
	
	
	'if (address_latch =  0) Then
	'
	'ppu_address = (ppu_address And  &H00FF) Or (data1 Shl 8)
	'	
	'address_latch = 1
	'Else
	'	ppu_address = (ppu_address And &HFF00) Or data1
	'	
	'	
	'	address_latch = 0
	'
	'End If
	
			if (address_latch =  0) then
		 
			'// PPU address bus can be accessed by CPU via the ADDR and DATA
			'// registers. The fisrt write to this register latches the high byte
			'// of the address, the second is the low byte. Note the writes
			'// are stored in the tram register...
			tram_addr.reg = cast(uint16_t,(Data1 and &H3F) shl 8) or (tram_addr.reg and &H00FF) 
			address_latch = 1 
		 
		else
		
		'	// ...when a whole address has been written, the internal vram address
		'	'// buffer is updated. Writing to the PPU is unwise during rendering
			'// as the PPU will maintam the vram address automatically whilst
			'// rendering the scanline position.
			tram_addr.reg = (tram_addr.reg and &HFF00) Or Data1 
		
			vram_addr = tram_addr 
			address_latch = 0 
	End if
	
	
	
	
	case &H0007 '// PPU Data
		ppuWrite(vram_addr.reg, data1) 
	'	// All writes from PPU data automatically increment the nametable
	'	// address depending upon the mode set in the control register.
	'	// If set to vertical mode, the increment is 32, so it skips
	'	// one whole nametable row; in horizontal mode it just increments
	'	// by 1, moving to the next column
	'ppu_address+=IIf(control.increment_mode, 32 , 1)

		vram_addr.reg += IIf(control.increment_mode, 32 , 1)
		
	
	End Select
End Sub


Sub renderer1()
	If (scanline >= -1 And scanline < 240) then
	 
		If scanline  = 0 and cycle  = 0 and odd_frame and (mask.render_background Or mask.render_sprites)Then
		
			'// "Odd Frame" cycle skip
			cycle = 1 
		End If

		if (scanline =  -1 and cycle  = 1) Then
		 
		'	// Effectively start of new frame, so clear vertical blank flag
			status.vertical_blank = 0 
			
				 
	 '//foreground stuff/////////////////////////////////////FINE
	 
			status.sprite_overflow = 0
			status.sprite_zero_hit = 0
			
			For i As uint8_t = 0 To 8 - 1
				sprite_shifter_pattern_lo(i) = 0
				sprite_shifter_pattern_hi(i) = 0
				
			Next
			
		'/////////////////////////////////////////////////////	
		End If


		if ((cycle >= 2 and cycle < 258) or (cycle >= 321 and cycle < 338)) Then
	 
			UpdateShifters() 


			'// In these cycles we are collecting and working with visible data
			'// The "shifters" have been preloaded by the end of the previous
			'// scanline with the data for the start of this scanline. Once we
			'// leave the visible region, we go dormant until the shifters are
			'// preloaded for the next scanline.

			'// Fortunately, for background rendering, we go through a fairly
			'// repeatable sequence of events, every 2 clock cycles.
			Select case((cycle - 1) mod 8)
		 
			case 0 
				'// Load the current background tile pattern and attributes into the "shifter"
				LoadBackgroundShifters() 

				'// Fetch the next background tile ID
				'// "(vram_addr.reg & 0x0FFF)" : Mask to 12 bits that are relevant
				'// "| 0x2000"                 : Offset into nametable space on PPU address bus
				bg_next_tile_id = ppuRead(&H2000 or (vram_addr.reg And &H0FFF)) 

			'	// Explanation:
			'	// The bottom 12 bits of the loopy register provide an index into
			'	// the 4 nametables, regardless of nametable mirroring configuration.
			'	// nametable_y(1) nametable_x(1) coarse_y(5) coarse_x(5)
				'//
			'	// Consider a single nametable is a 32x32 array, and we have four of them
			'	//   0                1
			'	// 0 +----------------+----------------+
			'	//   |                |                |
			  '//   |                |                |
			'	//   |    (32x32)     |    (32x32)     |
			'	//   |                |                |
			 '//   |                |                |
			'	// 1 +----------------+----------------+
			'	//   |                |                |
			'	//   |                |                |
			'	//   |    (32x32)     |    (32x32)     |
			'	//   |                |                |
			'	//   |                |                |
			'	//   +----------------+----------------+
			'	//
			'	// This means there are 4096 potential locations in this array, which
			'	// just so happens to be 2^12!
				 
			case 2 
				'// Fetch the next background tile attribute. OK, so this one is a bit
				'// more involved :P

				'// Recall that each nametable has two rows of cells that are not tile
				'// information, instead they represent the attribute information that
				'// indicates which palettes are applied to which area on the screen.
				'// Importantly (and frustratingly) there is not a 1 to 1 correspondance
				'// between background tile and palette. Two rows of tile data holds
				'// 64 attributes. Therfore we can assume that the attributes affect
				'// 8x8 zones on the screen for that nametable. Given a working resolution
				'// of 256x240, we can further assume that each zone is 32x32 pixels
				'// in screen space, or 4x4 tiles. Four system palettes are allocated
				'// to background rendering, so a palette can be specified using just
				'// 2 bits. The attribute byte therefore can specify 4 distinct palettes.
				'// Therefore we can even further assume that a single palette is
				'// applied to a 2x2 tile combination of the 4x4 tile zone. The very fact
				'// that background tiles "share" a palette locally is the reason why
				'// in some games you see distortion in the colours at screen edges.

				'// As before when choosing the tile ID, we can use the bottom 12 bits of
				'// the loopy register, but we need to make the implementation "coarser"
				'// because instead of a specific tile, we want the attribute byte for a
				'// group of 4x4 tiles, or in other words, we divide our 32x32 address
				'// by 4 to give us an equivalent 8x8 address, and we offset this address
				'// into the attribute section of the target nametable.

				'// Reconstruct the 12 bit loopy address into an offset into the
				'// attribute memory

				'// "(vram_addr.coarse_x >> 2)"        : integer divide coarse x by 4,
				'//                                      from 5 bits to 3 bits
				'// "((vram_addr.coarse_y >> 2) << 3)" : integer divide coarse y by 4,
				'//                                      from 5 bits to 3 bits,
				'//                                      shift to make room for coarse x

				'// Result so far: YX00 00yy yxxx

				'// All attribute memory begins at 0x03C0 within a nametable, so OR with
				'// result to select target nametable, and attribute byte offset. Finally
				'// OR with 0x2000 to offset into nametable address space on PPU bus.
				'
				bg_next_tile_attrib = ppuRead(&H23C0 or (vram_addr.nametable_y Shl 11) _
					                                 Or (vram_addr.nametable_x shl 10)_
				                                    Or ((vram_addr.coarse_y shr 2) Shl 3)_
					                                 Or (vram_addr.coarse_x Shr 2)) 

				'// Right we've read the correct attribute byte for a specified address,
				'// but the byte itself is broken down further into the 2x2 tile groups
				'// in the 4x4 attribute zone.

				'// The attribute byte is assembled thus: BR(76) BL(54) TR(32) TL(10)
				'//
				'// +----+----+			    +----+----+
				'// | TL | TR |			    | ID | ID |
				'// +----+----+ where TL =   +----+----+
				'// | BL | BR |			    | ID | ID |
				'// +----+----+			    +----+----+
				'//
			'	// Since we know we can access a tile directly from the 12 bit address, we
			'	// can analyse the bottom bits of the coarse coordinates to provide us with
			'	// the correct offset into the 8-bit word, to yield the 2 bits we are
			'	// actually interested in which specifies the palette for the 2x2 group of
				'// tiles. We know if "coarse y % 4" < 2 we are in the top half else bottom half.
				'// Likewise if "coarse x % 4" < 2 we are in the left half else right half.
				'// Ultimately we want the bottom two bits of our attribute word to be the
				'// palette selected. So shift as required...
				if (vram_addr.coarse_y And &H02) Then bg_next_tile_attrib shr= 4 
				if (vram_addr.coarse_x And &H02)Then  bg_next_tile_attrib Shr= 2 
				bg_next_tile_attrib and= &H03 
				' 

			'	// Compared to the last two, the next two are the easy ones... :P

			case 4 
			'	// Fetch the next background tile LSB bit plane from the pattern memory
			'  // The Tile ID has been read from the nametable. We will use this id to
			'	// index into the pattern memory to find the correct sprite (assuming
			'	// the sprites lie on 8x8 pixel boundaries in that memory, which they do
			'	// even though 8x16 sprites exist, as background tiles are always 8x8).
			'	//
			'	// Since the sprites are effectively 1 bit deep, but 8 pixels wide, we
			'	// can represent a whole sprite row as a single byte, so offsetting
			'	// into the pattern memory is easy. In total there is 8KB so we need a
			'	// 13 bit address.

				'// "(control.pattern_background << 12)"  : the pattern memory selector
			'	//                                         from control register, either 0K
				'//                                         or 4K offset
				'// "((uint16_t)bg_next_tile_id << 4)"    : the tile id multiplied by 16, as
				'//                                         2 lots of 8 rows of 8 bit pixels
				'// "(vram_addr.fine_y)"                  : Offset into which row based on
				'//                                         vertical scroll offset
				'// "+ 0"                                 : Mental clarity for plane offset
				'// Note: No PPU address bus offset required as it starts at 0x0000
				
				bg_next_tile_lsb = ppuRead((control.pattern_background Shl 12)  _
					                       +  (Cast(uint16_t,bg_next_tile_id) Shl 4)  _
				                          +  (vram_addr.fine_y) + 0) 
				
					'bg_next_tile_lsb = ppuRead((1 Shl 12)  _
					'                       +  (bg_next_tile_id Shl 4)  _
					'                       +  (vram_addr.fine_y) + 0) 

			
			case 6
			'	// Fetch the next background tile MSB bit plane from the pattern memory
			'	// This is the same as above, but has a +8 offset to select the next bit plane
			
				bg_next_tile_msb = ppuRead((control.pattern_background Shl 12) _
					                        + (cast(uint16_t,bg_next_tile_id) shl 4) _
					                        + (vram_addr.fine_y) + 8) 
				
					'bg_next_tile_msb = ppuRead((1 Shl 12) _
					'                        + (bg_next_tile_id shl 4) _
					'                        + (vram_addr.fine_y) + 8) 
				
				
				
				 
			case 7 
				'// Increment the background tile "pointer" to the next tile horizontally
				'// in the nametable memory. Note this may cross nametable boundaries which
				'// is a little complex, but essential to implement scrolling
				IncrementScrollX() 
				 
			End Select
	   End If 
	
	
	
		'// End of a visible scanline, so increment downwards...
		if (cycle =  256) then
		 
			IncrementScrollY() 
		End If

		'//...and reset the x position
		if (cycle  = 257) Then
		 
			LoadBackgroundShifters() 
			TransferAddressX() 
		End If

	
		
		'// Superfluous reads of tile id at end of scanline
		if (cycle = 338 Or cycle = 340) Then
		 
			bg_next_tile_id = ppuRead(&H2000 or (vram_addr.reg and &H0FFF)) 
		End If
		
   If (scanline =  -1 and cycle >= 280 And cycle < 305) Then
		 
			'// End of vertical blank period so reset the Y address ready for rendering
		TransferaddressY() 
		
	EndIf
	
	
If cycle  = 257 And scanline >=0 Then
		memset(@spritescanline(0),&HFF,8 * SizeOf(sObjectAttributeEntry))
		sprite_count = 0
		
		
		
		For i As uint8_t = 0 To 8 -1
			
			sprite_shifter_pattern_lo(i) = 0 
			sprite_shifter_pattern_hi(i) = 0 
			
		Next
		
	Dim nOAMEntry As uint8_t = 0 
	bSpriteZeroHitPossible = FALSE
	While nOAMEntry < 64 And sprite_count < 9
			Dim diff As int16_t = Cast(int16_t,scanline - Cast(int16_t,OAM(nOAMEntry).sy))
		If (diff >=0 And diff < (IIf(control.sprite_size,16,8))) Then
			If sprite_count < 8 Then
				
				If nOAMEntry = 0 Then
					bSpriteZeroHitPossible = TRUE
					
				EndIf
				
				
				memcpy(@spritescanline(sprite_count),@OAM(nOAMEntry),SizeOf(sObjectAttributeEntry))
				sprite_count+=1
			End If
		EndIf
		nOAMEntry+=1
	Wend
	status.sprite_overflow = IIf(sprite_count > 8,1,0)
   End If	
	
	
	
	If cycle = 340 Then
	
		'WindowTitle(Str(sprite_count))
		
		'For i As uint8_t = 0 To sprite_count-1
		'	
		Dim i As uint8_t = 0
		While i < sprite_count
		Dim As uint8_t sprite_pattern_bits_lo,sprite_pattern_bits_hi
		Dim As uint16_t sprite_pattern_addr_lo,sprite_pattern_addr_hi
	
		If iif(control.sprite_size,0,1) Then
			
			If IIf(spritescanline(i).attribute And &H80,0,1) Then 
				
		sprite_pattern_addr_lo = _ 
						(control.pattern_sprite Shl 12) _
						Or (spritescanline(i).id Shl 4) _
						Or (scanline - spriteScanline(i).sy)
				
			Else
				sprite_pattern_addr_lo = _ 
						(control.pattern_sprite Shl 12) _
						Or (spritescanline(i).id Shl 4) _
						Or (7-(scanline - spriteScanline(i).sy))
				
				
			EndIf
			
			
			
			
		Else
			
			If IIf(spritescanline(i).attribute And &H80,0,1) Then 
				
				If scanline - spriteScanline(i).sy < 8 Then
					
					sprite_pattern_addr_lo = _  
									((spriteScanline(i).id And &H01) Shl 12) _
									Or ((spriteScanline(i).id And &HFE) Shl 4) _ 
									Or ((scanline - spriteScanline(i).sy) And &H07)
				
				
				Else
				
						sprite_pattern_addr_lo = _  
									((spriteScanline(i).id And &H01) Shl 12) _
									Or (((spriteScanline(i).id And &HFE)+1) Shl 4) _ 
									Or ((scanline - spriteScanline(i).sy) And &H07)
				
				
				
				EndIf 
				
			
			Else
					
				
				If scanline - spriteScanline(i).sy < 8 Then
				
						sprite_pattern_addr_lo = _  
									((spriteScanline(i).id And &H01) Shl 12) _
									Or (((spriteScanline(i).id And &HFE)+1) Shl 4) _ 
									Or ((7-(scanline - spriteScanline(i).sy)) And &H07)
				Else
				sprite_pattern_addr_lo = _  
									((spriteScanline(i).id And &H01) Shl 12) _
									Or ((spriteScanline(i).id And &HFE) Shl 4) _ 
									Or ((7-(scanline - spriteScanline(i).sy)) And &H07)
				EndIf
			EndIf
		EndIf
		
		
		
		sprite_pattern_addr_hi = sprite_pattern_addr_lo + 8
		sprite_pattern_bits_lo = ppuRead(sprite_pattern_addr_lo)
		sprite_pattern_bits_hi = ppuRead(sprite_pattern_addr_hi)
		 
			
			If spritescanline(i).attribute And &H40 Then
				
				
	      sprite_pattern_bits_lo = flipbytes(sprite_pattern_bits_lo)
		   sprite_pattern_bits_hi = flipbytes(sprite_pattern_bits_hi)
		
			EndIf
			
			sprite_shifter_pattern_lo(i) = sprite_pattern_bits_lo
			sprite_shifter_pattern_hi(i) = sprite_pattern_bits_hi
			i+=1
		Wend
	 'Next
		

	EndIf
	
	
		
	
End If ' this end if  belongs to 	line 784

	
	
	

'end foreground stuff///////////////////////////////////////////////////			
		
	 
	 
	 
	 
	 
	 
	 
	 
	 
'///////////////////////////////////////////////////////////////////////	 
	 	if (scanline =  240) Then
	 
		'// Post Render Scanline - Do Nothing!
	End If

	if (scanline >= 241 And scanline < 261) Then
' 
		if (scanline =  241 and cycle =  1) Then
		 
		'	// Effectively end of frame, so set vertical blank flag
			status.vertical_blank = 1 

		'	// If the control register tells us to emit a ppu_nmi when
		'	// entering vertical blanking period, do it! The CPU
		'	// will be informed that rendering is complete so it can
		'	// perform operations with the PPU knowing it wont
		'	// produce visible artefacts
	
			if (control.enable_nmi) Then
				'Sleep
				ppu_nmi = true 
			End If
	    End If
	End If




	'// Composition - We now have background pixel information for this cycle
	'// At this point we are only interested in background

    bg_pixel = &H00   '// The 2-bit pixel to be rendered
	 bg_palette = &H00  '// The 3-bit index of the palette the pixel indexes

	'// We only render backgrounds if the PPU is enabled to do so. Note if
	'// background rendering is disabled, the pixel and palette combine
	'// to form 0x00. This will fall through the colour tables to yield
	'// the current background colour in effect
	If (mask.render_background) Then
If (mask.render_background_left or (cycle >= 9)) Then
		'// Handle Pixel Selection by selecting the relevant bit
		'// depending upon fine x scolling. This has the effect of
		'// offsetting ALL background rendering by a set number
		'// of pixels, permitting smooth scrolling
		bit_mux = &H8000 shr fine_x 

		'// Select Plane pixels by extracting from the shifter
		'// at the required location.
		
		
		 p0_pixel = IIf((bg_shifter_pattern_lo And bit_mux) > 0, 1,  0) 
		 p1_pixel = IIf((bg_shifter_pattern_hi and bit_mux) > 0, 1, 0) 

	'	// Combine to form pixel index
		bg_pixel = (p1_pixel   Shl 1)or p0_pixel 

		'// Get palette
		bg_pal0 = iif((bg_shifter_attrib_lo And bit_mux)  > 0,1,0) 
		bg_pal1 = iif((bg_shifter_attrib_hi And bit_mux) > 0,1,0) 
		bg_palette = (bg_pal1 shl 1) or bg_pal0 
	 End If
	End If
	 
	 
''//foreground stuff/////////////////////////////////////
	 Dim fg_pixel As uint8_t
	 Dim fg_palette As uint8_t
	 Dim fg_priority As uint8_t
	 
	 
	 If mask.render_sprites Then
	 		 bSpriteZeroBeingRendered = FALSE
	 	
	 	'For i As uint8_t = 0 To sprite_count-1
	If (mask.render_background_left or (cycle >= 9)) Then
	Dim i As uint8_t = 0 

	 		While i  < sprite_count
	 		If spritescanline(i).sx = 0 Then
	 			Dim As uint8_t fg_pixel_lo = IIf((sprite_shifter_pattern_lo(i) And &H80) > 0,1,0)
	 			Dim As uint8_t fg_pixel_hi = IIf((sprite_shifter_pattern_hi(i) And &H80) > 0,1,0)
	 			fg_pixel = (fg_pixel_hi Shl 1) Or fg_pixel_lo
	 			
	 			fg_palette = (spritescanline(i).attribute And &H03) + &H04
	 			fg_priority = IIf(spritescanline(i).attribute And &H20,0,1)
	 			
	 			If fg_pixel <> 0 Then
	 				
	 				'Exit for
	 				If i= 0 Then
	 				bSpriteZeroBeingRendered	= TRUE
	 				EndIf
	 				
	 				Exit while
	 			EndIf
	 		EndIf
	 		i+=1
	 		Wend
	 		EndIf
	 	'Next 
	 EndIf
	 
	 Dim pixel As uint8_t = 0
	 Dim pal1 As uint8_t= 0
	 
	 	 If (bg_pixel = 0 And fg_pixel = 0) Then
	 	
	 	pixel = 0
	 	pal1 = 0 
	 	
	 ElseIf (bg_pixel = 0 And fg_pixel >0) Then
	 	
	 	pixel = fg_pixel
	 	pal1 = fg_palette
	  ElseIf (bg_pixel > 0 And fg_pixel = 0) Then
	 	
	 	pixel = bg_pixel
	 	pal1 = bg_palette
	 	
	 ElseIf (bg_pixel > 0 And fg_pixel > 0) Then
	 	
	 	'pixel = bg_pixel
	 	'pal1 = bg_palette	
	 	
	 	If fg_priority Then
	 		
	 		
	 		
	 		pixel = fg_pixel
	 		pal1 = fg_palette
	 	Else
	 		pixel = bg_pixel
	 		pal1 = bg_palette
	 		
	 	EndIf
	
	 	If bSpriteZeroHitPossible And bSpriteZeroBeingRendered Then
	 		
	 		If mask.render_background And mask.render_sprites Then
	 			
	 			If (mask.render_background_left Or mask.render_sprites_left)=0 Then
	 				
	 				If cycle >= 9 And cycle < 258 Then
	 					
	 					status.sprite_zero_hit = 1
	 					
	 					
	 				EndIf
	 				
	 			Else
	 					If cycle >= 1 And cycle < 258 Then
	 					
	 					status.sprite_zero_hit = 1
	 					EndIf
	 			EndIf
	 		EndIf
	 	EndIf
	 	
	 	
	 	
	 EndIf
	 
	 
	 
	 
	
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
'////////////////////////////////////////////////////////////
	'col1 = GetColourFromPaletteRam(pal1, pixel)
	'image width: 341
   'image height: 261
'imagedata(cycle*scanline) = RGBA_R(col1)
'imagedata((cycle+1)*scanline) = RGBA_G(col1)
'imagedata((cycle+2)*scanline) = RGBA_B(col1)
'imagedata((cycle+3)*scanline) = RGBA_A(col1)



'WIP
'imagedata(3,cycle,scanline) = RGBA_A(col1)
'imagedata(2,cycle,scanline) = RGBA_R(col1)
'imagedata(1,cycle,scanline) = RGBA_G(col1)
'imagedata(0,cycle,scanline) = RGBA_B(col1)



	'imagedata(2) = RGBA_B(col1)
	'imagedata(3) = RGBA_A(col1)  
	
	

	'Line nesscrn,((cycle-1)*3,  ((scanline)*3))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(pal1, pixel) , BF 
	 'Line nesscrn,((cycle-1)*2,  ((scanline)*2))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(pal1, pixel) , BF 



	' Line nesscrn,((cycle-1)*2,  ((scanline)*2))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(bg_palette, bg_pixel) , BF 
	' Line nesscrn,((cycle-1)*2,  ((scanline)*2))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(fg_palette, fg_pixel) , BF 


If cycle >  0 Then
If scanline > -1 Then
If cycle <=  256 Then
If scanline <=  240-1 Then
mydata(((cycle-1)+256*scanline)) = GetColourFromPaletteRam(pal1, pixel)
'Line nesscrn,((cycle-1)*2,  ((scanline)*2))-STEP(2 OR 1, 2 OR 1), mydata((cycle+341*scanline)-1), BF

End If
End If
End If
End If
	 


 
 	'
 	'If(mask.render_background or mask.render_sprites) Then
   ' If ( scanline < 240 ) Then
   '     
   '         If( cycle = 324 Or cycle = 4 )Then
   '             get_scanline()
   '         End If
   '     Else
   '         If ( cycle = 260 ) Then
   '             get_scanline()
   '         End If
   ' End If
   ' 
   ' 
   ' 
   ' 
   ' 
   ' 
 	'End If
    

 		'If(mask.render_background or mask.render_sprites) Then
 		'	'If scanline < 240 Then
 		'	' And scanline < 240
 		'If (cycle >= 260 And cycle <= 341) And scanline < 240 Then
 		'
 		'get_scanline()
 		'
 		''End if	
 		'EndIf

		'If (cycle =  260 And scanline < 240) Then
		' 
		'	
		'	 get_scanline()

		'End If
	 'End If
	 'If(mask.render_background or mask.render_sprites) Then
		'If (cycle =  44 And scanline = 188) Then
		'
		'	
		''	 get_scanline()

		'End If
		
		
	 'End If
	 
	 cycle+=1
	 	'	 If(mask.render_background or mask.render_sprites) Then
		'If (cycle =  260 And scanline < 240) Then
		'
		'	
		'	 pmapper->get_scanline()

		'End If
		'
	 	'	 End If
	 
	 
	 
		If(mask.render_background or mask.render_sprites) Then
	 '	'260	 		 	
	 '	'279
	 '	'280
		If (cycle =  279 And scanline < 240) Then
		'If (scanline < 240) Then
			
			pmapper->get_scanline()

		End If
		
	 End If
 
 
 
	 '
	 
	 ' 	If(mask.render_background or mask.render_sprites) Then
    'If ( scanline < 240 ) Then
    '    
    '    '   If( cycle = 324 Or cycle = 4 )Then
    '    '        pmapper->get_scanline()
    '    '    End If
    '    'Else
    '    
    '  '  If ( cycle = 260) Then
    '   '        pmapper->get_scanline()
    '   ' End If
    '    
    '        If ( cycle = 266) Then
    '           pmapper->get_scanline()
    '        End If
    'End If
    '
    
    
    
    
    
 	'End If
    

	 
	 
	 
	 
	 
 		
'If  scanline = 174  Then
'  If cycle = 288 Then
'    	get_scanline()
'   
'    
'    End If
' 	End If
' 	
' 		
' 		
		'
	If (cycle >= 341) then
 
		cycle = 0 
		scanline+=1
		if (scanline >= 261) Then
		 
			scanline = -1
			frame_complete = TRUE
			 odd_frame = IIf(odd_frame,0,1)
		End If
	End If

End Sub


Sub ppu_clock()

	
renderer1()

'	 		
'GetColourFromPaletteRam(bg_palette, bg_pixel)


'  Line nesscrn,((cycle-1)*2,  ((scanline)*2))-STEP(2 OR 1, 2 OR 1),GetColourFromPaletteRam(bg_palette, bg_pixel) , BF 
'
''
'
'
' 	cycle+=1
' 	
'	If (cycle >= 341) then
' 
'		cycle = 0 
'		scanline+=1
'		if (scanline >= 261) Then
'		 
'			scanline = -1
'			frame_complete = TRUE
'			 
'		End If
'	End If

 	
End Sub


'             'allow for easy changes...
'
'DO 'Main Loop
'    LT! = TIMER ' Set Loop start time..
'   
'    LOCATE 1, 1: PRINT INT(FPS!); "   " 'Visual reference on whats happening..
'   
'    FPS! = F / (TIMER - T!)' Find FPS
'    IF LOCFPS <> INT(FPS!) THEN 'If the actual FPS is not the FPS wanted
'                                'then modify the delay...
'                               
'        IF LOCFPS < FPS! THEN DLY! += .001 'Increace Delay if too fast
'        IF LOCFPS > FPS! THEN DLY! -= .001 'Decreace Delay if too slow
'       
'    END IF 'Stop checking
'   
'    DO: LOOP UNTIL (TIMER - LT!) >= DLY! 'Loop delay, with the help of
'                                         'above, this will speed up or
'                                         'slow down the main loop speed.
'                                         
'    F += 1 'Count the frames, this will divide to loop time for FPS
'   
'LOOP UNTIL INKEY$ <> "" 'Loop until key is pressed..

