

sub PPU._reset
	
	erase this.paletteram
	erase this.oamRam
	erase this.secondaryOam
	erase this.spriteTiles
	
	
	erase this._pixelOutput

	
	erase this._mydata
	
	this.t = 0
	this.v = 0
	this.w = 0
	this.x = 0
	
	this._line  = 0
	this.dot = 0
	this.evenFrame = true
	
	
	this.oamAddress  = 0
	this.readBuffer  = 0
	
	this.spriteZero  = FALSE
	this.spriteOverflow = FALSE
	this.inVblank = FALSE
	
	 this.vramIncrement  = 1 
    this.spritePatternBase  = 0
    this.bgPatternBase  = 0
    this.spriteHeight   = 8 
    this.slave  = FALSE
    this.generateNmi  = FALSE

   this.greyScale   = FALSE
   this.bgInLeft  = FALSE
   this.sprInLeft  = FALSE
   this.bgRendering  = FALSE
   this.sprRendering  = FALSE
   this.emphasis  = 0 

    this.atl = 0 
    this.atr = 0 
    this.tl  = 0 
    this.th  = 0 
    this.spriteZeroIn = FALSE
    this.spriteCount = 0 
	
End Sub


sub PPU.cycle
	
	if this._line < 240 then
		
		if this.dot < 256 then
			
			this.generateDot()
			
			if ((this.dot + 1) and &H7) = 0 then
			
			if this.bgRendering or this.sprRendering then
				this.readTileBuffers()
				this.incrementVx()
		   EndIf
				
			EndIf

		
	elseif this.dot = 256 then
		
	if this.bgRendering or this.sprRendering then
		
			this.incrementVy()
		
	EndIf
		
		elseif this.dot = 257 then	
			
			
				if this.bgRendering or this.sprRendering then
		
			this.v and= &H7BE0
		this.v or= (this.t and &H41f)
		
		
		
				EndIf
		
	elseif this.dot = 270 then
		
		this.spriteZeroIn = false
		this.spriteCount = 0
		
			if this.bgRendering or this.sprRendering then
		
		 this.evaluateSprites()
		
			EndIf
			
	elseif this.dot = 321 or this.dot = 329 then
		
		
					if this.bgRendering or this.sprRendering then
		
			this.readTileBuffers
			this.incrementVx
		
					EndIf
						
	endIf	
	
	elseif this._line = 241 then
			
	if this.dot = 1 then
		this.inVblank = true
		
		if this.generateNmi then
	 
			'frame_complete = true
			 'this.ppu_nmi = true
			  this.bus->cpu.nmiwanted = true
		EndIf
		
			
	   if this.bgRendering or this.sprRendering then
			this.evenFrame = not(this.evenFrame)
		else
		 	this.evenFrame = true
		 EndIf
	endIf
	
	
	elseif this._line = 261 then	
			
	if this.dot = 1 then
		this.inVblank = false
		this.spriteZero = false
		this.spriteOverflow = false
		
	elseif(this.dot = 257)	then
		 if(this.bgRendering or this.sprRendering) then
         ' // copy x parts from t to v
          this.v and= &H7be0 
          this.v or= (this.t and &H41f) 
		 endif
		
	
		
   
	elseif(this.dot = 270)	then
	this.spriteZeroIn = false
	  this.spriteCount = 0
	     if(this.bgRendering or this.sprRendering) then
         ' // garbage sprite fetch
          dim _base as uint32_t = iif(this.spriteHeight = 16,  &H1000,this.spritePatternBase )
          this.readInternal(_base + &Hfff)
	     endif
        
   elseif this.dot = 280 then
           if(this.bgRendering or this.sprRendering)  then
          '// copy y parts from t to v
          this.v and= &H41f 
          this.v or= (this.t and &H7be0) 
          endif
          
       elseif this.dot = 321 or this.dot =329 then
       
       
          
          
               if(this.bgRendering or this.sprRendering)  then
            	   this.readTileBuffers
               	this.incrementVx()
               endif
          
	   EndIf
	
	EndIf
	
	this.dot+=1
	
	if this.dot = 341 or (this.dot = 340 and this._line = 261 and not(this.evenFrame)) then
		this.dot = 0
		this._line+=1
		
		if this._line = 262 then
			
			this._line = 0 
		EndIf
		
	EndIf
	
	
End Sub


function PPU.readInternal(adr as uint32_t) as uint8_t
	dim data_1 as uint8_t
	
	adr and= &H3FFF
	
	'if cart_ppuread(adr,data_1) then
	'	exit function
	'	
	'EndIf
	'
	

	 return 	getmapper()->_ppuread(adr)'data_1
	
	
	
	
End Function

sub PPU.writeInternal(adr as uint32_t,value as uint32_t) 
	adr and= &H3FFF 
	 getmapper()->_ppuwrite(adr,value)
	'if cart_ppuwrite(adr,value) then
	'	
	'	
	'	'exit sub
	'EndIf
	'
	
	
	
End sub







sub PPU.setFrame()'finalarray() as uint8_t)
	for i as uint32_t = 0 to ubound(this._pixelOutput)- 1
	
		dim _color1 as uint32_t = this._pixelOutput(i)
		dim r as uint8_t = getRvalue(palScreen(_color1 and &H3F)) 'this.nesPal(_color and &H3F,0)
		dim g as uint8_t = getGvalue(palScreen(_color1 and &H3F)) 'this.nesPal(_color and &H3F,1)
		dim b as uint8_t = getBvalue(palScreen(_color1 and &H3F)) 'this.nesPal(_color and &H3F,2)
		
		'if (_color and &H40) > 0 then
		'	r = r *1.1
		'	g = g * 0.9
		'	b = b * 0.9
		'EndIf
		'
		'
		'if (_color and &H80) > 0 then
		'	r = r *0.9
		'	g = g * 1.1
		'	b = b * 0.9
		'EndIf
		'
		'
		'if (_color and &H100) > 0 then
		'	r = r *0.9
		'	g = g * 0.9
		'	b = b * 1.1
		'EndIf
		'r = iif(r >255,255,r) and &HFF
		'g = iif(g >255,255,g) and &HFF
		'b = iif(b >255,255,b) and &HFF
	'	finalArray(i * 4) = r 
    '  finalArray(i * 4 + 1) = g 
    '  finalArray(i * 4 + 2) = b 
    '  finalArray(i * 4 + 3) = 255 
     
     this._mydata(i) = rgba(r,g,b,255) 'rgba(rnd*255,rnd*255,rnd*255,255)
     
   
	Next
	
End Sub

sub PPU.ConnectBus(n as any ptr)
	
  this.bus = n
	'bus = n
End Sub



sub PPU.readtilebuffers()
    dim tileNum as uint8_t = this.readInternal(&H2000 + (this.v and &Hfff)) 
'&H2000 + (this.v and &Hfff)

    this.atl = this.atr 
    dim attAdr as uint32_t  = &H23c0 
    attAdr or= (this.v and &H1c) shr 2 
    attAdr or= (this.v and &H380) shr 4 
    attAdr or= (this.v and &Hc00) 
    this.atr = this.readInternal(attAdr) 
    if((this.v and &H40) > 0) then
     '// bottom half
      this.atr shr= 4 
    end if
    this.atr and= &Hf 
    if((this.v and &H02) > 0)  then
     ' // right half
      this.atr shr= 2 
    end if
    this.atr and= &H3 

    dim fineY as uint32_t = (this.v and &H7000) shr 12 
    this.tl and= &Hff 
    this.tl shl= 8 
    this.tl or= this.readInternal(this.bgPatternBase + tileNum * 16 + fineY) 
    this.th and= &Hff 
    this.th shl= 8 
    this.th or= this.readInternal(this.bgPatternBase + tileNum * 16 + fineY + 8)
	
End Sub

sub PPU.generateDot() 
	dim i as uint32_t = this.dot and &H7
	dim bgpixel as uint32_t
	dim sprpixel as uint32_t
	dim sprNum as int32_t = -1
	dim sprPriority as uint32_t
   dim finalcolor as uint32_t
	
	dim j as  uint16_t
	
	if this.sprRendering and (this.dot > 7 or this.sprInLeft)  then
		
	'	spritecount = 1
			do while j < this.spriteCount
			
			
			dim xPos as uint32_t = this.secondaryOam(j*4+3)
			dim xCol as uint32_t = this.dot - xPos
			
			if xCol >=0 and xCol < 8 then
				
		
		   if (this.secondaryOam(j*4+2) and &H40) > 0 then
		   	xCol = 7 - xCol
		   EndIf
			
			dim _shift as uint32_t = 7 - xCol
			dim _pixel as uint32_t = (this.spriteTiles(j) shr _shift) and 1
			
			_pixel or= ((this.spriteTiles(j+8) shr _shift) and 1)shl 1
			
			if _pixel > 0 then
				  sprPixel = _pixel or ((this.secondaryOam(j * 4 + 2) and &H3) shl 2) 
             sprPriority = (this.secondaryOam(j * 4 + 2) and &H20) shr 5 
				 sprNum = j
				exit do' for
			EndIf
		
	EndIf
			j+=1
			loop
		
		
		
		'for j as uint16_t = 0 to this.spriteCount'-1
		'	j = 0
	'		do while j < this.spriteCount
	'		
	'		
	'		dim xPos as uint32_t = this.secondaryOam(j*4+3)
	'		dim xCol as uint32_t = this.dot - xPos
	'		
	'		if xCol >=0 and xCol < 8 then
	'			
	'	
	'	   if (this.secondaryOam(j*4+2) and &H40) > 0 then
	'	   	xCol = 7 - xCol
	'	   EndIf
	'		
	'		dim _shift as uint32_t = 7 - xCol
	'		dim pixel as uint32_t = (this.spriteTiles(j) shr _shift) and 1
	'		
	'		pixel or= ((this.spriteTiles(j+8)shr _shift) and 1)shl 1
	'		
	'		if pixel > 0 then
	'			  sprPixel = pixel or ((this.secondaryOam(j * 4 + 2) and &H3) shl 2) 
   '          sprPriority = (this.secondaryOam(j * 4 + 2) and &H20) shr 5 
	'			 sprNum = j
	'			'exit do' for
	'		EndIf
	'	
	
	'	EndIf
			'j+=1
			'loop
		'Next

	EndIf
	
	
	
	
	if this.bgRendering and (this.dot > 7 or this.bgInLeft)  then
		dim shiftAmount as uint32_t = 15 - i - this.x
		bgpixel = (this.tl shr shiftAmount) and 1
		bgPixel or= ((this.th shr shiftAmount) and 1) shl 1
		dim atroff as uint32_t
		
		if this.x + i > 7 then
			
			atrOff = this.atr * 4
		else	
			  atrOff = this.atl * 4
		EndIf
		 if(bgPixel > 0)  then
        bgPixel += atrOff 
		 endif
		
		
	EndIf
	
	
	
	
	
	
	
	
	
	
	
	if  not(this.bgrendering)  and   not(this.sprRendering) then
		
		if (this.v and &H3FFF) >= &H3f00 then
			finalcolor = this.readPalette(this.v and &H1f)
			
		else 	
			finalColor = this.readPalette(0)
			
		EndIf
		
		
	else
		

	
		 	 if(bgPixel = 0) then
       if(sprPixel > 0)  then
        finalColor = this.readPalette(sprPixel + &H10)
        
		 else  
           finalColor = this.readPalette(0) 
	end if
      else  
   '     // render sprite pixel if not 0 and it has priority
        if(sprPixel > 0)  then
          '// check for sprite zero
          if(sprNum = 0 and this.spriteZeroIn and this.dot <> 255)  then
            this.spriteZero = true 
                 EndIf
	EndIf
        if(sprPixel > 0 and sprPriority = 0) then
       finalColor = this.readPalette(sprPixel + &H10) 
        else  
        finalColor = this.readPalette(bgpixel) 
        
	 EndIf
	 
	 EndIf
		
		
		
	EndIf
	
	
	
	
	
	
	' (this.emphasis shl 6) or 
	
	
	this._pixelOutput(this._line * 256 + this.dot) = (finalColor and &H3F)
	
End sub






sub PPU.evaluateSprites()
dim i as uint32_t
	'for i as uint16_t = 0 to 256-1 step 4
	'	
	i = 0
	do while i < 256
		
		
		dim sprY as uint32_t = this.oamRam(i)
		dim sprRow as uint32_t = this._line - sprY
		
		if sprRow >= 0 and sprRow < this.spriteHeight then
			
			
			if this.spriteCount = 8 then
				
				
				this.spriteOverflow = true
				
				exit do
				
			else	
				
				
			'this is where you place the sprites in secondaryOAM fianlly
			
			
			if i = 0 then
				
				this.spriteZeroIn = true
			EndIf
	
			this.secondaryOam(this.spriteCount * 4) = this.oamRam(i)
			this.secondaryOam(this.spriteCount * 4+1) = this.oamRam(i+1)	
			this.secondaryOam(this.spriteCount * 4+2) = this.oamRam(i+2)	
			this.secondaryOam(this.spriteCount * 4+3) = this.oamRam(i+3)	
				
			if ((this.oamRam(i+2) and &H80) > 0) then
				sprRow = this.spriteHeight - 1  - sprRow
				 
			EndIf
			
			
			dim _base as uint32_t = this.spritePatternBase
			dim tilenum as uint32_t = this.oamRam(i + 1)
			
			
			if this.spriteHeight = 16 then
				
				_base = (tilenum and &H1) * &H1000
				tilenum = (tilenum and &HFE)
				tilenum += (sprRow and &H8) shr 3
				sprRow and= &H7
				
			EndIf
 
			
			this.spriteTiles(this.spriteCount) = this.readInternal(_base + tileNum * 16 + sprRow)
			this.spriteTiles(this.spriteCount + 8) = this.readInternal(_base + tileNum * 16 + sprRow + 8)

			
			 this.spriteCount+=1
			 
			EndIf	
			
			
			
			
		EndIf
		i+=4
	Loop
		
	'	
	'Next
	'
	'
	if this.spriteCount < 8 then
		
		dim _base as uint32_t = iif(this.spriteHeight = 16,&H1000,this.spritePatternBase)
		this.readInternal(_base + &Hfff)
		
	EndIf
	
End Sub



  function PPU._read(adr as uint16_t) as uint8_t
  '  static ret as uint32_t = 0
   
   select case adr
   	
   	case 0
   	    ' return 0
   	    exit function
   	   case 1
       
        'return 0
     exit function
   	case 2
    
        this.w = 0
        dim ret as uint32_t = 0
        
        if(this.inVblank)  then
          ret or= &H80
          this.inVblank = false
           
        end if
         
        ret or= iif(this.spriteZero, &H40, 0)
        ret or= iif(this.spriteOverflow, &H20, 0)
        'ret or= &H50
        return ret 
       
   	case 3 
        exit function
        'return 0  
   	case 4 
      
        return this.oamRam(this.oamAddress)
       
      case 5 
         exit function
      '  return 0 
   	case 6 
      exit function
       ' return 0 
        
   	case 7
   		
   	dim adr1 as uint32_t = this.v and &H3fff	
      dim temp as uint32_t = this.readBuffer
      
        if (this.bgRendering or this.sprRendering) and (this._line < 240 or this._line = 261) then
        	
        	 this.incrementVy()
          this.incrementVx()

        else
        this.v += this.vramIncrement
        	
        	this.v and= &H7fff
        EndIf
        
 
       if(adr1 >= &H3f00)  then
        '  // read palette in temp
          temp = this.readPalette(adr1) 
       end if
        
        this.readBuffer  = this.readInternal(adr1)
        
        return temp
        
   End Select
      
   


end function


  function PPU._write(adr as uint16_t, value as uint32_t) as uint8_t
   select case adr
   	
   	case 0
   	         
        this.t and= &H73ff 
        this.t or= (value and &H3) shl 10

        this.vramIncrement = iif((value and &H04)  > 0, 32,1) 
        this.spritePatternBase = iif((value and &H08)  > 0,&H1000, 0) 
        this.bgPatternBase = iif((value and &H10) > 0, &H1000, 0)
        this.spriteHeight = iif((value and &H20) > 0, 16, 8)
        dim oldNmi as boolean = this.generateNmi 
        this.slave = iif((value and &H40) > 0,true,false) 
        this.generateNmi = iif((value and &H80) > 0,true,false) 

        if(this.generateNmi and not(oldNmi) and this.inVblank)  then
         ' // immediate nmi if enabled during vblank
        ' frame_complete = true
         ' this.ppu_nmi = true
          this.bus->cpu.nmiwanted = true
        endif
        return 0
     
   	   case 1
        this.greyScale = iif((value and &H01) > 0,true,false) 
        this.bgInLeft = iif((value and &H02) > 0,true,false)
        this.sprInLeft = iif((value and &H04) > 0,true,false)
        this.bgRendering = iif((value and &H08) > 0,true,false)
        this.sprRendering = iif((value and &H10) > 0,true,false)
       ' this.emphasis = (value and &He0) shr 5
        return 0
    
   	case 2
    

        return 0 
       
   	case 3 
        this.oamAddress = value
        return 0  
   	case 4 
      
       
       this.oamRam(this.oamAddress) = value: this.oamAddress+=1
       this.oamAddress and= &HFF
       
       return 0 
      case 5 
        if(this.w = 0)  then
          this.t and= &H7fe0 
          this.t or= (value and &Hf8) shr 3 
          this.x = value and &H7 
          this.w = 1 
         else  
          this.t and= &H0c1f 
          this.t or= (value and &H7) shl 12 
          this.t or= (value and &Hf8) shl 2 
          this.w = 0 
        end if
        return 0
         
   	case 6 
        if(this.w = 0)  then
          this.t and= &HFF
          this.t or= (value and &H3F) shl 8
        
          this.w = 1 
         else  
          this.t and= &H7f00 
          this.t or=  value
          this.v  =  this.t 
          this.w = 0 
        end if
        
        return 0 
        
   	case 7
   		
   	dim adr1 as uint32_t = this.v and &H3fff	
        
        if (this.bgRendering or this.sprRendering) and (this._line < 240 or this._line = 261) then
        	
        	 this.incrementVy()
          this.incrementVx()

        else
       this.v += this.vramIncrement
        	this.v and= &H7fff
        EndIf
        
          if(adr1 >= &H3f00)  then
       '   // write palette
          this.writePalette(adr1, value) 
          return 0
          end if
        this.writeInternal(adr1, value) 
        return 0
      
      
   End Select
      
   


  end function










sub PPU.incrementVx()
	
	if (this.v and &H1F) = &H1F then
		
		this.v and= &H7FE0
		this.v xor= &H400
		
	else
		this.v+=1
		
		
	EndIf
	
	
End Sub


sub PPU.incrementVy()
	
	if (this.v and &H7000) <> &H7000 then
		
	this.v += &H1000
		
	else
		this.v and= &HFFF
		dim coarseY as uint16_t = (this.v and &H3E0) shr 5
		if coarseY = 29 then
			coarsey = 0
			this.v xor= &H800
		
	elseif coarseY = 31 then
		coarsey = 0
	else 
		coarsey+=1
		
		EndIf
			this.v and= &H7c1f
	this.v or= (coarseY shl 5)
		
	EndIf
	

	
End Sub

sub PPU.writePalette(adr as uint16_t,value as uint32_t)
	dim palAdr as uint16_t = adr and &H1F
	
	if palAdr >= &H10 and (palAdr and &H3) = 0  then
		paladr -=&H10
		
	EndIf
	
	this.paletteRam(paladr) = value
	
	
	
	
	
	
	
	
End Sub



function PPU.readPalette(adr as uint16_t) as uint32_t
	dim palAdr as uint32_t = adr and &H1F
	
	if palAdr >= &H10 and (palAdr and &H3) = 0 then
		
		paladr -=&H10
		
		
	EndIf
	
	dim ret as uint32_t = this.paletteRam(paladr)
	
	if this.greyscale  then
		ret and=  &H30
	EndIf
	
	return ret
	
	
End Function





