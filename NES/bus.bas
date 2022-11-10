#pragma once

#include "NES\Bus.bi"
'#include Once "NES\CPU.bi"

'#include once "nes\olc6502.bi"

'#include once "nes\olc6502_NEW_2.bi"

dim shared Businst as any ptr


constructor Bus()


'businst = @this



cpu.ConnectBus(@this)
'apu.ConnectBus(@this) ' LAGS ON MY EMU NEED TO FIX
 ppu.connectbus(@this) 'FIXES




erase this.ram


End Constructor



sub Bus._getpixels()
'	ppu.SetFrame()
	
End Sub




'finished
 function Bus._write(adr as uint16_t, value as uint8_t) as boolean
	 adr and= &Hffff
	 
if (cart_cpuWrite(adr, value)) Then
	 
		'// The cartridge "sees all" and has the facility to veto
		'// the propagation of the bus transaction if it requires.
		'// This allows the cartridge to map any address to some
		'// other data, including the facility to divert transactions
		'// with other physical devices. The NES does not do this
		'// but I figured it might be quite a flexible way of adding
		'// "custom" hardware to the NES in the future!
	
	elseif (adr >= &H0000 And adr <= &H1FFF) Then
	
		'// System RAM Address Range. The range covers 8KB, though
		'// there is only 2KB available. That 2KB is "mirrored"
		'// through this address range. Using bitwise AND to mask
		'// the bottom 11 bits is the same as addr % 2048.
		this.Ram(adr And &H07FF) = value  

	
	ElseIf (adr >= &H2000 And adr <= &H3FFF) Then

		'// PPU Address range. The PPU only has 8 primary registers
		'// and these are repeated throughout this range. We can
		'// use bitwise AND operation to mask the bottom 3 bits,
		'// which is the equivalent of addr % 8.
	
		'ppu_cpuWrite(adr And &H0007, value)
		
	ppu._write(adr And &H7, value)
		
  elseif ((adr >= &H4000 and adr <= &H4013) or adr = &H4015 or adr = &H4017) then' //  NES APU
	
		apu.Write_apu(adr, value) 
	
		
	ElseIf (adr = &H4014) Then
		this.dma_page = value
		this.dma_addr = 0
		this.dma_transfer = TRUE
		
		
		 this.inDma = true 
       this.dmaBase = value shl 8 
		
		'beep


	elseif (adr >= &H4016 And adr <= &H4017) Then

		this.controller_state(adr and &H0001) = this.controller(adr And &H0001)
	End If

	   'if adr < &H4020 then
   	'
   	''this.apu.write_apu(adr,value)
   	'
	   'EndIf
	
End function

'finished
function Bus._read(adr as uint16_t,bReadOnly as boolean) as uint8_t
adr and= &Hffff
   	Dim As uint8_t data1 = &H00 
 	
	If (cart_cpuRead(adr, data1)) Then
	 
	'	// Cartridge Address Range

	ElseIf (adr >= &H0000 and adr <= &H1FFF) Then
	 
		'// System RAM Address Range, mirrored every 2048
		data1 = this.Ram(adr and &H07FF) 
	 
	ElseIf (adr >= &H2000 And adr <= &H3FFF) then
 
		'// PPU Address range, mirrored every 8
	 'data1 = ppu_cpuRead(adr And &H0007, bReadOnly) 
		
		
      data1 = ppu._Read(adr And &H7)
      
    elseif adr = &H4015 then
   	
   	data1 = this.apu.read_apu(adr)
   

      
      
	ElseIf (adr >= &H4016 and adr <= &H4017) then
	
		data1 = (this.controller_state(adr and &H0001) and &H80) > 0 
		this.controller_state(adr and &H0001) Shl= 1 
		
	End If



	return data1

	
End function

Sub Bus._reset(hard as boolean)

	
	'hard reset////////////////////////
	if hard then
	Erase(this.ram)
	end if
	
	
	cart_reset(hard)
	'apu.apu_reset()
	'//////////////////////////////////
   cpu._reset()
   
   
	 'ppu_reset() 
	
	ppu._reset() ' NEW PPU
	
	
	this.cycles = 0
	
	this.nSystemClockCounter = 0
	this.dma_page = 0
	this.dma_addr = 0
	this.dma_data = 0
	this.dma_dummy = TRUE
	this.dma_transfer = FALSE
	
	
	
	
	
End Sub	
	


'Sub bus._clock() 
'	
'	'
'	'	// Clocking. The heart and soul of an emulator. The running
'	'// frequency is controlled by whatever calls this function.
'	'// So here we "divide" the clock as necessary and call
'	'// the peripheral devices clock() function at the correct
'	'// times.
'
'	'// The fastest clock frequency the digital system cares
'	'// about is equivalent to the PPU clock. So the PPU is clocked
'	'// each time this function is called.
'	ppu_clock() 
'	apu.apu_cycle
'	 
'	'// The CPU runs 3 times slower than the PPU so we only call its
'	'// clock() function every 3 times this function is called. We
'	'// have a global counter to keep track of this.
'	if (this.nSystemClockCounter mod 3 =  0) Then
'		If this.dma_transfer Then
'		'	
'			If this.dma_dummy Then
'				
'				if (this.nSystemClockCounter mod 2 =  1) Then
'					this.dma_dummy = FALSE
'				End If
'		'		
'			Else
'					if (this.nSystemClockCounter mod 2 =  0) Then
'					this.dma_data = this._read(this.dma_page shl 8 or this.dma_addr)
'					Else
'				
'						pOAM[this.dma_addr] = this.dma_data
'						this.dma_addr+=1
'				   		
'				      if this.dma_addr = 0 Then
'							this.dma_transfer = FALSE
'							this.dma_dummy = TRUE
'						EndIf
'		'
'		'	 	
'				
'		      End If
'		'		
'	
'	End If
'     
'		Else  
'	cpu._clock() 
'
'
'		End If
'	
'End If ' ends nSystemClockCounter mod 3 =  0
'
'		
'
'	'// The PPU is capable of emitting an interrupt to indicate the
'	'// vertical blanking period has been entered. If it has, we need
'	'// to send that irq to the CPU.
'	
'	If (getmapper()->irqState()) Then
'
'		GetMapper()->irqClear()
'		cpu.irq()		
'	End If
'	'
'	if (ppu_nmi) Then
'	 
'		ppu_nmi = false 
'		cpu.nmi() 
'	End If
'
'	this.nSystemClockCounter+=1
'	
'End Sub


sub bus.old_funcs_tests()
	'
	'	// Clocking. The heart and soul of an emulator. The running
	'// frequency is controlled by whatever calls this function.
	'// So here we "divide" the clock as necessary and call
	'// the peripheral devices clock() function at the correct
	'// times.

	'// The fastest clock frequency the digital system cares
	'// about is equivalent to the PPU clock. So the PPU is clocked
	'// each time this function is called.
	
	
	
	
	'ppu.cycle()
	
	
	
	
	
'frame_complete = true
 
	
	
	'	if (this.nSystemClockCounter mod 6) Then

	'apu.apu_cycle
	'EndIf	 
	
'apu.apu_cycle
	
	'
	'	if (this.nSystemClockCounter mod 3 =  0) Then

	''apu.apu_cycle
	'EndIf	 

	'// The CPU runs 3 times slower than the PPU so we only call its
	'// clock() function every 3 times this function is called. We
	'// have a global counter to keep track of this.
	
		
	'if (this.nSystemClockCounter mod 3 =  0) Then
	''	If this.dma_transfer Then
	''	'	
	''		If this.dma_dummy Then
	''			
	''			if (this.nSystemClockCounter mod 2 =  1) Then
	''				this.dma_dummy = FALSE
	''			End If
	''	'		
	''		Else
	''				if (this.nSystemClockCounter mod 2 =  0) Then
	''				this.dma_data = this._read(this.dma_page shl 8 or this.dma_addr)
	''				Else
	''			
	''			
	''			   
	''			      
	''					pOAM[this.dma_addr] = this.dma_data
	''					this.dma_addr+=1
	''			   		
	''			      if this.dma_addr = 0 Then
	''						this.dma_transfer = FALSE
	''						this.dma_dummy = TRUE
	''					EndIf
	''	'
	''	'	 	
	''			
	''	      End If
	''	'		
	''	
	''End If
   ''  
	''	Else
	'			'while(cycleCount < 29780)
	'				
	'		cpu._clock()		
	'
	''cycleCount +=1
	'	
	''wend
	'
	'		

 

	'	'End If


	'End If ' ends nSystemClockCounter mod 3 =  0
	
	
	



  'dim bAudioSampleReady as bool = false
  'this.dAudiotime += this.dAudioTimePerNESClock
  '	if (dAudioTime >= dAudioTimePerSystemSample) then
		'this.dAudioTime -= this.dAudioTimePerSystemSample 
		'this.dAudioSample = apu.mix() 
		'bAudioSampleReady = true 
  '	end if 
  '

if(this.cycles =  0) then
this.cycles = 3



	'If (getmapper()->irqState() and not(cpu.getflag(cpu.I))) Then

	'GetMapper()->irqClear()
	'	cpu.irq()
	'		'		else
	'	
	'	
	'	
	'End If
	'''


'///////////////////////	
   '	if (ppu.ppu_nmi)  Then 
	'	ppu.ppu_nmi = false
	'	cpu.nmi()
	'EndIf
   '     	
      if(this.inDma = false) then
   
        this.cpu._clock()
        
'       	if (ppu.ppu_nmi)  Then 
	'	ppu.ppu_nmi = false
	'	cpu.nmi()
        '   EndIf
     
        	
        else 
        
      '  if(this.dmaTimer > 0) then
      '   if((this.dmaTimer and 1) =  0)then
      '  '   // even cycles are write to ppu
      '    'ppu_cpuWrite(4, this.dmaValue) 
      '    
      '    
		'				pOAM[this.dma_addr] = this.dmaValue
		'				this.dma_addr+=1
		'				this.dma_addr and= &HFF
      '   else  
      ' '    // odd cycles are read for value
      '   this.dmaValue = this._read(this.dmaBase + ((this.dmaTimer \ 2) and &Hff))
      '     
      '     
      '   endif
      '  endif
      '  ' 
        this.dmaTimer+=1
                if(this.dmaTimer = 513)  then
          this.dmaTimer = 0 
          this.inDma = false 
      endif
        
        
      endif
  this.apu.apu_cycle()

     end if
     
     
     
     
     
   '  ppu_clock() 
     
     
     
     
     
     
   ' this.ppu.cycle()
    this.cycles-=1
 


	
		'// The PPU is capable of emitting an interrupt to indicate the
	'// vertical blanking period has been entered. If it has, we need
	'// to send that irq to the CPU.
	
	'and not(cpu.getflag(cpu.I))
'	If (getmapper()->irqState() and not(cpu.getflag(cpu.I))) Then
'
'		GetMapper()->irqClear()
'		cpu.irq()
'		
''	else
'		
'		
'		
'	End If
	
	'if (ppu_nmi)  Then 
	'	ppu_nmi = false
	'	cpu.nmi() 
	'End If
		

	this.nSystemClockCounter+=1
End Sub



sub bus.old_funcs()
	'
	'	// Clocking. The heart and soul of an emulator. The running
	'// frequency is controlled by whatever calls this function.
	'// So here we "divide" the clock as necessary and call
	'// the peripheral devices clock() function at the correct
	'// times.

	'// The fastest clock frequency the digital system cares
	'// about is equivalent to the PPU clock. So the PPU is clocked
	'// each time this function is called.
	
	
'	ppu.cycle()
	
	
	
	  'ppu_clock()
	
	
	
	
'frame_complete = true
 
	
	
	'	if (this.nSystemClockCounter mod 6) Then

	'apu.apu_cycle
	'EndIf	 
	
'apu.apu_cycle
	
	'
	'	if (this.nSystemClockCounter mod 3 =  0) Then

	''apu.apu_cycle
	'EndIf	 

	'// The CPU runs 3 times slower than the PPU so we only call its
	'// clock() function every 3 times this function is called. We
	'// have a global counter to keep track of this.
	
		
	if (this.nSystemClockCounter mod 3 =  0) Then
		If this.dma_transfer Then
		'	
			If this.dma_dummy Then
				
				if (this.nSystemClockCounter mod 2 =  1) Then
					this.dma_dummy = FALSE
				End If
		'		
			Else
					if (this.nSystemClockCounter mod 2 =  0) Then
					this.dma_data = this._read(this.dma_page shl 8 or this.dma_addr)
					Else
				
				
				   
				   
				   
				      
						'pOAM[this.dma_addr] = this.dma_data
						
						
						
						
						
						
						this.dma_addr+=1
				   		
				      if this.dma_addr = 0 Then
							this.dma_transfer = FALSE
							this.dma_dummy = TRUE
						EndIf
		
			 	
				
		      End If
				
		
	End If
     
		Else
			'	while(cycleCount < 29780)
					
			cpu._clock()		
	
'cycleCount +=1
		
	'wend
	
			

 

		End If


	End If ' ends nSystemClockCounter mod 3 =  0
	
	
	



  'dim bAudioSampleReady as bool = false
  'this.dAudiotime += this.dAudioTimePerNESClock
  '	if (dAudioTime >= dAudioTimePerSystemSample) then
		'this.dAudioTime -= this.dAudioTimePerSystemSample 
		'this.dAudioSample = apu.mix() 
		'bAudioSampleReady = true 
  '	end if 
  '



 


	
		'// The PPU is capable of emitting an interrupt to indicate the
	'// vertical blanking period has been entered. If it has, we need
	'// to send that irq to the CPU.
	
	'and not(cpu.getflag(cpu.I))
	'If (getmapper()->irqState() and not(cpu.getflag(cpu.I))) Then

	'	GetMapper()->irqClear()
	'	cpu.irq()
	'	
	'else
		
		
		
'	End If
	
	'if (ppu_nmi)  Then 
	'	ppu_nmi = false
	'	cpu.nmi() 
	'End If
		

	this.nSystemClockCounter+=1
End Sub


sub bus.NEW_NMI_AND_IRQ
	
	If (getmapper()->irqState() or this.mapperirqwanted or this.frameirqwanted) Then
   cpu.irqWanted = true
	else 
	cpu.irqWanted = false 	 

End If

	
	
	   'if (ppu.ppu_nmi)  Then
      ' 	beep
		'ppu.ppu_nmi = false
      ' cpu.nmiWanted = true
      '   EndIf
	
	
	
End Sub


    	


sub bus.OLD_NMI_AND_IRQ
	
'this crashes marios time machine and mortal kombat 4 fix it  
	If (getmapper()->irqState() and not(cpu.getflag(cpu.I))) Then

	GetMapper()->irqClear()
		 cpu.irq()
			 	 
End If

    	'if (ppu.ppu_nmi)  Then
      ' 		
		'ppu.ppu_nmi = false
 		'cpu.nmi()

    	'EndIf
	
End Sub




sub bus.new_funcs()

'NEW PPU WIP////////////
'ppu.ppu_nmi = false


    	'if (ppu.ppu_nmi)  Then
      ' 		
		'ppu.ppu_nmi = false
 		'cpu.nmi()

    	'EndIf
    	
    	
' if (ppu.ppu_nmi)  Then 
'     	
'  
'		ppu.ppu_nmi = false
'		cpu.nmi() 
'   End If
'     
'     
'   if (this.nSystemClockCounter mod 3 =  0) Then
'		If this.dma_transfer Then
'		'	
'			If this.dma_dummy Then
'				
'				if (this.nSystemClockCounter mod 2 =  1) Then
'					this.dma_dummy = FALSE
'				End If
'		'		
'			Else
'					if (this.nSystemClockCounter mod 2 =  0) Then
'					this.dma_data = this._read(this.dma_page shl 8 or this.dma_addr)
'					Else
'				
'				
'				   
'				   
'				   
'				      
'					' 	ppu.pOAM[this.dma_addr] = this.dma_data
'						
'						
'						
'						
'						
'						
'						this.dma_addr+=1
'				   		
'				      if this.dma_addr = 0 Then
'							this.dma_transfer = FALSE
'							this.dma_dummy = TRUE
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
'			'	while(cycleCount < 29780)
'					
'			cpu._clock()		
'	
''cycleCount +=1
'		
'	'wend
'	
'			
'
' 
'
'		End If
'
'
'	End If ' ends nSystemClockCounter mod 3 =  0
'	   this.nSystemClockCounter+=1
   
 if(this.cycles =  0) then
 this.cycles = 3
 ' 
 ' 	If (getmapper()->irqState() and not(cpu.getflag(cpu.I))) Then
 '
 '	GetMapper()->irqClear()
 '		 cpu.irq()
 '			 	 
 'End If
'
'
'
'    
''
if CPU_CHOOSE then
this.OLD_NMI_AND_IRQ
else
this.NEW_NMI_AND_IRQ
end if


	
' 	If (getmapper()->irqState() or this.mapperirqwanted or this.frameirqwanted) Then
'    cpu.irqWanted = true
' 	else 
' 	cpu.irqWanted = false 	 
''
' End If



   '     	if (ppu.ppu_nmi)  Then 
   ' 
   ' 
	'	ppu.ppu_nmi = false
	'	 cpu.nmi()
	'	 this.cpu.nmiWanted = true
	'EndIf
        	
        

'///////////////////////	


        	
        	
     ' new dma tested on old bus clock and its fine  	
      if(this.inDma = false) then
      
       getmapper()->clock_irq ' mapper69 IRQ WIP 
        this.cpu._clock()
        
 






        	
        else 
        
        if(this.dmaTimer > 0) then
         if((this.dmaTimer and 1) =  0)then
          ' // even cycles are write to ppu
           this.ppu._write(4, this.dmaValue) 
         else  
          ' // odd cycles are read for value
         this.dmaValue = this._read(this.dmaBase + ((this.dmaTimer \ 2) and &Hff))
           
           
         endif
        endif
         
        this.dmaTimer+=1
                if(this.dmaTimer = 513)  then
          this.dmaTimer = 0 
          this.inDma = false 
        endif
        
        
      endif
      
      
      
      
      
      
      
      
      
      
      
      
    this.apu.apu_cycle()

     end if
     
     
    this.ppu.cycle()


    this.cycles-=1










 'this.ppu._cycle()


    
    
 
   
	'// The PPU is capable of emitting an interrupt to indicate the
	'// vertical blanking period has been entered. If it has, we need
	'// to send that irq to the CPU.
	
	'and not(cpu.getflag(cpu.I))

	'
	
	
End Sub



	function bus._clock() as bool
 'old_funcs_tests()
	'old_funcs()
	new_funcs()
	
	
	return false'bAudioSampleReady
	
End function
	
	
	
	
	
	
	
	
	
	
	
	
	
Sub bus.getsamples(bytes_data() As double, count As uint16_t) 
    '// apu returns 29780 or 29781 samples (0 - 1) for a frame
   ' // we need count values (0 - 1)
   ' dim samples As outputs ptr = this.apu.getOutput() 
  this.APU.getOutput()
    dim runAdd As long= (29780 / count) 
    dim total As double =  0
    dim inputPos As long = 0 
    dim running As long  = 0 
    for i As uint16_t = 0 To count -1  
      running += runAdd 
      total = 0 
      Dim avgCount As uint16_t = running And &Hffff 
      
      for  j As uint16_t = inputPos  To(inputPos + avgCount)  - 1
      total += this.apu._output(j) 'samples->apu_output(j)
       'cast(double,samples)[j]
      Next
      'rnd * 2-1*0.15
      'if bytes_data <> null then
      bytes_data(i) = total / avgCount 'this.apu._output(1)'total '/ avgCount 
      'end if
      '0.0028454054054054055 '
      inputPos += avgCount 
      running -= avgCount 
    Next
    
    
    
    
        '// apu returns 29780 or 29781 samples (0 - 1) for a frame
   ' // we need count values (0 - 1)
   ' dim samples As outputs ptr = this.apu.getOutput() 
   

   

    ' 'avgcount correct value
    'this.APU.getOutput() ' works for now
    'dim runAdd As double = (29780 / count) 'same as NESJS now
    'dim total As double =  0 'same as NESJS now
    'dim inputPos As ulong = 0 
    'dim running As double  = 0 'same as NESJS now mostly
    'Dim avgCount As double = 0
    '
    'dim j as  uint32_t = 0
    '
    'for i As uint32_t = 0 To count -1  
    '  running += runAdd 
    '  total = 0 
    '  avgCount = int(running) And &Hffff 
    '  
    ' ' if i = 135 then
    '  for  j As uint32_t = inputPos to (inputPos + avgCount) -1
    '  'j  = inputpos
    ' ' while j < (inputPos + avgCount)

    '  total += this.apu._output(j) 'samples->apu_output(j)

    ' ' j+=1
    '  'wend
    '  next
    '  
    '  'fixed its just truncating for now
    ' ' print # audoutdata, str(total / avgCount)' / avgCount 
    '  
    '    'its correct but freebasic rounds after maximum floating point decimal 
    '  bytes_data(i) = total / avgCount 'this.apu._output(1)'total '/ avgCount
    '  

    '  

    '  inputPos += avgCount 
    '  running -= avgCount 
    'Next

    
    
    
    
    
    
    
    
    
End Sub


sub bus.SetSampleFrequency(sample_rate as uint32_t )
	this.dAudioTimePerSystemSample  = 2.267573696145125 '1.0 /   cast(double, sample_rate)
	this.dAudioTimePerNESClock  = 1.0 / 5369318.0 '// PPU Clock Frequency
	
End Sub










#include "PPU.bas"

'#include "nes\olc6502_new_2.bas"

'#include "olc6502.bas"
#include "CPU.bas" ' UNCHECK WHEN FIXED

#include "NES\olc6502_BACKUP2.bas"



'#include "CPU_fixes.bas"

#include "APU.bas"	
	


























'