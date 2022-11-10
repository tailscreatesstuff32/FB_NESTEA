#pragma once


'#include once "APU.bi"
dim shared dmc_reg1 as uint8_t
dim shared dmc_reg2 as uint8_t
dim shared dmc_reg3 as uint8_t
dim shared dmc_reg4 as uint8_t

dim shared  p1_reg as uint32_t
'dim shared  p2_reg as uint_32
'dim shared  tri_reg as uint_32
'dim shared  noise_reg as uint_32


type PULSEREG 
	R0 as uint8_t
	R1 as uint8_t
	R2 as uint8_t
	R3 as uint8_t
	
	
End Type

type TRIREG 
	R0 as uint8_t
	R1 as uint8_t
	R2 as uint8_t
	R3 as uint8_t
End Type

type NOISEREG 
	R0 as uint8_t
	R1 as uint8_t
	R2 as uint8_t
	R3 as uint8_t
End Type

type DMCREG 
	R0 as uint8_t
	R1 as uint8_t
	R2 as uint8_t
	R3 as uint8_t
End Type

dim shared pulsereg1 as PULSEREG
dim shared pulsereg2 as PULSEREG
dim shared _trireg as TRIREG
dim shared _noisereg as NOISEREG
dim shared _dmcreg as DMCREG

constructor apu()

this.apu_reset()

End Constructor

sub APU.ConnectBus(n as any ptr)
	
  this.bus = n
	
End Sub

'NOT SURE
function APU.getOutput() as outputs ptr 'any ptr
	dim tempouts1 as outputs
	
	
	'tempouts1 = this.out1
	
	
	'dim ret(1) as any ptr' = {(this.outputoffset,@this.apu_output(0))}
	'
	'ret(0) = @this.outputoffset  'uint32_t 
	'ret(1) = @this.apu_output(0) ' double
	'
	'this.outputoffset = 0
	'
	
  '' dim ret as outputs
  '' ret.outputoffset = this.outputoffset
  '' ret.apu_output = @this.apu_output(0)
  ' 

  'tempouts1.apu_output
   this._outputoffset = 0

	return @tempouts1 '@ret(0)
	
	
End Function
sub APU.cycleNoise() 'finished
	   if(this.noiseTimerValue <> 0) then
      this.noiseTimerValue-=1
      else  
      this.noiseTimerValue = this.noiseTimer 
      dim feedback as uint8_t = this.noiseShift and &H1
      if(this.noiseTonal) then
        feedback xor= (this.noiseShift and &H40) shr 6 
        else  
        feedback xor= (this.noiseShift and &H2) shr 1 
      end if
      this.noiseShift shr= 1 
      this.noiseShift or= feedback shl 14 
	   end if
    if(this.noiseCounter = 0 or (this.noiseShift and &H1) = 1) then
      this.noiseOutput = 0 
     else  
      this.noiseOutput = iif(this.noiseConstantVolume, this.noiseVolume,this.noiseDecay)
    end if
	
	
	
End Sub
sub APU.cyclePulse1() 'finished 
		if this.p1TimerValue <> 0 then
		this.p1TimerValue-=1
	else
	this.p1TimerValue = (this.p1Timer * 2) + 1
	 this.p1DutyIndex += 1
	 this.p1DutyIndex and= &H7
	EndIf
	
	dim _output1 as uint8_T = this.dutycycles(this.p1Duty,this.p1DutyIndex)
	
	if _output1 = 0 or this.p1SweepMuting or this.p1counter = 0 then
		this.p1Output = 0
	else	
		 this.p1Output = iif(this.p1ConstantVolume,this.p1Volume,this.p1Decay)
	EndIf
End Sub
sub APU.cyclePulse2() 'finished 
	if this.p2TimerValue <> 0 then
		this.p2TimerValue-=1
	else
	this.p2TimerValue = (this.p2Timer * 2) + 1
	 this.p2DutyIndex += 1
	 this.p2DutyIndex and= &H7
	EndIf
	
	dim _output1 as uint8_T = this.dutycycles(this.p2Duty,this.p2DutyIndex)
	
	if _output1 = 0 or this.p2SweepMuting or this.p2counter = 0 then
		this.p2Output = 0
		
	else	
		 this.p2Output = iif(this.p2ConstantVolume,this.p2Volume,this.p2Decay)
	EndIf
	
End Sub


sub APU.cycleTriangle() 'finished 
	if this.triTimerValue <> 0 then
		this.triTimerValue-=1
	else
		this.tritimervalue = this.triTimer
		if this.triCounter <> 0 and this.triLinearCounter <> 0 then
		this.triOutput = this.trianglesteps(this.triStepIndex):this.triStepIndex+=1
		if this.tritimer < 2 then
		this.triOutput = 7.5
		EndIf
		this.triStepIndex and= &H1f
		end if
		end if
	
End Sub

sub APU.apu_cycle() 'finished 
	
	if (this.framecounter = 29830 and this.step5Mode = false) or _
		this.framecounter = 37282 then
		this.frameCounter = 0
	end if
	'
	 this.frameCounter+=1 ' works similar to nesjs's framecounter
	
	this.handleFrameCounter()
	
	this.cycleTriangle()
	this.cyclePulse1()
	this.cyclePulse2()
	this.cycleNoise()
	this.cycleDmc()
	''
	'
	''setconsoletitle(str(this.mix()))
	'
	''this.mix()

	'
	this._output(this._outputOffset) = this.mix():this._outputOffset +=1'this.mix():this._outputOffset +=1
	if this._outputOffset = 29781 then
		this._outputOffset = 29780
   end if

End Sub



sub APU.apu_reset() ' finished
	
'erase this.apu_output
erase this._output 
this._outputoffset = 0

this.framecounter = 0

this.interruptInhibit = false 
this.step5Mode        = false

this.enablenoise    = false
this.enabletriangle = false
this.enablePulse2   = false
this.enablePulse1   = false
	
'pulse 1
this.p1Timer  = 0
this.p1TimerValue  = 0
this.p1Duty  = 0
this.p1DutyIndex  = 0
this.p1Output  = 0
this.p1CounterHalt  = false
this.p1Counter  = 0
this.p1Volume  = 0
this.p1ConstantVolume  = false
this.p1Decay  = 0
this.p1EnvelopeCounter  = 0
this.p1EnvelopeStart  = false
this.p1SweepEnabled  = false
this.p1SweepPeriod  = 0
this.p1SweepNegate  = false
this.p1SweepShift  = 0
this.p1SweepTimer  = 0
this.p1SweepTarget  = 0
this.p1SweepMuting  = true
this.p1SweepReload  = false



'worry about this next////////////////////////////////////
'pulse 2
this.p2Timer  = 0
this.p2TimerValue  = 0
this.p2Duty  = 0
this.p2DutyIndex  = 0
this.p2Output  = 0
this.p2CounterHalt  = false
this.p2Counter  = 0
this.p2Volume  = 0
this.p2ConstantVolume  = false
this.p2Decay  = 0
this.p2EnvelopeCounter  = 0
this.p2EnvelopeStart  = false
this.p2SweepEnabled  = false
this.p2SweepPeriod  = 0
this.p2SweepNegate  = false
this.p2SweepShift  = 0
this.p2SweepTimer  = 0
this.p2SweepTarget  = 0
this.p2SweepMuting  = true
this.p2SweepReload  = false

'triangle
this.triTimer  = 0
this.triTimerValue  = 0
this.triStepIndex  = 0
this.triOutput  = 0
this.triCounterHalt  = false
this.triCounter  = 0
this.triLinearCounter  = 0
this.triReloadLinear  = false
this.triLinearReload  = 0

'noise
this.noiseTimer  = 0
this.noiseTimerValue  = 0
this.noiseShift  = 1
this.noiseTonal  = false
this.noiseOutput  = 0 
this.noiseCounterHalt  = false
this.noiseCounter  = 0
this.noiseVolume  = 0
this.noiseConstantVolume  = false
this.noiseDecay  = 0
this.noiseEnvelopeCounter  = 0
this.noiseEnvelopeStart  = false
'//////////////////////////////////////////////////////////////

'dmc
this.dmcInterrupt  = false
this.dmcLoop  = false
this.dmcTimer  = 0
this.dmcTimerValue  = 0
this.dmcOutput  = 0
this.dmcSampleAddress  = &Hc000
this.dmcAddress  = &Hc000
this.dmcSample  = 0
this.dmcSampleLength  = 0
this.dmcSampleEmpty  = true
this.dmcBytesLeft  = 0
this.dmcShifter  = 0
this.dmcBitsLeft  = 8
this.dmcSilent  = true
	
	
End Sub


   '   this.dmcTimerValue-=1
   ' else 
	'this.dmcTimerValue = this.dmcTimer
	'
	'  if((this.dmcShifter and &H1) = 0) then
   '       if(this.dmcOutput >= 2) then
   '         this.dmcOutput -= 2 
   '       end if
   '     else  
   '       if(this.dmcOutput <= 125) then
   '         this.dmcOutput += 2 
   '       end if
	'  end if
	'this.dmcShifter shr= 1
	'this.dmcBitsLeft-=1
	'      if(this.dmcBitsLeft = 0) then
   '     this.dmcBitsLeft = 8 
   '     if(this.dmcSampleEmpty)  then
   '       this.dmcSilent = true 
   '     else
   '       this.dmcSilent = false 
   '       this.dmcShifter = this.dmcSample 
   '       this.dmcSampleEmpty = true 
   '    end if
	'      end if







sub APU.cycleDMC()
	
	 if(this.dmcTimerValue <> 0) then
			this.dmcTimerValue-=1
	else
 this.dmcTimerValue = this.dmcTimer
   if(this.dmcSilent  = 0) then
   	
   	  if((this.dmcShifter and &H1) = 0) then
          if(this.dmcOutput >= 2)  then
            this.dmcOutput -= 2 
          end if
   	  else 
          if(this.dmcOutput <= 125) then
            this.dmcOutput += 2 
          EndIf
   	  EndIf
   EndIf
	
	
	this.dmcShifter shr= 1
	this.dmcBitsLeft-=1
	
	 if(this.dmcBitsLeft = 0) then
	 	 this.dmcBitsLeft = 8
	 	  if(this.dmcSampleEmpty) then
	 	  	 this.dmcSilent = true
        else
	 	this.dmcSilent = false
	 	 this.dmcShifter = this.dmcSample
	 	 this.dmcSampleEmpty = true
	 EndIf
	
	
	endif
	
	
	
	
 end if
	
	    if(this.dmcBytesLeft > 0 and this.dmcSampleEmpty) then


	      this.dmcSampleEmpty = false
      this.dmcSample = bus->_read(this.dmcAddress) 
      this.dmcAddress+=1
      if(this.dmcAddress = &H10000)  then
        this.dmcAddress = &H8000 
      end if
      
      
      this.dmcBytesLeft-=1
            if(this.dmcBytesLeft = 0 and this.dmcLoop)  then
        this.dmcBytesLeft = this.dmcSampleLength 
        this.dmcAddress = this.dmcSampleAddress 
            elseif(this.dmcBytesLeft = 0 and this.dmcInterrupt) then
     '   this.nes.dmcIrqWanted = true 
            end if
	    end if
      
	
     '
End Sub



sub APU.write_apu(addr1 as uint16,data1 as uint8_t) ' finished
	
	
	


	
	
	
	
	
	select case addr1
		
		case &H4000
		
			pulsereg1.R0 = data1
			
			this.p1Duty = (data1 and &HC0) shr 6
			this.p1Volume = data1 and &HF
			this.p1CounterHalt = iif((data1 and &H20)  > 0,true,false)
			this.p1ConstantVolume = iif((data1 and &H10) > 0,true,false)
      
		case &H4001
			
		  pulsereg1.R1 = data1
		  this.p1SweepEnabled = iif((data1 and &H80) > 0,true,false)
        this.p1SweepPeriod = (data1 and &H70) shr 4 
        this.p1SweepNegate = iif((data1 and &H08) > 0,true,false)
        this.p1SweepShift = data1 and &H7
        this.p1SweepReload = true
			
		  this.updatesweepP1()
			
		case &H4002
			 pulsereg1.R2 = data1
			'p1_reg or= data1 shl 16
		  'p1_reg += data1
		  this.p1Timer and= &H700 
        this.p1Timer or= data1 
		 this.updatesweepP1()
		 
		case &H4003
		'  p1_reg = (data1 shl 8)
		  
		  
		  pulsereg1.R3 = data1
		  
		  this.p1Timer and= &Hff
        this.p1Timer or= (data1 and &H7) shl 8 
        this.p1DutyIndex = 0 
        if (this.enablePulse1) then
          this.p1Counter = this.lengthLoadValues((data1 and &Hf8) shr 3)
          '  setconsoletitle( str( this.p2Counter))
       
        end if
     
        this.p1EnvelopeStart = true 
       this.updateSweepP1() 
			
		case &H4004
				
			pulsereg2.R0 = data1
			this.p2Duty = (data1 and &HC0) shr 6
			'this.p2Duty = 1
			this.p2Volume = data1 and &HF
			this.p2CounterHalt = iif((data1 and &H20)  > 0,true,false)
			this.p2ConstantVolume = iif((data1 and &H10) > 0,true,false)
			
		case &H4005
			pulsereg2.R1 = data1
		  this.p2SweepEnabled = iif((data1 and &H80) > 0,true,false)
        this.p2SweepPeriod = (data1 and &H70) shr 4 
        this.p2SweepNegate = iif((data1 and &H08) > 0,true,false)
        this.p2SweepShift = data1 and &H7
        this.p2SweepReload = true
        this.updateSweepP2() 
		case &H4006
			pulsereg2.R2 = data1
		  this.p2Timer and= &H700 
        this.p2Timer or= data1 
		 this.updatesweepP2()
		case &H4007
				pulsereg2.R3 = data1
			  this.p2Timer and= &Hff
        this.p2Timer or= (data1 and &H7) shl 8 
        this.p2DutyIndex = 0 
        if (this.enablePulse2) then
          this.p2Counter = this.lengthLoadValues((data1 and &Hf8) shr 3)
        end if
        this.p2EnvelopeStart = true 
       this.updateSweepP2() 
		case &H4008	
			_trireg.R0 = data1
		  this.triCounterHalt = iif((data1 and &H80) > 0,true,false)  
        this.triLinearReload = data1 and &H7f
         
			'this.triReloadLinear = true
	
		case &H400A
			_trireg.R1 = data1
		  this.triTimer and= &H700 
        this.triTimer or= data1 
		case &H400B
				_trireg.R2 = data1
		this.triTimer and= &Hff 
        this.triTimer or= (data1 and &H7) shl 8 
        if(this.enableTriangle)  then
          this.triCounter = this.lengthLoadValues((data1 and &Hf8) shr 3) 
        end if
        this.triReloadLinear = true 
       
      
		case &H400C
			_noisereg.R0 = data1
		 this.noiseCounterHalt = iif((data1  and &H20) > 0,true,false) 
        this.noiseConstantVolume = iif((data1  and &H10) > 0,true,false) 
        this.noiseVolume = data1  and &Hf 
		
		case &H400E
			_noisereg.R1 = data1
		  this.noiseTonal = iif((data1 and &H80) > 0,true,false) 
        this.noiseTimer = this.noiseLoadValues(data1 and &Hf) - 1 
		case &H400F
				_noisereg.R2 = data1
			        if(this.enableNoise)  then
          this.noiseCounter = this.lengthLoadValues((data1 and &Hf8) shr 3) 
			        end if
        this.noiseEnvelopeStart = true 
			
			
		case &H4010
			
			   _dmcreg.R0 = data1 
			      this.dmcInterrupt = iif((data1 and &H80) > 0,true,false)  
        this.dmcLoop = iif((data1 and &H40) > 0,true,false) 
        this.dmcTimer = this.dmcLoadValues(data1 and &Hf) - 1 
        if(this.dmcInterrupt = 0)  then
'         this.dmcIrqWanted = false 
        end if
		case &H4011	
			  _dmcreg.R1 = data1  
			  this.dmcOutput = data1 and &H7f
		case &H4012
			 _dmcreg.R2 = data1  
			this.dmcSampleAddress = &Hc000 or (data1 shl 6)
		case &H4013
			 _dmcreg.R3 = data1  
      this.dmcSampleLength = (data1 shl 4) + 1
		case &H4015
		  this.enableNoise = iif((data1 and &H08) > 0,true,false) 
        this.enableTriangle = iif((data1 and &H04) > 0,true,false) 
        this.enablePulse2 = iif((data1 and &H02) > 0,true,false) 
        this.enablePulse1 = iif((data1 and &H01) > 0,true,false) 
        if( this.enablePulse1 = 0) then
          this.p1Counter = 0 
        end if
        if( this.enablePulse2 = 0) then
          this.p2Counter = 0 
        end if
        if(this.enableTriangle = 0) then
          this.triCounter = 0 
        end if
        if(this.enableNoise = 0) then
          this.noiseCounter = 0 
        end if
        if((data1 and &H10) > 0)  then
          if(this.dmcBytesLeft = 0) then
            this.dmcBytesLeft = this.dmcSampleLength 
            this.dmcAddress = this.dmcSampleAddress 
          end if
        else
         this.dmcBytesLeft = 0 
        end if
       'this.dmcIrqWanted = false 
		
		case &H4017	
		 this.step5Mode = iif((data1 and &H80) > 0,true,false) 
        this.interruptInhibit = iif((data1 and &H40) > 0,true,false) 
        if(this.interruptInhibit) then
        '  bus->frameIrqWanted = false
        end if
        this.frameCounter = 0
        if(this.step5Mode) then
          this.clockQuarter()
          this.clockHalf()
        end if
		case else
			
			
	End Select
	
	
	
End Sub


sub APU.clockquarter() ' finished
	
  	if(this.triReloadLinear)  then
      this.triLinearCounter = this.triLinearReload 
  	elseif this.triLinearCounter <> 0   then
      this.triLinearCounter-=1 
  	end if
  	
    if(this.triCounterHalt = 0)  then
      this.triReloadLinear = false 
    end if
    
    '// handle envelopes
    if( this.p1EnvelopeStart = 0) then
    	
      if(this.p1EnvelopeCounter <> 0) then
        this.p1EnvelopeCounter-=1
      else  
        this.p1EnvelopeCounter = this.p1Volume 
        if(this.p1Decay <> 0)  then
          this.p1Decay-=1
          else  
          if(this.p1CounterHalt) then
            this.p1Decay = 15 
          end if
        end if
      end if
    
    else 
      this.p1EnvelopeStart = false 
      this.p1Decay = 15 
      this.p1EnvelopeCounter = this.p1Volume 
    end if

    if( this.p2EnvelopeStart = 0) then
      if(this.p2EnvelopeCounter <> 0) then
       this.p2EnvelopeCounter-=1
        else  
         this.p2EnvelopeCounter = this.p2Volume 
          if(this.p2Decay <> 0)  then
           this.p2Decay-=1
            else 
          if(this.p2CounterHalt) then
            this.p2Decay = 15            
     this.noiseEnvelopeStart = false 
      this.noiseDecay = 15 
      this.noiseEnvelopeCounter = this.noiseVolume 
      end if    
     end if
      end if
      
    else 
    this.p2EnvelopeStart = false
      this.p2Decay = 15
      this.p2EnvelopeCounter = this.p2Volume
     end if

    if( this.noiseEnvelopeStart = 0)  then
       if(this.noiseEnvelopeCounter <> 0)  then
        this.noiseEnvelopeCounter-=1
      else  
         this.noiseEnvelopeCounter = this.noiseVolume 
        if(this.noiseDecay <> 0)  then
        this.noiseDecay-=1
          else  
           if(this.noiseCounterHalt) then
           this.noiseDecay = 15 
           end if
         end if
        end if
    else  
      this.noiseEnvelopeStart = false 
      this.noiseDecay = 15 
      this.noiseEnvelopeCounter = this.noiseVolume 
    end if
end sub
function APU.read_apu(addr1 as uint16_t) as uint8_t 'finished
		if addr1 = &H4015 then
			dim ret as uint32_t = 0
			ret or= iif((this.p1Counter > 0),&H1,0)
			ret or= iif((this.p2Counter > 0),&H2,0)
			ret or= iif((this.triCounter > 0),&H4,0)
		   ret or= iif((this.noiseCounter > 0),&H8,0)
		   ret or= iif((this.dmcBytesLeft > 0),&H10,0)
		'   ret or= iif((bus->frameIrqWanted),&H40,0)
		  ' ret or= iif(((this.dmcIrqWanted > 0),&H80,0)
		   bus->frameIrqWanted = false
		   return ret
		End if
	   
		return 0
End Function

sub APU.updatesweepP2() 'finished
	
	
   dim change as integer = this.p2Timer shr this.p2SweepShift
  	
  	if this.p2SweepNegate then
  	change = (-change)  
  	EndIf
  	
  	    this.p2SweepTarget = this.p2Timer + change 
    if(this.p2SweepTarget > &H7ff or this.p2Timer < 8)  then
      this.p2SweepMuting = true 
    else  
      this.p2SweepMuting = false 
    end if
End Sub


sub APU.updatesweepP1() 'finished
  	dim change as integer = this.p1Timer shr this.p1SweepShift
  	
  	if this.p1SweepNegate then
  		change = (-change)-1 
  	EndIf
  	
  	    this.p1SweepTarget = this.p1Timer + change 
    if(this.p1SweepTarget > &H7ff or this.p1Timer < 8)  then
      this.p1SweepMuting = true 
    else  
      this.p1SweepMuting = false 
    end if
  	
  End Sub
function APU.mix() as Double ' finished
	dim tnd as double = (0.00851 * this.triOutput + _ '+ _
     						   0.00494 * this.noiseOutput + _' + _') '+
     				   		0.00335 * this.dmcOutput)
     				   		
   dim pulse as double = 0.00752 * ( this.p1Output + this.p2Output)
	
	'tnd + 
	'tnd +
	'final1
	'
	'
	return ( pulse + tnd   + finalVRC6output + finalN163output )
	'tnd + 
End Function

sub APU.clockhalf() 'finished
	
	    '// decrement length counters
    if( this.p1CounterHalt = 0 and this.p1Counter <> 0)  then
      this.p1Counter-=1 
    end if
    
    if(this.p2CounterHalt = 0 and this.p2Counter <> 0)  then
      this.p2Counter-=1
    end if
    if(this.triCounterHalt = 0 and this.triCounter <> 0)  then
      this.triCounter-=1
    end if
    if(this.noiseCounterHalt = 0 and this.noiseCounter <> 0) then
      this.noiseCounter-=1
    end if
    '// handle sweeps
    if this.p1SweepTimer = 0 and this.p1SweepEnabled and this.p1SweepMuting = 0 and this.p1SweepShift > 0 then
      this.p1Timer = this.p1SweepTarget 
      this.updateSweepP1() 
    end if
    
    if(this.p1SweepTimer = 0 or this.p1SweepReload) then
      this.p1SweepTimer = this.p1SweepPeriod 
      this.p1SweepReload = false 
      else 
      this.p1SweepTimer-=1
    end if

    if this.p2SweepTimer = 0 and this.p2SweepEnabled and this.p2SweepMuting = 0 and this.p2SweepShift > 0 then
   
      this.p2Timer = this.p2SweepTarget 
      this.updateSweepP2() 
    end if
    
    if(this.p2SweepTimer = 0 or this.p2SweepReload) then
      this.p2SweepTimer = this.p2SweepPeriod 
      this.p2SweepReload = false 
      else  
      this.p2SweepTimer-=1
    end if
	
	
	
	
	
	
	
End Sub

' finished





sub APU.handleframecounter() ' finished
 	if this.framecounter =7457 then
 			this.clockquarter()
 	elseif this.framecounter = 14913 then
 			this.clockquarter()
 			this.clockhalf()
 	elseif this.frameCounter  = 22371 then
 		   this.clockQuarter()
 	elseif this.framecounter = 29829 and this.step5Mode = false then
 		this.clockQuarter()
 		this.clockHalf()
 		if this.interruptInhibit = false then
 			this.bus->frameIrqWanted = true
 		EndIf
 	elseif(this.frameCounter = 37281) then
 		 this.clockQuarter()
 	 	 this.clockHalf()
 	end if
 		
 End Sub



