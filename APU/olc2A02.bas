#Include once "windows.bi"
#Include Once"crt.bi"
'#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"
#include once "file.bi"

'finished
dim dutycycles as ubyte 
'finished
dim lengthloadvalues(32) as uint8_t => { 10, 254, 20,  2, 40,  4, 80,  6,_ 
							       				 160,   8, 60, 10, 14, 12, 26, 14,_
							         			  12,  16, 24, 18, 48, 20, 96, 22,_
							                   192,  24, 72, 26, 16, 28, 32, 30}
							                   
							                   
type outputs
	outputoffset as uint32_t
	apu_output as double ptr
	
	
End Type
							                   						                   
		
type APU ' finished
	
	declare constructor ()
	
	'nes as any ptr 
	
	bus as _bus ptr
	
	lengthloadvalues(32) as uint8_t => { 10, 254, 20,  2, 40,  4, 80,  6,_
							       				 160,   8, 60, 10, 14, 12, 26, 14,_
							         			  12,  16, 24, 18, 48, 20, 96, 22,_
							                   192,  24, 72, 26, 16, 28, 32, 30}
							                   
	dutycycles(4,8) as uint8_t = { {0, 1, 0, 0, 0, 0, 0, 0},_
    										 {0, 1, 1, 0, 0, 0, 0, 0},_
    										 {0, 1, 1, 1, 1, 0, 0, 0},_
    										 {1, 0, 0, 1, 1, 1, 1, 1}}
	
	trianglesteps(32) as uint8_t => {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5,  4,  3,  2,  1,  0, _
    										   0,  1,  2,  3,  4,  5,  6, 7, 8, 9, 10, 11, 12, 13, 14, 15}
    										   
	noiseLoadValues(16) as uint16_t => { _
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068 _
    }
	
	dmcLoadValues(16) as uint16_t => { _
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54 _
    }

	apu_output(29781) as double
'//////////////////////////////////////////////////////////////////////////////////////////////	
	
	declare sub apu_reset() ' finished
	declare sub apu_cycle() 'finished 
	declare sub cyclePulse1()'finished 
	declare sub cyclePulse2() 'finished 
	declare sub cycleTriangle() 'finished 
   declare sub cycleNoise() 'finished
  ' declare sub cycleDMC()      'not going to use for now
   declare sub updatesweepP1() ' finished
   declare sub updatesweepP2() ' finished
   declare sub clockquarter() ' finished
   declare sub clockhalf() ' finished
   declare function mix() as double ' finished
   declare sub handleframecounter() ' finished
   declare function getOutput() as outputs ptr' MAYBE
   declare function read_apu(addr1 as uint16_t) as uint8_t ' finished
   declare sub write_apu(addr1 as uint16_t,data1 as uint8_t) ' finished
   
'APU VARIABLES///////////////////	
	outputoffset as uint32_t
	framecounter as uint32_t
	step5Mode as boolean
	interruptInhibit as boolean
	enablenoise as boolean
	enabletriangle as boolean
	enablePulse2 as boolean
	enablePulse1  as boolean
	
	'pulse 1
	 p1Timer as uint32_t = 0
    p1TimerValue as uint32_t = 0
    p1Duty as uint32_t = 0
    p1DutyIndex as uint32_t = 0
    p1Output as uint32_t = 0
    p1CounterHalt as boolean = false
    p1Counter as uint32_t = 0
    p1Volume as uint32_t = 0
    p1ConstantVolume as boolean = false
    p1Decay as uint32_t = 0
    p1EnvelopeCounter as uint32_t = 0
    p1EnvelopeStart as boolean = false
    p1SweepEnabled as boolean = false
    p1SweepPeriod as uint32_t = 0
    p1SweepNegate as boolean = false
    p1SweepShift as uint32_t = 0
    p1SweepTimer as uint32_t = 0
    p1SweepTarget as uint32_t = 0
    p1SweepMuting as boolean = true
    p1SweepReload as boolean = false
	
	'pulse 2
	 p2Timer as uint32_t = 0
    p2TimerValue as uint32_t = 0
    p2Duty as uint32_t = 0
    p2DutyIndex as uint32_t = 0
    p2Output as uint32_t = 0
    p2CounterHalt as boolean = false
    p2Counter as uint32_t = 0
    p2Volume as uint32_t = 0
    p2ConstantVolume as boolean = false
    p2Decay as uint32_t = 0
    p2EnvelopeCounter as uint32_t = 0
    p2EnvelopeStart as boolean = false
    p2SweepEnabled as boolean = false
    p2SweepPeriod as uint32_t = 0
    p2SweepNegate as boolean = false
    p2SweepShift as uint32_t = 0
    p2SweepTimer as uint32_t = 0
    p2SweepTarget as uint32_t = 0
    p2SweepMuting as boolean = true
    p2SweepReload as boolean = false

	'triangle
	 triTimer as uint32_t = 0
    triTimerValue as uint32_t = 0
    triStepIndex as uint32_t = 0
    triOutput as uint32_t = 0
    triCounterHalt as boolean = false
    triCounter as uint32_t = 0
    triLinearCounter as uint32_t = 0
    triReloadLinear as boolean = false
    triLinearReload as uint32_t = 0
    
	'noise
	 noiseTimer as uint32_t = 0
    noiseTimerValue as uint32_t = 0
    noiseShift as uint32_t = 1
    noiseTonal as boolean = false
    noiseOutput as uint32_t = 0 
    noiseCounterHalt as boolean = false
    noiseCounter as uint32_t = 0
    noiseVolume as uint32_t = 0
    noiseConstantVolume as uint32_t = false
    noiseDecay as uint32_t = 0
    noiseEnvelopeCounter as uint32_t = 0
    noiseEnvelopeStart as boolean = false
    
	'dmc
	 'dmcInterrupt as boolean = false
    'dmcLoop as boolean = false
    'dmcTimer as uint32_t = 0
    'dmcTimerValue as uint32_t = 0
    'dmcOutput as uint32_t = 0
    'dmcSampleAddress as uint32_t = &Hc000
    'dmcAddress as uint32_t = &Hc000
    'dmcSample as uint32_t = 0
    'dmcSampleLength as uint32_t = 0
    'dmcSampleEmpty as boolean = true
    'dmcBytesLeft as uint32_t = 0
    'dmcShifter as uint32_t = 0
    'dmcBitsLeft as uint32_t = 8
    'dmcSilent as boolean = true

End Type

'sub APU.cycleDmc()
'	
'	
'	
'End Sub

constructor apu()
 
'this.nes = @NES



this.apu_reset()
End Constructor

'MAYBE
function APU.getOutput() as outputs ptr
   dim ret as outputs
   ret.outputoffset = this.outputoffset
   ret.apu_output = @this.apu_output(0)
   


	return @ret
End Function



sub APU.apu_cycle() 'finished 
	
	'if (this.framecounter = 29830 and this.step5Mode) or _
	'	this.framecounter = 37282 then
	'	this.frameCounter = 0
'	end if
	 this.frameCounter+=1
	
	'this.handleFrameCounter()
	'this.cycleTriangle()
	'this.cyclePulse1()
	'this.cyclePulse2()
	'this.cycleNoise()
	'this.cycleDmc()
	
	this.apu_output(this.outputOffset) = this.mix():this.outputOffset +=1
	if this.outputoffset = 29781 then
		 this.outputOffset = 29780
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
	
	dim _output as uint8_T = this.dutycycles(this.p1Duty,this.p1DutyIndex)
	
	if _output = 0 or this.p1SweepMuting or p1counter = 0 then
		this.p2Output = 0
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
	
	dim _output as uint8_T = this.dutycycles(this.p2Duty,this.p2DutyIndex)
	
	if _output = 0 or this.p2SweepMuting or p2counter = 0 then
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
 		if this.interruptInhibit = 0 then
 			'this.nes.frameIrqWanted = true
 		EndIf
 	elseif(this.frameCounter = 37281) then
 		 this.clockQuarter()
 	  	 this.clockHalf()
 	end if
 		
 End Sub







sub APU.write_apu(addr1 as uint16,data1 as uint8_t) ' finished
	
	
	


	
	
	
	
	
	select case addr1
		
		case &H4000
			this.p1Duty = (data1 and &HC0) shr 6
			this.p1Volume = data1 and &HF
			this.p1CounterHalt = iif((data1 and &H20)  > 0,true,false)
			this.p1ConstantVolume = iif((data1 and &H10) > 0,true,false)

		case &H4001
		  this.p1SweepEnabled = iif((data1 and &H80) > 0,true,false)
        this.p1SweepPeriod = (data1 and &H70) shr 4 
        this.p1SweepNegate = iif((data1 and &H08) > 0,true,false)
        this.p1SweepShift = data1 and &H7
        this.p1SweepReload = true
			
		 this.updatesweepP1()
			
		case &H4002
		  this.p1Timer and= &H700 
        this.p1Timer or= data1 
		 this.updatesweepP1()
		case &H4003
		  this.p1Timer and= &Hff
        this.p1Timer or= (data1 and &H7) shl 8 
        this.p1DutyIndex = 0 
        if (this.enablePulse1) then
          this.p1Counter = this.lengthLoadValues((data1 and &Hf8) shr 3)
        end if
        this.p1EnvelopeStart = true 
       this.updateSweepP1() 
			
		case &H4004
			this.p2Duty = (data1 and &HC0) shr 6
			this.p2Volume = data1 and &HF
			this.p2CounterHalt = iif((data1 and &H20)  > 0,true,false)
			this.p2ConstantVolume = iif((data1 and &H10) > 0,true,false)
			
		case &H4005
		  this.p2SweepEnabled = iif((data1 and &H80) > 0,true,false)
        this.p2SweepPeriod = (data1 and &H70) shr 4 
        this.p2SweepNegate = iif((data1 and &H08) > 0,true,false)
        this.p2SweepShift = data1 and &H7
        this.p2SweepReload = true
		case &H4006
			
		  this.p2Timer and= &H700 
        this.p2Timer or= data1 
		 this.updatesweepP2()
		case &H4007
			  this.p2Timer and= &Hff
        this.p2Timer or= (data1 and &H7) shl 8 
        this.p2DutyIndex = 0 
        if (this.enablePulse2) then
          this.p2Counter = this.lengthLoadValues((data1 and &Hf8) shr 3)
        end if
        this.p2EnvelopeStart = true 
       this.updateSweepP2() 
		case &H4008	
		  this.triCounterHalt = iif((data1 and &H80) > 0,true,false)  
        this.triLinearReload = data1 and &H7f
         
			'this.triReloadLinear = true
	
		case &H400A
			
		  this.triTimer and= &H700 
        this.triTimer or= data1 
		case &H400B
		this.triTimer and= &Hff 
        this.triTimer or= (data1 and &H7) shl 8 
        if(this.enableTriangle)  then
          this.triCounter = this.lengthLoadValues((data1 and &Hf8) shr 3) 
        end if
        this.triReloadLinear = true 
       
      
		case &H400C
		 this.noiseCounterHalt = iif((data1  and &H20) > 0,true,false) 
        this.noiseConstantVolume = iif((data1  and &H10) > 0,true,false) 
        this.noiseVolume = data1  and &Hf 
		
		case &H400E
		  this.noiseTonal = iif((data1 and &H80) > 0,true,false) 
        this.noiseTimer = this.noiseLoadValues(data1 and &Hf) - 1 
		case &H400F
			        if(this.enableNoise)  then
          this.noiseCounter = this.lengthLoadValues((data1 and &Hf8) shr 3) 
			        end if
        this.noiseEnvelopeStart = true 
			
			
		case &H4010
			  '    this.dmcInterrupt = iif((data1 and &H80) > 0,true,false)  
      '  this.dmcLoop = (data1 and &H40) > 0 
       ' this.dmcTimer = this.dmcLoadValues(data1 and &Hf) - 1 
        'if(this.dmcInterrupt = 0)  then
        '  'this.nes.dmcIrqWanted = false 
        'end if
		case &H4011	
			  'this.dmcOutput = data1 and &H7f
		case &H4012
			'this.dmcSampleAddress = &Hc000 or (data1 shl 6)
		case &H4013
      'this.dmcSampleLength = (data1 shl 4) + 1
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
          'if(this.dmcBytesLeft = 0) then
          '  this.dmcBytesLeft = this.dmcSampleLength 
            'this.dmcAddress = this.dmcSampleAddress 
          'end if
        else
        '  this.dmcBytesLeft = 0 
        end if
       'this.nes.dmcIrqWanted = false 
		
		case &H4017	
		 this.step5Mode = (data1 and &H80) > 0 
        this.interruptInhibit = iif((data1 and &H40) > 0,true,false) 
        if(this.interruptInhibit) then
        '  this.nes.frameIrqWanted = false
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
			dim ret as uint8_t = 0
			ret or= iif((this.p1Counter > 0),&H1,0)
			ret or= iif((this.p2Counter > 0),&H2,0)
			ret or= iif((this.triCounter > 0),&H4,0)
		   ret or= iif((this.noiseCounter > 0),&H8,0)
		  ' ret or= iif(((this.dmcBytesLeft > 0),&H10,0)
		   'ret or= iif(((this.nes.frameIrqWanted > 0),&H40,0)
		   'ret or= iif(((this.nes.dmcIrqWanted > 0),&H80,0)
		   'this.nes.frameIrqWanted = false
		   return ret
	   End If
		return 0
End Function

sub APU.updatesweepP2() 'finished
	
	
   dim change as int16_t = this.p2Timer shr this.p2SweepShift
  	
  	if this.p2SweepNegate then
  		change = (-change)-1 
  	EndIf
  	
  	    this.p2SweepTarget = this.p2Timer + change 
    if(this.p2SweepTarget > &H7ff or this.p2Timer < 8)  then
      this.p2SweepMuting = true 
    else  
      this.p2SweepMuting = false 
    end if
End Sub


sub APU.updatesweepP1() 'finished
  	dim change as int16_t = this.p1Timer shr this.p1SweepShift
  	
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
	dim tnd as double = (0.00851 * this.triOutput + _
      						0.00494 * this.noiseOutput)' + _') '+
      						'0.00335 * this.dmcOutput)
      						
   dim pulse as double = 0.0752 * (this.p1Output + this.p2Output)
	
	
	return tnd + pulse
	
End Function




sub APU.apu_reset() ' finished
	
erase this.apu_output
	
this.outputoffset = 0

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

'dmc
'this.dmcInterrupt  = false
'this.dmcLoop  = false
'this.dmcTimer  = 0
'this.dmcTimerValue  = 0
'this.dmcOutput  = 0
'this.dmcSampleAddress  = &Hc000
'this.dmcAddress  = &Hc000
'this.dmcSample  = 0
'this.dmcSampleLength  = 0
'this.dmcSampleEmpty  = true
'this.dmcBytesLeft  = 0
'this.dmcShifter  = 0
'this.dmcBitsLeft  = 8
'this.dmcSilent  = true
'	
	
End Sub




'sleep