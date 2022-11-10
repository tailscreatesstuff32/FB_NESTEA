#pragma once


#Include once "windows.bi"
#Include Once"crt.bi"
'#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"
#include once "file.bi"
'							                   
type outputs
	outputoffset as uint32_t
	apu_output(29781) as Double

End Type
							                   						                   		
type APU ' finished
	 
	 declare constructor ()
	   
	 'nes as any ptr 
	  
	 bus as _Bus ptr
	 'NSF as _NSF ptr
	 
	declare sub ConnectBus(n as any ptr)
	
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
 
	' apu_output(29781) as double
	

 '//////////////////////////////////////////////////////////////////////////////////////////////	
    
    out1 as outputs
 	 
 	 
    declare sub apu_reset() ' finished
	 declare sub apu_cycle() 'finished 
	 declare sub cyclePulse1()'finished 
	 declare sub cyclePulse2() 'finished 
	 declare sub cycleTriangle() 'finished 
    declare sub cycleNoise() 'finished
    declare sub cycleDMC()      'not going to use for now
    declare sub updatesweepP1() ' finished
    declare sub updatesweepP2() ' finished
    declare sub clockquarter() ' finished
    declare sub clockhalf() ' finished
    declare function mix() as double ' finished
    declare sub handleframecounter() ' finished
    declare function getOutput() as outputs ptr  'any ptr' MAYBE
    declare function read_apu(addr1 as uint16_t) as uint8_t ' finished
    declare sub write_apu(addr1 as uint16_t,data1 as uint8_t) ' finished
    
'APU VARIABLES///////////////////
	
	_output(29781) as double
	_outputoffset as uint32_t
	
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
    p1Duty as double = 0
    p1DutyIndex as uint32_t = 0
    p1Output as double = 0
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
    p1SweepShift as int32_t = 0
    p1SweepTimer as int32_t = 0
    p1SweepTarget as int32_t = 0
    p1SweepMuting as boolean = true
    p1SweepReload as boolean = false
	
	'pulse 2
	 p2Timer as uint32_t = 0
    p2TimerValue as uint32_t = 0
    p2Duty as uint32_t = 0
    p2DutyIndex as uint32_t = 0
    p2Output as double = 0
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
    p2SweepShift as int32_t = 0
    p2SweepTimer as int32_t = 0
    p2SweepTarget as int32_t = 0
    p2SweepMuting as boolean = true
    p2SweepReload as boolean = false

	'triangle
	 triTimer as uint32_t = 0
    triTimerValue as uint32_t = 0
    triStepIndex as uint32_t = 0
    triOutput as double = 0
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
    noiseOutput as double = 0 
    noiseCounterHalt as boolean = false
    noiseCounter as uint32_t = 0
    noiseVolume as uint32_t = 0
    noiseConstantVolume as uint32_t = false
    noiseDecay as uint32_t = 0
    noiseEnvelopeCounter as uint32_t = 0
    noiseEnvelopeStart as boolean = false
    
	'dmc
	 dmcInterrupt as boolean = false
    dmcLoop as boolean = false
    dmcTimer as uint32_t = 0
    dmcTimerValue as uint32_t = 0
    dmcOutput as double = 0
    dmcSampleAddress as uint32_t = &Hc000
    dmcAddress as uint32_t = &Hc000
    dmcSample as double = 0
    dmcSampleLength as uint32_t = 0
    dmcSampleEmpty as boolean = true
    dmcBytesLeft as uint32_t = 0
    dmcShifter as uint32_t = 0
    dmcBitsLeft as uint32_t = 8
    dmcSilent as boolean = true

End Type





