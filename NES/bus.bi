









type as Bus _Bus

#pragma once

#include "crt.bi"



'#include Once "nes\olc6502_NEW_2.bi"


#include once "nes/CRC32.bas"
'#include Once "NES\olc6502_new_2.bi" 
'#include Once "NES\olc6502.bi" ' UNCHECK WHEN FIXED
#include Once "NES\CPU.bi" 'NesJS port

#Include Once "NES\cartridge.bas"

#Include Once "NES\olc2c02.bas"


'#Include Once "olc2c02_portable.bas"


#Include Once "NES\PPU.bi" 'NesJS port

#include Once "NES\APU.bi" 'NesJS port
#include Once "NES\Audio.bas"






'if CPU_CHOOSE then
'
'else
'

'
'end if


type Bus
	
	dmatimer as uint32_t
	dmavalue as uint32_t
	dmaBase as uint32_t
	inDma as boolean
	
	declare sub old_funcs_tests()
	declare sub old_funcs()
	declare sub new_funcs()
	declare sub new_funcs2()
	
	cycles as uint32_t
	
	
	public:
	
	mapperirqwanted as bool = false
	frameirqwanted as bool = false
	
	
	declare sub NEW_NMI_AND_IRQ
	declare sub OLD_NMI_AND_IRQ
	
	declare constructor()
	
  'devices on bus
  
  	ram(&H800) As uint8_t
  	
  	'CHANGE WHEN NEEDED/////////////////////////////////
  	
  	
	cpu as cpu_type'olc6502
    
    'cpu as CPU
  '////////////////////////////////////////////////////
  
  
	apu as APU 'NesJS port
	'ppu as olc2c02
	ppu as ppu_type
	
	
	controller(2) As uint8_t
	'////////////
	
	'frameirqwanted As BOOLEAN =  FALSE

 'declare sub _clock ()
 declare function _clock() as bool
 
 
   declare sub getsamples(bytes_data() As double, count As uint16_t) 
	Declare Function _read(adr As uint16_t,rdonly as boolean= false) As uint8_t
	Declare Function _write(adr As uint16_t,value As uint8_t) As boolean
   declare Sub _reset(hard as boolean)

  
    controller_state(2) As uint8_t
	As uint32_t nSystemClockCounter = 0
	
	
	
	Declare sub _getpixels()  
	
	
	
	
dma_page As uint8_t
dma_addr As uint8_t
dma_data As uint8_t

dma_transfer As bool = FALSE
dma_dummy As bool = TRUE



	private:
	
'    controller_state(2) As uint8_t
'	As uint32_t nSystemClockCounter = 0
'	
'	
'dma_page As uint8_t
'dma_addr As uint8_t
'dma_data As uint8_t
'
'dma_transfer As bool = FALSE
'dma_dummy As bool = TRUE
'
'



public:
declare sub SetSampleFrequency( sample_rate as uint32_t)
dAudioSample as Double= 0.0


private:
dAudioTime as double = 0.0
dAudioGlobalTime as Double = 0.0
dAudioTimePerNESClock as Double = 0.0
dAudioTimePerSystemSample  as Double = 0.0

 	 
End Type




