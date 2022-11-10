#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"


dim shared Prgbanksptr as uint32_t ptr
dim shared mydata1 as  int16_t ptr

  Type Mapper_69 extends Mapper
  	
  	
   	declare function banked_read(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t) as uint32_t
 
  	  	declare sub banked_write(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t,data1 as uint8_t)

  	Declare Sub clock_irq()
  	
 ' 	declare sub commands()
  	declare sub commands(data1 as uint8_t)
  	declare sub setbanks()
  	
  	declare sub MapPrg(pagesize as long,slot as long,bank as long)
  	declare sub MapCHR(pagesize as long,slot as long,bank as long)
  	 	
  	prgmap(4) as uint32_t
  	chrmap(8) as uint32_t
  	
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper(hard as boolean)
  	
  	declare sub runcmd(param as uint8_t)
  	
  	  	  	  		Declare function _ppuRead(adr As uint16_t) As uint8_t
   	Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
  	  	
  	  	
  	 	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	
  	Declare function getchraddr1(addr1 as uint16_t) as uint32_t
  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t


  	Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean
  	
  	chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 'ppuram(&H1000) as uint8_t
  	ppuram(&H800) as uint8_t
  	
  	cmd as uint8_t
  	param1 as uint8_t
  	
  	const PRG_BANK_SIZE = 8192
  	const CHR_BANK_SIZE = 1024
  	
  	
  	RAM_ENABLED as boolean = false
  	ROM_BIT as boolean = false 
  	
  	
  	declare function irqstate() as bool
  	
  	  	   mirrormode as MIRROR= MIRROR.VERTICAL
  	   mirroring as uint8_t
  	   
  	   vRAMStatic As TVECTORUINT8_T
  	   
  	  	  	declare function _mirror() as MIRROR
  	'enum commands
  	'chr_bank
  	'prg_bank
  	'mirrormode
  	'
  	'
  	'
  	'
  	'
  	'End Enum
  	chr_banks(8) as uint32_t
  	prg_banks(4) as uint32_t
  	
  	dim srambank as uint32_t
  	
  	prgramSelected as boolean
  	prgramEnabled as boolean
  	
  	irq_pending as boolean
  	irq_enabled as boolean
  	irq_counter_enabled as boolean
  	
  	irq_counter as  int16_t
  	
  	  bIRQActive As bool = false 
  	
  	private:


  End Type
  
  Function Mapper_69.irqstate() as bool
	
	return this.bIRQActive
  End Function
    sub Mapper_69.banked_write(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t,data1 as uint8_t)
 ' 		dim eff_addr1 as uint32_t = (bank_size * bank_index) + (offset1 mod bank_size)
'	return eff_addr1 and this.prgand
  	
  	
    End Sub
  function Mapper_69.banked_read(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t) as uint32_t
	
	'dim eff_addr1 as uint32_t = (bank_size * bank_index) + (offset1 mod bank_size)
	'return eff_addr1 and this.prgand
	
End function

  
  sub Mapper_69.MapPrg(pagesize as long,slot as long,bank as long)
  	
  	
  	if bank < 0 then
  		dim numPages as uint8_t = (this.nPRGbanks * &H4000) \ (1024 * pagesize)
  		bank+=numPages
  		
  	EndIf
  	
  	for i as long = 0 to (pagesize \ 8) - 1
  	
  	dim curPage as uint8_t = ( (pageSize \ 8) * slot) + i
  	this.prgMap(curPage) = ( (pageSize * 1024 * bank) + (8192 * i) ) mod (this.nPRGbanks * &H4000)
  	next
  	
  End Sub
  
  
  
    sub Mapper_69.MapCHR(pagesize as long,slot as long,bank as long)
  	
  	
  	if bank < 0 then
  		dim numPages as uint8_t = (this.nCHRbanks * &H2000) \ (1024 * pagesize)
  		bank+=numPages
  		
  	EndIf
  	
  	for i as long = 0 to (pagesize \ 8) - 1
  	
  	dim curPage as uint8_t = ( (pageSize \ 8) * slot) + i
  	this.chrMap(curPage) = ( (pageSize * 1024 * bank) + (8192 * i) ) mod (this.nCHRbanks * &H2000)
  	next
  End Sub
  
  
  
  
  
  
  function  Mapper_69._mirror() as MIRROR
	Return this.mirrormode
  End Function
  

  Constructor Mapper_69(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)

'this.cmd = 0
'this.param1 = 0
'this.mirroring = 0
'
'erase this.chr_banks
'erase this.prg_banks
'
'SetBanks()
mydata1 = @this.irq_counter
 this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)

  
  prgbanksptr = @this.prg_banks(0)

End Constructor
Destructor Mapper_69()


End Destructor


sub Mapper_69.clock_irq()
	 
	if this.irq_counter_enabled then
		
		
		 this.irq_counter -= 1 
		      if this.irq_enabled and this.irq_counter  <= 0  then
		     
              bIRQActive = true 
               	this.irq_counter  = &HFFFF
		      end if
		  
	EndIf
	
	
	
End Sub

'
'sub Mapper_69.commands()
'	
'	if this.cmd >= &H0 and this.cmd <= &H07 then
'		
'   this.chr_banks(this.cmd) = param1
'   
'	elseif cmd = &H08 then
'		
'   this.prg_banks(0) = param1
'   
'	elseif this.cmd >= &H09 and this.cmd <= &H0B then
'		
'		dim slot as uint32_t = this.param1
'		this.prg_banks(slot) = this.param1 and &H3F
'		
'	elseif this.cmd = &H0C  then
'		
'   this.mirroring = param1 and &H03
'   
'	elseif this.cmd = &H0D  then
'		'TODO IRQ Control
'	elseif this.cmd = &H0E  then
'		'TODO IRQ Counter Low Byte 
'	elseif this.cmd = &H0F  then
'		'TODO IRQ Counter High Byte
'	EndIf 
'	
'	
'
'End Sub

sub Mapper_69.commands(data1 as uint8_t)
	
	if this.cmd >= &H0 and this.cmd <= &H07 then
		
   this.chr_banks(this.cmd) = data1 
 
	elseif this.cmd = &H08 then
		
	this.prgramEnabled = iif((data1 and &H10000000)<>0,true,false)
	this.prgramSelected = iif((data1 and &H01000000)<>0,true,false)
   this.prg_banks(0) =  data1 and &HB00111111
    
   
	elseif this.cmd >= &H09 and this.cmd <= &H0B then
		
	this.prg_banks(this.cmd - &H08) = (data1 and &HB00111111)
		
	elseif this.cmd = &H0C  then
		select case (data1 and &H00000011)
			case 0
				this.mirroring = 0
				this.mirroring = 0
				this.mirroring = 0
				
				this.mirroring = 0
			case 1
				this.mirroring = 1
				this.mirroring = 1
					this.mirroring = 1
				
				this.mirroring = 1
			case 2
				this.mirroring = 2
				this.mirroring = 2
				
					this.mirroring = 2
				
				
				this.mirroring = 2	
			case 3
				this.mirroring = 3
				this.mirroring = 3
					this.mirroring = 3
				this.mirroring = 3

		End Select
  
   
	elseif this.cmd = &H0D  then
		'TODO IRQ Control
	elseif this.cmd = &H0E  then
		'TODO IRQ Counter Low Byte 
	elseif this.cmd = &H0F  then
		'TODO IRQ Counter High Byte
	EndIf 
	
	

End Sub

sub Mapper_69.setbanks()
	
	
	this.MapPrg(8,0,this.prg_banks(1))
	 this.MapPrg(8,1,this.prg_banks(2))
	 this.MapPrg(8,2,this.prg_banks(3))
	 this.MapPrg(8,3,-1)
	'

		for i as integer = 0 to 8 -1
		
		this.MapCHR(1,i,this.chr_banks(i))
		
		
		Next
'TODO add mirror
 	    
End Sub



 function Mapper_69.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	
 	    select case this.mirroring 
      case 0 
       ' // vertical
        return adr and &H7ff 
      case 1 
   
   ' // horizontal
        return (adr and &H3ff) or ((adr and &H800) shr 1)
      case 2 
     
         ' // 1-screen A
        return adr and &H3ff 
     
      case 3 
        
          '  // 1-screen B
        return &H400 + (adr and &H3ff) 
 	    end select
 	    
   End Function

function Mapper_69.getchraddr1(addr1 as uint16_t) as uint32_t
	

	
End Function

function Mapper_69.getromaddr1(addr1 as uint16_t) as uint32_t

End Function

  Sub Mapper_69.resetmappper(hard as boolean)  
  	
'this.cmd = 0
'this.param1 = 0
'this.mirroring = 0
'
'erase this.chr_banks
'erase this.prg_banks

'SetBanks()


  End Sub
  
  
  
  function Mapper_69._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t
	 'if(adr < &H2000)  then
    '  if(this.nchrBanks = 0)  then
    '    this.chrRam(this.getChrAddr1(adr)) = value 
    '    return 0
    '    
    '   else  
    '   ' // not writable
    '    return 0
    '  end if
    '  else  
    ' this.ppuRam(this.getMirroringAdr(adr))   = value
    '  return this.ppuRam(this.getMirroringAdr(adr)) 
	 'end if
   '
   'if adr >= &H2000 and adr <= &H3FFF then
   '	
   '	
   '	
   '	
   '	
   '	
   'EndIf
   '
   '
   
   
  ' 
  ' 
  'if adr >= &H000 and adr <= &H03FF then	
  '	
  'elseif  adr >= &H0400 and adr <= &H07FF  then
  '	
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  '	
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  '	
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  'elseif  adr >= &H2000 and adr <= &H3FFF  then
  '	
  '	
  '	
  'EndIf
  ' 
  ' 
   
   
   
   
   
   
   
   
   
   
   
   if adr >= &H2000 and adr <= &H3FFF then
   		
	select case this.mirroring
		case 0
			ppuram(getmirroringadr(adr)) = value  
		case 1
		   ppuram(getmirroringadr(adr))  = value 
		case 2
			ppuram(getmirroringadr(adr))  = value 
		case 3
		   ppuram(getmirroringadr(adr))  = value 
		
	End Select
	
EndIf

   
   
   
   
 'return 0
End function 


function Mapper_69._ppuRead(adr As uint16_t) As uint8_t
'return 0
  'if(adr < &H2000)  then
  ' 
  '    if(this.nchrbanks = 0 )then
  '   return this.chrRam(this.getChrAddr1(adr))
  '    else 
  '    return vCHRMemory[this.getChrAddr1(adr)]
  '    end if
  '          
  '          'return this.ppuRam(this.getmirroringadr(adr)) 
  ' End if

  
if adr >= &H0000 and adr <= &H03FF then
	

dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(0)) + ((adr -  &H0000) and &H03FF)
	return vCHRMemory[eff_addr1 mod vCHRMemory.size()]'and this.'rg'nd

elseif adr >= &H0400 and adr <= &H07FF  then
		
	dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(1)) + ((adr -  &H0400) and &H03FF)
	return  vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rgand
elseif adr >= &H0800 and adr <= &H0BFF then	
	
		dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(2)) + ((adr -  &H0800) and &H03FF)
	return vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rgand
elseif adr >= &H0C00 and adr <= &H0FFF  then
	
			dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(3)) + ((adr -  &H0C00) and &H03FF)
	return vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rgand
elseif adr >= &H1000 and adr <= &H13FF then 	
		
			dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(4)) + ((adr -  &H1000) and &H03FF)
	return  vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rgand
elseif adr >= &H1400 and adr <= &H17FF  then
	
			dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(5)) + ((adr -  &H1400) and &H03FF)
	return vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rgand
elseif adr >= &H1800 and adr <= &H1BFF then
	
			dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(6)) + ((adr -  &H1800) and &H03FF)
	return  vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rg'nd
	
	
elseif adr >= &H1C00 and adr <= &H1FFF then
	
			dim eff_addr1 as uint32_t = (&H0400 * this.chr_banks(7)) + ((adr -  &H1C00) and &H03FF)
	return  vCHRMemory[eff_addr1 and vCHRMemory.size()-1]'and this.'rg'nd
	
	

	
elseif adr >= &H2000 and adr <= &H3FFF then	

	select case this.mirroring
		case 0
			return ppuram(getmirroringadr(adr))  
		case 1
		   return ppuram(getmirroringadr(adr)) 
		case 2
			
			return ppuram(getmirroringadr(adr)) 
		case 3
		   return ppuram(getmirroringadr(adr))  
		
	End select
	
EndIf

  
  
  
  
End Function
  
  
sub Mapper_69.runcmd(param as uint8_t)
  
 
End sub
  
  
  
Function Mapper_69.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean

 

'if addr1 < &H8000 and this.RAM_ENABLED then
'	
'	'this.prgram(addr1 - &H6000) = data1
'	
'elseif addr1 >= &H8000 and  addr1 <= &H9FFF then
'	
'	this.cmd = data1 and &H0F
'		
'elseif addr1 >= &HA000 and  addr1 <= &HBFFF then	
'		
'	this.param1 = data1 
'   commands()	
'	
'EndIf
'
'
'setbanks()
'
'
'mapped_addr = addr1
'
'
'return true

	


if addr1 >= &H6000 and addr1 <= &H7FFF then

 'if this.prgramSelected and this.prgramEnabled
 'this.vRAMStatic[addr1 and &H1FFF] = data1  
 'mapped_addr = &HFFFFFFFF 
 'return true
 'end if
'beep
return false

elseif addr1 >= &H8000 and addr1 <= &H9FFF then
	'mydata1 = data1
	this.cmd = data1 and &HF'&HB00001111
	return false
elseif addr1 >= &HA000 and addr1 <= &HBFFF then
	
	'this.commands(data1)
	
	
	select case this.cmd 
      	
      	case &H00 to &H07
      		this.chr_banks(this.cmd) = data1
      		
      		
      	
      	case &H08
      	
      	'beep
      	this.prg_banks(0) = data1 and &HB00111111
      	srambank = srambank
      	
      	
      	case &H09 to &HB
      	
      	
      	this.prgramEnabled = iif((data1 and &HB10000000) <> 0,true,false)
      	this.prgramSelected = iif((data1 and &HB01000000) <> 0,true,false)
      	this.prg_banks(this.cmd - 8) = (data1 and &H00111111)  
      	
      	
      	case &H0C
      	
      	
      	   select case data1 and &H00000011
      	   	
      	   	case 0
      	   		this.mirrormode = MIRROR.VERTICAL
      	   	case 1
      	   		this.mirrormode = MIRROR.HORIZONTAL
      	   	case 2
      	   		this.mirrormode = MIRROR.ONESCREEN_LO
      	   	case 3
      	         this.mirrormode = MIRROR.ONESCREEN_HI
      	   End Select
      	
      	case &H0D
      		this.bIRQActive = false
      		this.irq_enabled = iif(data1 and &H01,true,false)'iif((data1 and &B00000001) <> 0,true,false)
      		this.irq_counter_enabled = iif(data1 and &H80,true,false)'iif((data1 and &B10000000) <> 0,true,false)
      		
      	case &H0E
      		this.irq_counter =  (this.irq_counter and  &HFF00) + cast(uint16_t,data1)
      		
      		
      	case &H0F
      	this.irq_counter =  (this.irq_counter and  &H00FF) + cast(uint16_t,data1)shl 8
      	
      	
	End Select
	
	
	
	
	return false
elseif addr1 >= &HC000 and addr1 <= &HDFFF then
	return false
elseif addr1 >= &HE000 and addr1 <= &HFFFF then
	return false
end if











End function 

Function Mapper_69.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
   


'if addr1 >= &H6000 and addr1 <= &H8000 then
'	if (this.RAM_ENABLED) then
'		'dim offsetaddress as uint16_t = addr1 - &H8000
'		'data1 = this.prgram(offsetaddress)
'	else
'		dim bankaddr as uint8_t = this.prg_banks(0) and &H3F
'		 dim addr2 as uint32_t = ( 8*1024  * bankaddr) mod (this.nPRGbanks * &H4000)
'		 mapped_addr = addr2
'		' 
'		 'data1 = vPRGmemory[mapped_addr]
'		return true
'EndIf
'
'elseif addr1 >=&H8000 and addr1 <= &HFFFF then
'	
'	dim offsetaddress as uint16_t = (addr1 - &H8000)
'	dim prgaddr as uint32_t = this.prgmap(offsetaddress \ &H2000) + (offsetaddress mod &H2000)
'	mapped_addr = prgaddr
'	return true
'EndIf


if addr1 >= &H6000 and addr1 <= &H7FFF then
	
	 if this.prgramSelected then 
	
	 	if this.prgramEnabled then 
			 'data1 = this.vRAMStatic[addr1 and &H1FFF]
          'mapped_addr = &HFFFFFFFF 
         'return true
		'		return true
		 else
			
		'	return false
	'		
	 	EndIf
		
	
	 else
	 	 
	dim eff_addr1 as uint32_t = (&H2000 * this.prg_banks(0)) + ((addr1 -  &H6000) and &H1FFF)
	mapped_addr = eff_addr1  and vPRGMemory.size()-1'this.prgand
	return true
EndIf
	
'return false


elseif addr1 >= &H8000 and addr1 <= &H9FFF then
dim eff_addr1 as uint32_t = (&H2000 * this.prg_banks(1)) + ((addr1 -  &H8000) and &H1FFF)
	mapped_addr = eff_addr1 and vPRGMemory.size()-1'and this.prgand
	return true
		'return eff_addr1 and this.prgand
elseif addr1 >= &HA000 and addr1 <= &HBFFF then
 
dim eff_addr1 as uint32_t = (&H2000 * this.prg_banks(2)) + ((addr1 -  &HA000) and &H1FFF)
	mapped_addr = eff_addr1 and vPRGMemory.size()-1'and this.prgand
	return true
elseif addr1 >= &HC000 and addr1 <= &HDFFF then
dim eff_addr1 as uint32_t = (&H2000 * this.prg_banks(3)) + ((addr1 -  &HC000) and &H1FFF)
	mapped_addr = eff_addr1 and vPRGMemory.size()-1'and this.prgand
	return true
elseif addr1 >= &HE000 and addr1 <= &HFFFF then

	dim eff_addr1 as uint32_t = (&H2000 * &HFF) + ((addr1 -  &HE000) and &H1FFF)
	mapped_addr = eff_addr1  and vPRGMemory.size()-1'this.prgand
	return true
end if



		
End function 
  
  'sub Mapper_69.banked_write(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t)
  '	
  '	
  '	
  'End Sub
  
'function Mapper_69.banked_read(bank_size as uint32_t,bank_index as uint32_t,offset1 as uint32_t) as uint32_t
'	
'	'dim eff_addr1 as uint32_t = (bank_size * bank_index) + (offset1 mod bank_size)
'	'return eff_addr1 and this.prgand
'	
'End function

  
Function Mapper_69.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 


End function 

Function Mapper_69.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
return false
End Function

 function Mapper_69.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T

		
	return true
 End Function
 
 function Mapper_69.SetBattery()  as boolean'TVECTORUINT8_T

	return true 
	
 End Function


  
  
  
  