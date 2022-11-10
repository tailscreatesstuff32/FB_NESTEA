#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"
#include once "file.bi"
dim shared lastread1 as uint32_t

MVectorTemplate(uint8_t)
  Type Mapper_04 extends Mapper
    	  	Public:
   Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
   declare Destructor()
  	
  	Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T


	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	
  
  	
  	  	  	  		Declare function _ppuRead(adr As uint16_t) As uint8_t
   	Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
  	  	
  	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	'Declare function getchradr(adr as uint16_t) as uint32_t
  	
  	
  	
  	
   	declare sub get_scanline() 
  	


Declare function getchraddr1(addr1 as uint16_t) as uint32_t


'Declare function getchradr(adr as uint16_t) as uint32_t

'Declare function getmirroringadr(addr1 as uint16_t) as uint32_t



  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t
  	
  			
  	Declare Sub resetmappper(hard as boolean)
  	  	declare function _mirror() as MIRROR
  	  	
bankregs(8) as uint8_t
 	
prgmode as uint8_t = 1
chrmode as uint8_t = 1
regselect as uint8_t
'reloadirq as boolean
'irqlatch as uint16_t
'irqcounter as uint16_t
'irqEnabled as boolean
'lastRead as uint16_t



 vRAMStatic As TVECTORUINT8_T
 
 chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
  
  	   mirrormode as MIRROR= MIRROR.HORIZONTAL
  	   mirroring as uint8_t
  	   	declare function irqstate() as bool
  	declare sub irqclear()
  	      
    bIRQActive As bool = false 
    'bIRQEnable As bool =  false 
    'bIRQUpdate As bool =  false 
  	 '  
	 'nIRQCounter As uint16_t 
	
	 'nIRQReload  As uint16_t
  	 ' irqcounter as uint32_t
  	 
  	  ' 	 bIRQActive As bool = false 
  	
  	  	reloadirq as boolean
irqlatch as uint16_t
irqcounter as uint16_t
irqEnabled as boolean
lastRead as uint16_t
  	 
  	 
  End Type

 function Mapper_04.getmirroringadr(adr  as uint16_t) as uint32_t
    if(this.mirroring = 0)  then
     ' // vertical
   
        return adr and &H7ff 
      else  
    '  // horizontal
   
      
        return (adr and &H3ff) or ((adr and &H800) shr 1)
    end if
    
 End Function




function Mapper_04._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t
	 if(adr < &H2000)  then
      if(this.nchrBanks = 0)  then
        this.chrRam(this.getChrAddr1(adr)) = value 
        return 0
        
       else  
       ' // not writable
        return 0
      end if
      else  
     this.ppuRam(this.getMirroringAdr(adr))   = value
      return this.ppuRam(this.getMirroringAdr(adr)) 
   end if
  
End function 


function Mapper_04._ppuRead(adr As uint16_t) As uint8_t

 if(adr < &H2000)  then
      '// A12 should be ignored for 8 cycles after going high
      '// see https://forums.nesdev.com/viewtopic.php?f=3&t=17290#p217456
      '// ignore for nametables, for now
      
     	if ((lastread1 and &H1000) = 0 and (adr  and &H1000) > 0) then
	 this.get_scanline()
		'this.clockIrq()
     	EndIf
     	
     	
     	
    ' 	(16 + &H4000 * this.nPRGBanks)
   	lastread1 = adr
   '  	131088 + 
      if(this.nchrbanks = 0 )then
     return this.chrRam(this.getChrAddr1(adr))
      else 
      return vCHRMemory[this.getChrAddr1(adr)]
      end if
      else  
      return this.ppuRam(this.getmirroringadr(adr)) 
   EndIf
End Function




Constructor Mapper_04(prgbanks As uint8_t,chrbanks As uint8_t) 
							base(prgbanks ,chrbanks)
							
							
this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)
  
 'this.GetBattery()
  this.SetBattery()	
	
	


End Constructor

function Mapper_04.getromaddr1(addr1 as uint16_t) as uint32_t
	
	
	 dim final as uint32_t
    if(this.prgMode = 1)  then
     if(addr1 < &Ha000)  then
        final = ((this.nPRGBanks * 2) - 2) * &H2000 + (addr1 and &H1fff) 
       elseif(addr1 < &Hc000) then  
        final = this.bankRegs(7) * &H2000 + (addr1 and &H1fff) 
       elseif(addr1 < &He000) then
        final = this.bankRegs(6) * &H2000 + (addr1 and &H1fff) 
     else  
       final = ((this.nPRGBanks * 2) - 1) * &H2000 + (addr1 and &H1fff) 
      end if
    else 
      if(addr1 < &Ha000)  then
        final = this.bankRegs(6) * &H2000 + (addr1 and &H1fff) 
        elseif(addr1 < &Hc000) then
        final = this.bankRegs(7) * &H2000 + (addr1 and &H1fff) 
        elseif(addr1 < &He000) then 
        final = ((this.nPRGBanks * 2) - 2) * &H2000 + (addr1 and &H1fff) 
      else 
        final = ((this.nPRGBanks * 2) - 1) * &H2000 + (addr1 and &H1fff) 
      end if
    end if
    return final and this.prgAnd 
	
	
	
End Function

function Mapper_04.getchraddr1(addr1 as uint16_t) as uint32_t
	
	
	 dim final as uint32_t
    if(this.chrMode = 1)  then
     addr1 xor=  &H1000
    end if
    
      if(addr1 < &H800)  then
      final = (this.bankRegs(0) shr 1) * &H800 + (addr1 and &H7ff) 
      elseif(addr1 < &H1000)  then
      final = (this.bankRegs(1) shr 1) * &H800 + (addr1 and &H7ff) 
      elseif(addr1 < &H1400)  then 
      final = this.bankRegs(2) * &H400 + (addr1 and &H3ff) 
      elseif(addr1 < &H1800)  then
      final = this.bankRegs(3) * &H400 + (addr1 and &H3ff) 
      elseif(addr1 < &H1c00) then
      final = this.bankRegs(4) * &H400 + (addr1 and &H3ff) 
     else  
      final = this.bankRegs(5) * &H400 + (addr1 and &H3ff) 
      end if
 
   
    return final and this.chrAnd 
	
	
	
End Function

 function Mapper_04.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
								dim sram_save as integer = FreeFile
			'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" &
			'chdir("C:\NESTEA_BATTSAVES")
 'romname = mid(romname,19,5)
 
	open "C:\NESTEA_BATTSAVES\"  & romname & ".batt" for binary as sram_save
			' 
			 put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			'
			'beep
			close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_04.SetBattery()  as boolean'TVECTORUINT8_T
	dim sram_save as integer = freefile
	
	'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" & 
	 'chdir("C:\NESTEA_BATTSAVES")
  ' romname = mid(romname,19,5)
 'cls
 'print romname
 'sleep
	if fileexists("C:\NESTEA_BATTSAVES\" & romname & ".batt"  ) then
	open "C:\NESTEA_BATTSAVES\"  & romname & ".batt" for binary as sram_save
			 
			'   this.vRAMStatic.clear						
  'this.vRAMStatic.resize(32*1024)
   ' beep
			 		 
			 get # sram_save,,this.vRAMStatic[0],32*1024
			
			close sram_save
			return true 
	else		
		
 
			return false
	end if

	
	
 End Function

function  Mapper_04._mirror() as MIRROR
	Return this.mirrormode
End Function







Destructor Mapper_04()

 
 

 ' this.vRAMStatic.clear						
 ' this.vRAMStatic.resize(32*1024)
  
End Destructor



Function Mapper_04.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean
	
	
	    if(addr1 < &H6000) then
      return false '// not readable
    end if 
    if(addr1 < &H8000) then
     ' return this.prgRam[adr & 0x1fff]
     
    data1 = this.vRAMStatic[addr1 and &H1FFF]
     	mapped_addr = &HFFFFFFFF 
     	 
     return true
    end if
    mapped_addr  = this.getRomAddr1(addr1)
	return true
	
	
	
	
End Function 

  Function Mapper_04.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
  	
  	if(addr1 < &H6000)  then
      return false' // no mapper registers
  	end if
    if(addr1 < &H8000)  then
     ' this.prgRam[adr & 0x1fff] = value;
      this.vRAMStatic[addr1 and &H1FFF] = data1 
     
     	mapped_addr = &HFFFFFFFF 
      return true
    end if
    select case (addr1 and &H6001) 
      case &H0000 
        this.regSelect = data1 and &H7 
        this.prgMode = (data1 and &H40) shr 6 
        this.chrMode = (data1 and &H80) shr 7 
          return false
      
    	case &H0001 
        this.bankRegs(this.regSelect) = data1
            return false
      
    	case &H2000
    		
    		this.mirroring = data1 and &H01
      '  this.mirrormode = data1 and &H1
      if (data1 and &H01) Then
				this.mirrormode = HORIZONTAL
			else
				this.mirrormode = VERTICAL
			End If 
           return false
     
    	case &H2001 
       ' // ram protection not implemented
       
    
    	case &H4000 
       ' this.irqLatch = data1 
      ' this.nIRQReload = data1
      
      
      this.irqLatch =   data1
      
            return false
       
    	case &H4001 
    	'	this.nIRQCounter = &H0000 
    	this.reloadIrq = true
    	
    	
       ' this.reloadIrq = true 
         'this.nIRQReload = data1
          return false
    	case &H6000 
    		this.irqEnabled = false
    		
    	'		this.bIRQEnable = false 
			this.bIRQActive = false
			 
     '  this.irqEnabled = false 
       ' this.nes.mapperIrqWanted = false 
            return false
      
      case &H6001  
      '  this.irqEnabled = true 
      '  this.bIRQEnable = true
      
      this.irqEnabled = true
            return false
      
    end select
  	
  	

  	
  	
  End Function
sub Mapper_04.irqclear()
	this.bIRQActive = FALSE
	
End sub

sub Mapper_04.get_scanline()
	'if (this.nIRQCounter =  0  ) Then
   '	this.nIRQCounter = this.nIRQReload 
   '	'this.reloadIrq = false
	'else
	'	this.nIRQCounter-=1
	'	
	''	this.nIRQCounter And= &Hff
	'End If
	'if (this.nIRQCounter =  0 and this.bIRQEnable) Then
	' 
	'	this.bIRQActive = true
	'End If
	'
	'
	if this.irqcounter = 0 or this.reloadirq then
		
		this.irqCounter = this.irqLatch
		this.reloadIrq = false
		
	else
		this.irqCounter-=1
		this.irqCounter and= &Hff
		
	EndIf
	
	 if(this.irqCounter = 0 and this.irqEnabled)  then
    'this.nes.mapperIrqWanted = true 
    this.bIRQActive = true
	 end if
	
	
 	  '  if this.irqCounter = 0 or this.reloadIrq then
     ' this.irqCounter = this.irqLatch 
     ' this.reloadIrq = false 
     'else 
     ' this.irqCounter- 1
     ' this.irqCounter and &Hf;  	    end if   
     '  if(this.irqCounter =  0 &and this.irqEnabled) { then     this.bIRQActive = true  end if
	
End sub

Function Mapper_04.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
	   
if addr1 < &H2000 then
'	
'	
'	
	if (this.lastread and &H1000) = 0 and (addr1 and &H1000) > 0 then
		
	this.get_scanline()
'	'	'this.clockIrq()
'
'	'																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																		EndIf
'	'
'	'
'	'
	EndIf
 this.lastread = addr1
'	
'	
'	
	if this.nchrbanks = 0 then
		mapped_addr = this.getChrAddr1(addr1)
		return true
'		
'		
	else
		mapped_addr = this.getChrAddr1(addr1)
		return true
'		
EndIf
'	
else
'	
	return false
EndIf
 
End Function

Function Mapper_04.irqstate() as bool
	
	return this.bIRQActive
End Function

Function Mapper_04.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
if addr1 < &H2000 then
	if this.nchrbanks = 0 then
		mapped_addr = this.getChrAddr1(addr1)
		return true
		
		
	else
		
		return false
		
	EndIf
	
else
	
	return false
EndIf
 return false

End Function







  Sub Mapper_04.resetmappper(hard as boolean)   
   if bat_saves = false then
   '	if this.vRAMStatic.size() <> 0 then
   	
   	this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
   	
   	''
    EndIf
   	
   	'erase SRAM
   '	
   'Erase pPRGBank
   'Erase pCHRBank


  End Sub
  
 