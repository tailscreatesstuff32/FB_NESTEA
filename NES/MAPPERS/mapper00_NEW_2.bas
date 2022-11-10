#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"
#include once "file.bi"


MVectorTemplate(uint8_t)
  Type Mapper_00 extends Mapper
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
  
  	 '  mirrormode as MIRROR= MIRROR.HORIZONTAL
  	  ' mirroring as uint8_t

  End Type

 
 function Mapper_00.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	 '= MIRROR.VERTICAL
 	
    if((header.mapper1 And &H01))  then
     ' // vertical
      return adr and &H7ff 
      else  
    '  // horizontal
      return (adr and &H3ff) or ((adr and &H800) shr 1) 
    end if
   End Function




function Mapper_00._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

	 if(adr < &H2000)  then
      if(this.nchrBanks = 0)  then
        this.chrRam(this.getChrAddr1(adr)) = value 
        return 0
       else  
       '// not writable
        return 0
      end if
      else  
    ' this.ppuRam( (adr and &H3ff) or ((adr and &H800) shr 1) )   = value
     ' return this.ppuRam((adr and &H3ff) or ((adr and &H800) shr 1) )
     
     ' this.ppuRam(adr and &H7ff) = value
     
     
     
     
     
     '  this.ppuRam(adr and &H7ff) = value
       '  return this.ppuRam(adr and &H7ff)
       
        
         this.ppuRam(getmirroringadr(adr)) = value
          return this.ppuRam(getmirroringadr(adr))
        
        
         
          
   end if
  
End function 


function Mapper_00._ppuRead(adr As uint16_t) As uint8_t
 if(adr < &H2000)  then
  
      if(this.nchrbanks = 0 )then
     return this.chrRam(this.getChrAddr1(adr))
      else 
      return vCHRMemory[this.getChrAddr1(adr)]
      endif
 else  
    ' return this.ppuRam(adr and &H7ff) 
     ' return this.ppuRam(  (adr and &H3ff) or ((adr and &H800) shr 1) ) 
     
     return this.ppuRam(getmirroringadr(adr))
     
     
     
     
   EndIf
 
End Function




Constructor Mapper_00(prgbanks As uint8_t,chrbanks As uint8_t) 
							base(prgbanks ,chrbanks)
							
							
this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)
  
  this.SetBattery()	
	
	


End Constructor

function Mapper_00.getromaddr1(addr1 as uint16_t) as uint32_t
	if this.nprgbanks = 2 then
		return addr1 and &H7fff
		
	EndIf

	return addr1 and &H3fff
End Function

function Mapper_00.getchraddr1(addr1 as uint16_t) as uint32_t
	
return addr1
	
	
End Function

 function Mapper_00.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
	'							dim sram_save as integer = FreeFile
	'		
	'		open "kirbyadventure.batt" for binary as sram_save
	'		' 
	'		 put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
	'		'
	'		close sram_save
	'	
	'return true '@this.vRAMStatic
 End Function
 
 function Mapper_00.SetBattery()  as boolean'TVECTORUINT8_T
	''dim sram_save as integer = freefile
	''open "C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\kirbyadventure.batt" for binary as sram_save
			 
			'   this.vRAMStatic.clear						
  ''this.vRAMStatic.resize(32*1024)
  ''
			 
			 
			 
	'		 get # sram_save,,this.vRAMStatic[0],32*1024
	'		
	'		close sram_save
	'		
	'return true 
	
 End Function

function  Mapper_00._mirror() as MIRROR
	Return this.mirrormode
End Function







Destructor Mapper_00()
  this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)

End Destructor



Function Mapper_00.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean
	
	  if(addr1 < &H6000)  then
      return false ' // not readable
	  end if
    if(addr1 < &H8000)  then 
     'return this.prgRam[adr & 0x1fff] 
     return true
    end if
    mapped_addr = this.getRomAddr1(addr1) 
 return true
	
	
	
End Function 

  Function Mapper_00.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
  	    if(addr1 < &H6000 or addr1 >= &H8000)  then
      return false '// no mapper registers
    end if
    this.prgRam(addr1 and &H1fff) = data1

  	
  End Function



Function Mapper_00.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	  
		mapped_addr = addr1 
		return true 
	End If

	return false

End function 

Function Mapper_00.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	 
	
	 If (this.nCHRBanks = 0) Then
	 	mapped_addr = addr1 
		return TRUE 
	 EndIf
	
		
		
		
	End If

	return FALSE

End Function

  Sub Mapper_00.resetmappper(hard as boolean)   
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
  
 