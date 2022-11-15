#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"



  Type Mapper_03 extends Mapper
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper(hard as boolean)
  	
  	  	Declare function _ppuRead(adr As uint16_t) As uint8_t
   Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
   
   Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
chrram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
 
 Declare function getchraddr1(addr1 as uint16_t) as uint32_t
Declare function getromaddr1(addr1 as uint16_t) as uint32_t
    	
  	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	  	
    	chrbank  as uint32_t
  	  		Declare function ppuPeak(adr As uint16_t) As uint8_t
  	
  	
  	private:
  nCHRBankSelect as 	uint8_t = &H00

  End Type

  function Mapper_03.getromaddr1(addr1 as uint16_t) as uint32_t
  	if this.nPRGBanks = 2 then
		return addr1 and &H7fff
		
	EndIf

	return addr1 and &H3fff
  	
  	
  End Function
  
  
   function Mapper_03.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	 '= MIRROR.VERTICAL
 	
    if((header.mapper1 And &H01))  then
     ' // vertical
      return adr and &H7ff 
      else  
    '  // horizontal
      return (adr and &H3ff) or ((adr and &H800) shr 1) 
    end if
   End Function

  
 function Mapper_03.getchraddr1(addr1 as uint16_t) as uint32_t
	
  dim final as uint32_t = this.chrBank * &H2000 + (addr1 and &H1fff) 
    return final and this.chrAnd
	
 End Function 
  
  
  
  
  
  
function Mapper_03._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

	 if(adr < &H2000)  then
      if(this.nchrBanks = 0)  then
        this.chrRam(this.getChrAddr1(adr)) = value 
        return 0
       else  
       '// not writable
        return 0
      end if
      else  
 
       
        
         this.ppuRam(getmirroringadr(adr)) = value
          return this.ppuRam(getmirroringadr(adr))
        
        
         
          
   end if
  
End function 


function Mapper_03._ppuRead(adr As uint16_t) As uint8_t
 if(adr < &H2000)  then
  
      if(this.nchrbanks = 0 )then
     return this.chrRam(this.getChrAddr1(adr))
      else 
      return vCHRMemory[this.getChrAddr1(adr)]
      endif
 else  

     
     return this.ppuRam(getmirroringadr(adr))
     
     
     
     
   EndIf
 
End Function







Constructor Mapper_03(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)


End Constructor
Destructor Mapper_03()


End Destructor


  Sub Mapper_03.resetmappper(hard as boolean)   

  End Sub
  
  
  
  
  
  Function Mapper_03.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
 
	''
    if(addr1 < &H8000) then
      return  false' // no mapper registers or prg ram
    end if
    this.chrBank = data1 
   '
   '	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
	' 
	'	this.nCHRBankSelect = data1 and &H03 
	'	mapped_addr = addr1		
	'end if

	'// Mapper has handled write, but do not update ROMs
	'return false;s handled write, but do not update ROMs
	return false

   
   
End function 

Function Mapper_03.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
      if(addr1 < &H8000) then
      return  false ' // no mapper registers or prg ram
      end if
		mapped_addr = this.getRomAddr1(addr1)
		return  true 
		
	'if (addr1 >= &H8000 and addr1 <= &HFFFF) then
  
	'	if (this.nPRGBanks  = 1) then'// 16K ROM 
	'		mapped_addr = addr1 and &H3FFF
	'	end if 
	'	if (this.nPRGBanks =  2) then '// 32K ROM
	'		mapped_addr = addr1 and &H7FFF 
	'		
	'	end if
	'	return true 
	' 
	'else
	'	return false 
	'end if
		
End function 
'  '
  
  
  
  
  
Function Mapper_03.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 


End function 

Function Mapper_03.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 

End Function

 function Mapper_03.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
					
	'return true 
 End Function
 
 function Mapper_03.SetBattery()  as boolean'TVECTORUINT8_T
		
	'return true 
	
 End Function

Function Mapper_03.ppuPeak(adr As uint16_t) As uint8_t
	return this._ppuRead(adr)
End Function
