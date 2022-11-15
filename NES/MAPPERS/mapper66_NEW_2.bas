#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"



  Type Mapper_66 extends Mapper
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper(hard as boolean)
  	
  		Declare function ppuPeak(adr As uint16_t) As uint8_t
  	
  	
  	  	  	  		Declare function _ppuRead(adr As uint16_t) As uint8_t
   	Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
  	  	
  	  	
  	 	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	
  	Declare function getchraddr1(addr1 as uint16_t) as uint32_t
  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t


  	Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  	chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
  	
  	private:
 	nCHRBankSelect as uint8_t = &H00 
	nPRGBankSelect as uint8_t = &H00 

  End Type

Constructor Mapper_66(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)


End Constructor
Destructor Mapper_66()


End Destructor
 function Mapper_66.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	 '= MIRROR.VERTICAL
 	
    if((header.mapper1 And &H01))  then
     ' // vertical
      return adr and &H7ff 
      else  
    '  // horizontal
      return (adr and &H3ff) or ((adr and &H800) shr 1) 
    end if
   End Function

function Mapper_66.getchraddr1(addr1 as uint16_t) as uint32_t
	
return addr1
	
	
End Function

function Mapper_66.getromaddr1(addr1 as uint16_t) as uint32_t
	if this.nprgbanks = 2 then
		return addr1 and &H7fff
		
	EndIf

	return addr1 and &H3fff
End Function

  Sub Mapper_66.resetmappper(hard as boolean)  
  	
  	'idk if has battery support
  	
  	
   this.nCHRBankSelect = 0 
	this.nPRGBankSelect = 0 
  End Sub
  
  
  
  function Mapper_66._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

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
          return this.ppuRam(adr and &H7ff)
        
        
         
          
   end if
   
   
   
   
  return 0
End function 


function Mapper_66._ppuRead(adr As uint16_t) As uint8_t
 if(adr < &H2000)  then
  
      if(this.nchrbanks = 0 )then
     return this.chrRam(this.nCHRBankSelect * &H2000 + adr)
      else 
      return vCHRMemory[this.nCHRBankSelect * &H2000 + adr]
      endif
 else  
    ' return this.ppuRam(adr and &H7ff) 
     ' return this.ppuRam(  (adr and &H3ff) or ((adr and &H800) shr 1) ) 
     
     return this.ppuRam(getmirroringadr(adr))
     
     
     
     
   EndIf
 
End Function
  
  
  
  
  
  
  Function Mapper_66.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
	 
		this.nCHRBankSelect = data1 and &H03 
		this.nPRGBankSelect = (data1 and &H30) shr 4 
	end if
	
	'// Mapper has handled write, but do not update ROMs
	return false 
End function 

Function Mapper_66.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
 
	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
	
		mapped_addr = this.nPRGBankSelect * &H8000 + (addr1 and &H7FFF)
		return true
	
	else
		return false
	end if
		
		
End function 
  
Function Mapper_66.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	if (addr1 < &H2000) then
	 
		mapped_addr = this.nCHRBankSelect * &H2000 + addr1
		return true 
	 
	else
		return false 
	End if

End function 

Function Mapper_66.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
return false
End Function

 function Mapper_66.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
			'					dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			'' 
			' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			''
			'close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_66.SetBattery()  as boolean'TVECTORUINT8_T
	'dim sram_save as integer = freefile
	'open "C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\kirbyadventure.batt" for binary as sram_save
	'		 
			'   this.vRAMStatic.clear						
  ''this.vRAMStatic.resize(32*1024)
  ''
			 
			 
			 
	'		 get # sram_save,,this.vRAMStatic[0],32*1024
	'		
	'		close sram_save
	'		
	return true 
	
 End Function

 
function Mapper_66.ppuPeak(adr As uint16_t) As uint8_t
	return this._ppuRead(adr)
End Function

