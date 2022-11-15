#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"






Type Mapper_34 extends Mapper
  	
  	const PRG_BANK_SIZE as uint32_t = 32768
  	const CHR_BANK_SIZE as uint32_t = 4096
  	
  	
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  		Declare function ppuPeak(adr As uint16_t) As uint8_t
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper(hard as boolean)
  	
  	  	declare function _mirror() as MIRROR
  	  	
  	  	  	  		Declare function _ppuRead(adr As uint16_t) As uint8_t
   	Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
  	  	
  	  	
  	 	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	
  	Declare function getchraddr1(addr1 as uint16_t) as uint32_t
  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t


  	Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  '	chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
  	
  	
  	
  	
  	prgbank as uint32_t
  	chr_bank0 as uint8_t
  	chr_bank1 as uint8_t
  	
  	'enum boards
  	'BxROM
  	'NINA001
  	'End Enum
  	'
  	'board as boards
 
End Type

 function Mapper_34.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	 '= MIRROR.VERTICAL
 	
    if((header.mapper1 And &H01))  then
     ' // vertical
      return adr and &H7ff 
      else  
    '  // horizontal
      return (adr and &H3ff) or ((adr and &H800) shr 1) 
    end if
   End Function


'function Mapper_34._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t
'
'	 if(adr < &H2000)  then
'      if(this.nchrBanks = 0)  then
'        this.chrRam(this.getChrAddr1(adr)) = value 
'        return 0
'       else  
'       '// not writable
'        return 0
'      end if
'      else  
'    ' this.ppuRam( (adr and &H3ff) or ((adr and &H800) shr 1) )   = value
'     ' return this.ppuRam((adr and &H3ff) or ((adr and &H800) shr 1) )
'     
'     ' this.ppuRam(adr and &H7ff) = value
'     
'     
'     
'     
'     
'     '  this.ppuRam(adr and &H7ff) = value
'       '  return this.ppuRam(adr and &H7ff)
'       
'        
'         this.ppuRam(getmirroringadr(adr)) = value
'          return this.ppuRam(adr and &H7ff)
'        
'        
'         
'          
'   end if
'  
'End function 


'function Mapper_34._ppuRead(adr As uint16_t) As uint8_t
' if(adr < &H2000)  then
'  
'      if(this.nchrbanks = 0 )then
'     return this.chrRam(this.getChrAddr1(adr))
'      else 
'      return vCHRMemory[this.getChrAddr1(adr)]
'      endif
' else  
'    ' return this.ppuRam(adr and &H7ff) 
'     ' return this.ppuRam(  (adr and &H3ff) or ((adr and &H800) shr 1) ) 
'     
'     return this.ppuRam(getmirroringadr(adr))
'     
'     
'     
'     
'   EndIf
' 
'End Function




Constructor Mapper_34(prgbanks As uint8_t,chrbanks As uint8_t) 
							base(prgbanks ,chrbanks)
							
							
this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)
  
  this.SetBattery()	
	
	
	this.board  = iif(vCHRMemory.size() <= 8192,this.boards.BxROM ,this.boards.NINA001)
	
	'if this.board =  this.boards.BxROM then
		
	'else
		this.mirrormode = HORIZONTAL 
		
		
	'EndIf


End Constructor
function Mapper_34._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

return false
  
End function 


function Mapper_34._ppuRead(adr As uint16_t) As uint8_t
 
  if(adr < &H2000)  then
     
     
    if adr < &H1000 then 
    dim _base as uint32_t = (this.chr_bank0) * CHR_BANK_SIZE 
    return vCHRMemory[ _base or  adr ]
    end if
    
     

    dim _base as uint32_t = (this.chr_bank1) * CHR_BANK_SIZE 
    return vCHRMemory[ _base or (adr and &H0FFF) ]
   
   EndIf
 
End Function

function Mapper_34.getromaddr1(addr1 as uint16_t) as uint32_t

End Function

function Mapper_34.getchraddr1(addr1 as uint16_t) as uint32_t
	
return addr1
	
	
End Function

 function Mapper_34.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
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
 
 function Mapper_34.SetBattery()  as boolean'TVECTORUINT8_T
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

function  Mapper_34._mirror() as MIRROR
	Return this.mirrormode
End Function







Destructor Mapper_34()
  this.vRAMStatic.clear						
  this.vRAMStatic.resize(32*1024)

End Destructor


	'if addr1 < &H1000 then
	'		dim base as uint32_t =  this.chr_bank0 * this.CHR_BANK_SIZE
	'	dim index as uint32_t = base or (addr1 and &H7FFF)
	'	 
	'	mapped_addr = base or  addr1 
	'	
	'	
	'	
	'EndIf
	'
	'	if addr1 < &H2000 then
	'	
	'	
	'	
	'	
	'	EndIf

Function Mapper_34.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean
	
	'if this.board = this.boards.NINA001 then
		
		
	'if addr1 >= &H6000 and  addr1 < &H8000 then
	'	  data1 = this.vRAMStatic[addr1 and &H1FFF]
   '  	mapped_addr = &HFFFFFFFF 
   '  	 
   '  return true
	'	
	'	
	'EndIf
	'
	'	dim _base as uint32_t =  this.prgbank * this.PRG_BANK_SIZE
	'	'dim index as uint32_t = base or (addr1 and &H7FFF)
	'	 
	'	mapped_addr = _base or (addr1 and &H7FFF) 
   '   return false
   '   
	'else
	'
	'EndIf
		    if(addr1 < &H6000) then
      return false '// not readable
    end if 
    if(addr1 < &H8000) then
     ' return this.prgRam[adr & 0x1fff]
    
      data1 =  this.prgram(addr1 and &H1FFF)
     	mapped_addr = &HFFFFFFFF 
     	 
     return true
    end if
    
    
    
    dim _base as uint32_t = (this.prgbank) * PRG_BANK_SIZE 
    mapped_addr   = _base or (addr1 and &H7fff) 
    return true               
    
    
    
    
   ' mapped_addr  = this.getRomAddr1(addr1)
	
	
End Function 

  Function Mapper_34.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
  
   	
  	if(addr1 < &H6000)  then
      return false' // no mapper registers
  	end if
    if(addr1 < &H7FFD)  then
     ' this.prgRam[adr & 0x1fff] = value;
     'this.prgram(addr1-&H6000) = data1 
     '
     '	mapped_addr = &HFFFFFFFF 
      return true
    end if
  
  
    if addr1 = &H7ffd then
  		
  		this.prgbank = data1 and &B00000001
  		' this.prgram(&H1FFD) = data1
  		'    	mapped_addr = &HFFFFFFFF 
  		return true
  EndIf
 	
    
    if addr1 = &H7ffE then
  		'
  		'this.chr_bank0  = data1 and &B00001111
  		' this.prgram(&H1FFE) = data1
  		'    	mapped_addr = &HFFFFFFFF 
  		return true
  EndIf
 	
      if addr1 = &H7ffF then
  		
  		'this.chr_bank1  = data1 and &B00001111
  		'this.prgram(&H1FFF) = data1
  		'    	mapped_addr = &HFFFFFFFF 
  		return true
  EndIf
 	
  
  
  
  
  
  'if this.board = this.boards.NINA001 then
	'if addr1 >= &H6000 and addr1 <= &H7FFC then
		
		
	' this.vRAMStatic[addr1 - &H6000] = data1 
  '   	mapped_addr = &HFFFFFFFF 
  '   	 
  '   return true
		
		
	'EndIf
  '			

  'if addr1 = &H7ffe then
  '		
  '		this.chr_bank0 = data1 and &B00001111
  '		 this.vRAMStatic[&H1FFe] = data1
  '		    ' return true
  'EndIf	
  '			  if addr1 = &H7fff then
  '		
  '		this.chr_bank1 = data1 and &B00001111
  '		 this.vRAMStatic[&H1FFF] = data1
  '		    ' return true
  '			  EndIf	
  			
  	'	else
  			
  			
  			
  	'	EndIf
  		
  End Function



Function Mapper_34.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 

	return false

End function 

Function Mapper_34.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 


	return FALSE

End Function

  Sub Mapper_34.resetmappper(hard as boolean)   
   if bat_saves = false then
   '	if this.vRAMStatic.size() <> 0 then
   	
   	this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
   	
   	''
    EndIf
   	
   	
   	



  End Sub
  
 
function Mapper_34.ppuPeak(adr As uint16_t) As uint8_t
	return this._ppuRead(adr)
End Function






