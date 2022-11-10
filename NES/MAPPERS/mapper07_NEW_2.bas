#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"
#include once "file.bi"


MVectorTemplate(uint8_t)
  
  
  dim shared mirraddr as uint16_t
  Type Mapper_07 extends Mapper
  	
  	'chrram(&H2000) as uint8_t

 	prgbank as uint32_t = 0'&H07
 	
  
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  		Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  Declare Sub resetmappper(hard as boolean)
  	'
   Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  	declare function _mirror() as MIRROR
  	
  	Declare function getchraddr1(addr1 as uint16_t) as uint32_t
  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t
  	'
  	'
  	'private:

  	
  	  	  	  		Declare function _ppuRead(adr As uint16_t) As uint8_t
   	Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t
  	  	
  	  	 chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
  

  End Type
  
  function  Mapper_07._mirror() as MIRROR
	Return this.mirrormode
  End Function
  
  Constructor Mapper_07(prgbanks As uint8_t,chrbanks As uint8_t)
   base(prgbanks,chrbanks)
   
   this.resetmappper(true)
   
   
  End Constructor
  function Mapper_07.getmirroringadr(adr  as uint16_t) as uint32_t
    if(this.mirroring = 0)  then
     ' // A
   
        return adr and &H3ff 
      else  
    '  // horizontal
   
      
        return   &H400  + (adr and &H3ff) 
    end if
    
  End Function
  
  function Mapper_07._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

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
  
End function 


function Mapper_07._ppuRead(adr As uint16_t) As uint8_t
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

  
  
  
   function Mapper_07.getchraddr1(addr1 as uint16_t) as uint32_t
 	
 	
 	
 	return addr1
 	
 	
   End Function
 function Mapper_07.getromaddr1(addr1 as uint16_t) as uint32_t
 	'
 	dim final as uint32_t = this.prgBank * &H8000 + (addr1 and &H7fff) 
 	
 	return final and this.prgand
 	
 	
 End Function
Function Mapper_07.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
  if addr1 < &H8000 then
 	
 	return false
 	
 	
  End If
  '
  
   this.mirroring = (data1 and &H10) shr 4
 this.prgbank   = data1 and &Hf
 	if (data1 and &H10) = 0 then
		
		this.mirrormode = ONESCREEN_LO
		
	else
		this.mirrormode = ONESCREEN_HI
		
		
	EndIf
	
	
	
	
' if addr1 >= &H8000 and addr1 <= &HFFFE then
'	
'	
'
'	
'	'return true
'		'
'EndIf

'	return true
 
  '
  '
  '    if(this.mirroring = 0) then
  '   ' // A
  '    return  addr1 and &H3ff 
  '  else
  '    '// B
  '    return &H400 + (addr1 and &H3ff) 
  ' 
  'end if
  'return true
 



'EndIf









End function 

Function Mapper_07.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 

  if addr1 < &H8000 then
 	
 		return false
 	
 	
  EndIf
  
mapped_addr = getromaddr1(addr1) ' data1 and &Hf
return true
'
'if addr1 >= &H8000 and addr1 <= &HFFFE then
'	
'mapped_addr = this.prgBank * &H8000 + (addr1 and &H7fff) 
'
'
'
'
'	
'	
'EndIf

	'return true
'return false







End function 
  
  
  
  
  function Mapper_07.ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN 
 
	
  'if addr >= &H0000 and addr <=  &H1FFF then
	
  'If (this.nCHRBanks = 0) then'// Treating as RAM
	 
		'	mapped_addr = addr  
			'return true 
  'End If



	
	
  'End If
  '
  'return false
  '
  
      if(addr < &H2000) then
      if(this.nCHRBanks = 0) then
       ' return this.chrRam[this.getChrAdr(adr)];
       	mapped_addr = addr
       	return true
      else 
      	mapped_addr = addr
        'return this.rom[this.h.chrBase + this.getChrAdr(adr)];
        return false
      end if
      else 
      return false'this.ppuRam[this.getMirroringAdr(adr)];
   end if
  
  'return false
  
' 
' 
'   if addr >= &H2000 and addr <=  &H3FFF then
'	
'   mapped_addr =  addr 
'	return false
'
'
''
''	
''	
'   End If
'  
'  
'  
'  
  
  
  
  
  
  End Function 
 function Mapper_07.ppumapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
 	' 
	'if (addr  < &H2000) Then
	' 
	'	If (this.nCHRBanks = 0) then'// Treating as RAM
	' 
	'		mapped_addr = addr  
	'		return true 
	'	End If
	'else
	'	
	'	mirraddr = addr
	'	this.getmirroringaddr(addr)
	'	
	'	return false	
	'end if

	'' 
	
  'if addr >= &H0000 and addr <=  &H1FFF then
	
  'If (this.nCHRBanks = 0) then'// Treating as RAM
	 
			'mapped_addr = addr  
			'return true 
  'End If


	
	
  'End If
  '
  'return false
  '
 
 
  ' if addr >= &H2000 and addr <=  &H3FFF then
	
'	select case 	this.mirrormode
		
		'case ONESCREEN_LO
	'	mapped_addr = addr 'and &H3ff 
		
		'case ONESCREEN_HI
  ''    mapped_addr = addr '&H400 + (addr and &H3ff)  
		
		
		
	'End Select
	

	
 	

  'End If
 
 
    if(addr < &H2000)  then
      if(this.nCHRBanks = 0) then
        'this.chrRam[this.getChrAdr(adr)] = value;
        
        
        mapped_addr = addr
        
        return true
      else 
       ' // not writable
        return false
      end if
      else 
     ' return this.ppuRam[this.getMirroringAdr(adr)] = value 
     
    
      return false
     
     
    End If
 
 
  'return true
 
 
 
End Function  
'  
'  
'  
  function Mapper_07.SetBattery()  as boolean	
  	
  	
  End Function
    function Mapper_07.GetBattery()  as boolean	
  	
  	
    End Function
    
        sub Mapper_07.resetmappper(hard as boolean) 
  	    if(hard)  then
      '// clear chr ram
    ' erase this.chrram
      '// clear ppu ram
      
      end if
   

    this.prgBank = 0 
    this.mirroring = 0 
  	
        End sub
  

  
  
  