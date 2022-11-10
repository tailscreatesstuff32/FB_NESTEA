#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"

MVectorTemplate(uint8_t)
  Type Mapper_01 extends Mapper
  	
  	Public:
   Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
   declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	
  	
  	Declare function _ppuRead(adr As uint16_t) As uint8_t
   Declare function _ppuWrite(adr As uint16_t,ByRef value As uint32_t)  as uint8_t

  	
  	Declare function getmirroringadr(addr1 as uint16_t) as uint32_t
  	  	


Declare function getchraddr1(addr1 as uint16_t) as uint32_t


  	Declare function getromaddr1(addr1 as uint16_t) as uint32_t
  	
 chrram(&H2000) as uint8_t
 prgram(&H2000) as uint8_t
 ppuram(&H800) as uint8_t
  
  	
shiftreg as uint32_t = 0
shiftcount as uint32_t
mirroring as uint8_t = 0
ramEnable as uint8_t = 0
prgbank as uint8_t = 0
prgmode as uint8_t = 3
chrmode as uint8_t = 1
  	
  	chrbank0 as uint32_t
  	chrbank1 as uint32_t
  	
  	Declare Sub resetmappper(hard as boolean)
  	
   Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  	declare function _mirror() as MIRROR
  	 
 

   vRAMStatic As TVECTORUINT8_T
 
  End Type

Constructor Mapper_01(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks,chrbanks)
  
  this.vRAMStatic.resize(32*1024)

'resetmappper(true)


  'this.GetBattery()
  'this.SetBattery()	
  

  
 this.SetBattery() 
  
  
End Constructor
Destructor Mapper_01()


End Destructor


  Sub Mapper_01.resetmappper(hard as boolean)   

if hard then
	
	
	erase chrram
	erase ppuram
	erase prgram
	
      if bat_saves = false then
   '	if this.vRAMStatic.size() <> 0 then
   	
   	this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
   	
   	''
      EndIf
end if

shiftreg  = 0
shiftcount = 0

mirroring = 0
ramEnable = 0
prgbank = 0
prgmode = 3
chrmode = 1




  
			
  



  End Sub
  
  
  
  
  
  
   function Mapper_01.getmirroringadr(adr  as uint16_t) as uint32_t
 	
 	    select case this.mirroring 
      case 0 
       ' // 1-screen A
        return adr and &H3ff 
     
      case 1 
      '  // 1-screen B
        return &H400 + (adr and &H3ff) 
   
      case 2 
       ' // vertical
        return adr and &H7ff 
       
      case 3 
       ' // horizontal
        return (adr and &H3ff) or ((adr and &H800) shr 1) 
       
 	    end select
 	    
 	    
   End Function


  
function Mapper_01._ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t

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


function Mapper_01._ppuRead(adr As uint16_t) As uint8_t
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

  
  
  
  function Mapper_01.getchraddr1(addr1 as uint16_t) as uint32_t
 dim final as uint32_t
 
 if this.chrmode = 1 then
 	
 	if addr1 < &H1000 then
 		final = this.chrbank0 * &H1000 + (addr1 and &HFFF)
 		
 	else
 		final = this.chrbank1 * &H1000 + (addr1 and &HFFF)
 		
 		
 	EndIf
 else
 	final = (this.chrBank0 shr 1) * &H2000 + (addr1 and &H1fff)
 	
 	
 EndIf
 
 
 return final and this.chrAnd
 
	
  End Function
  
  function Mapper_01.getromaddr1(addr1 as uint16_t) as uint32_t
	
	
	 dim final as uint32_t
	 
	 select case this.prgmode
	 	
	 	case 0
	 	
	 		
	 	case 1
	 	    	final = &H8000 * (this.prgbank shr 1) + (addr1 and &H7FFF)
	 
	 	case 2
	 
	if addr1 < &HC000 then
	 	   	final = addr1 and &H3FFF
	 	   else	
	 	   	final = this.prgBank * &H4000 + (addr1 and &H3fff)
	 	   	
	 	   EndIf

	 	
	 	case 3
	 	
	 	
	 		   if addr1 < &HC000 then
	 	   	final = this.prgBank * &H4000 + (addr1 and &H3FFF)
	 	   else	
	 	   final = (this.nPRGBanks - 1) * &H4000 + (addr1 and &H3fff)
	 	   	
	 	   EndIf
	 	

	 	
	 	
	 	
	 	
	 End Select
	 

    return final and this.prgAnd 
	
	
	
End Function

  Function Mapper_01.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
		    if(addr1 < &H6000) then
      return false '// not readable
    end if 
    
    if(addr1 < &H8000) then
     ' return this.prgRam[adr & 0x1fff]
     
     if this.ramEnable = 1 then
     	return 0
     EndIf
     
     this.vRAMStatic[addr1 and &H1FFF] = data1 
     	mapped_addr = &HFFFFFFFF 

     return true
    end if

  if((data1 and &H80) > 0)  then
      this.shiftCount = 0 
      this.shiftReg = 0 
    else 
      this.shiftReg or= (data1 and &H1) shl this.shiftCount 
      this.shiftCount+=1
      if(this.shiftCount =  5) then
        select case  ((addr1 and &H6000) shr 13)  
          case 0 
            this.mirroring = this.shiftReg and &H3 
            this.prgMode = (this.shiftReg and &Hc) shr 2 
            this.chrMode = (this.shiftReg and &H10) shr 4 
            
       
          case 1 
            this.chrBank0 = this.shiftReg 
     
           
          case 2 
            this.chrBank1 = this.shiftReg 
           
        
          case 3  
            this.prgBank = this.shiftReg and &Hf 
            this.ramEnable = (this.shiftReg and &H10) shr 4 
      
       
     
    
        end select
        
            
    this.shiftCount = 0 
        this.shiftReg = 0 
     endif
 endif





End function 

Function Mapper_01.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 

	
		
	    if(addr1 < &H6000) then
      return false '// not readable
    end if 
    
    if(addr1 < &H8000) then
     ' return this.prgRam[adr & 0x1fff]
     
     if this.ramEnable = 1 then
     	return 0
     EndIf
     
       data1 = this.vRAMStatic[addr1 and &H1FFF]
     	mapped_addr = &HFFFFFFFF 

     return true
    end if
    
    mapped_addr  = this.getRomAddr1(addr1)
	return true
	
	
	
	

	'return false 
End function 
  
Function Mapper_01.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
	

	return false 
 
End Function


Function Mapper_01.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 

End Function



function  Mapper_01._mirror() as MIRROR
	Return this.mirrormode
End Function

 function Mapper_01.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
								dim sram_save as integer = FreeFile
			'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" &
			'chdir("C:\NESTEA_BATTSAVES")
 'romname = mid(romname,19,5)
 
	open "C:\NESTEA_BATTSAVES\"  & romname & ".batt" for binary as sram_save
			' 
			 put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			'
			close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_01.SetBattery()  as boolean'TVECTORUINT8_T
dim sram_save as integer = freefile
	
	'"C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\" & 
	'chdir("C:\NESTEA_BATTSAVES")
 ' romname = mid(romname,19,5)
	if fileexists("C:\NESTEA_BATTSAVES\" & romname & ".batt"  ) then
	open "C:\NESTEA_BATTSAVES\"  & romname & ".batt" for binary as sram_save
			 
			'   this.vRAMStatic.clear						
  'this.vRAMStatic.resize(32*1024)
  ' 
			 		 
			 get # sram_save,,this.vRAMStatic[0],32*1024
			
			close sram_save
			return true 
	else		
		

			return false
	end if
 End Function
