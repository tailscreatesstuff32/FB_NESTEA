#Include "windows.bi"
#Include Once"crt.bi"

#include once "containers/vector.bi"

MVectorTemplate(uint8_t)

Enum MIRROR
HARDWARE
HORIZONTAL
VERTICAL
ONESCREEN_LO
ONESCREEN_HI
End Enum

 	dim shared prg_banks(4) as uint32_t
  Type Mapper extends object
  	
  	Public:
  	Declare Constructor(prgbanks As uint8_t = 0,chrbanks As uint8_t = 0)
  	declare Destructor()
  	
  	Public:
  	Declare abstract function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare abstract function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare abstract function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare abstract function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
   
   'nIRQCounter As uint16_t 
	' nIRQReload  As uint16_t
	
  	declare virtual function GetBattery() as boolean' TVECTORUINT8_T ptr  'TVECTORUINT8_T
  	declare virtual function SetBattery() as boolean 'TVECTORUINT8_T
  	
   declare virtual function GetIrqReloadVal()  as uint16_t 
  	
  	Declare virtual function irqstate() as bool
  	Declare virtual Sub irqclear()
  	Declare virtual Sub clock_irq()
  	
  	Declare virtual Sub get_scanline()
  	Declare virtual Sub resetmappper(hard as boolean)
  	
  	vRAMStatic As TVECTORUINT8_T
  

  	Declare virtual Function _mirror() As MIRROR
  	
  	  	prgbank as uint32_t
  	  	  	mirroring as uint8_t
  	  	  		mirrormode as MIRROR '= ONESCREEN_HI
  	  	  		
  	  	  		Declare virtual function _ppuRead(adr As uint16_t) As uint8_t
   	Declare virtual function _ppuWrite(adr As uint16_t,ByRef value As uint32_t) as uint8_t


  	  	  		Declare virtual function ppuPeak(adr As uint16_t) As uint8_t


  	Declare virtual function getmirroringaddr(addr1 as uint16_t) as uint32_t
  	Declare virtual function getchraddr1(addr1 as uint16_t) as uint32_t
  	
  	Declare virtual function getmirroringadr(addr1 as uint16_t) as uint32_t
  	Declare virtual  function getchradr(adr as uint16_t) as uint32_t
  	
  	  	enum boards
  	BxROM
  	NINA001
  	End Enum
  	
  	board as boards
  	
  	 bIRQActive As bool = false 
  	
  	  	reloadirq as boolean
irqlatch as uint16_t
irqcounter as  int16_t
irqEnabled as boolean
lastRead as uint16_t
   prgand as uint32_t
   chrand as uint32_t
    	'irq_counter as uint16_t
  	
  	  	 ' 	chr_banks(8) as uint32_t
 ' 	prg_banks(4) as uint32_t
  	
  	Protected:
  	nPRGBanks As uint8_t = 0
   nCHRBanks As uint8_t = 0
   
   SRAM(32*1024) as uint8_t
   

  		chrbase as uint32_t
  		

  	
  	
  End Type



sub Mapper.clock_irq()
	
End sub


function Mapper._ppuWrite(adr As uint16_t,ByRef value As uint32_t) As uint8_t
	
End function


function Mapper._ppuRead(adr As uint16_t) As uint8_t
	
End Function

 function Mapper.getmirroringaddr(addr1 as uint16_t) as uint32_t
    if(this.mirroring = 0) then
     ' // A
      return  addr1 and &H3ff 
    else
      '// B
      return &H400 + (addr1 and &H3ff) 
   
  end if
   End Function
   
   function Mapper.getmirroringadr(adr  as uint16_t) as uint32_t
    if(this.mirroring = 0) then
     ' // A
      return  adr and &H3ff 
    else
      '// B
      return &H400 + (adr and &H3ff) 
   
  end if
   End Function


   function Mapper.getchradr(adr  as uint16_t) as uint32_t
    if(this.mirroring = 0) then
     ' // A
      return  adr and &H3ff 
    else
      '// B
      return &H400 + (adr and &H3ff) 
   
  end if
   End Function




   function Mapper.getchraddr1(adr  as uint16_t) as uint32_t
    if(this.mirroring = 0) then
     ' // A
      return  adr and &H3ff 
    else
      '// B
      return &H400 + (adr and &H3ff) 
   
  end if
   End Function









Destructor Mapper()

End Destructor





  Constructor Mapper(prgbanks As uint8_t,chrbanks As uint8_t)
  
  
   	this.nPRGBanks = prgbanks 
   	this.nCHRBanks = chrbanks
   	
   	this.prgand = (this.nPRGBanks *&H4000) - 1
   	this.chrand = iif(nCHRBanks = 0,&H1FFF,(this.nCHRBanks*&H2000)-1)
   
   	

  
		this.resetmappper(true)
  
  End Constructor
  
 
function Mapper.ppuPeak(adr As uint16_t) As uint8_t

End Function


  function Mapper.irqstate() as bool
  	
  	
  	
  	
  End function 
    Sub Mapper.irqclear()
  	
  	
  	
  	
    End Sub
    Sub Mapper.get_scanline()
  	
  	
  	
  	
    End Sub
  Sub Mapper.resetmappper(hard as boolean)   
  	
  	
  	
  	
  End Sub
  Function Mapper._mirror() As MIRROR
  	
  	Return MIRROR.HARDWARE	
  End Function


 function Mapper.GetIrqReloadVal()  as uint16_t
	
	
 End Function

 function Mapper.GetBattery()  as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
	
	
 End Function
 function Mapper.SetBattery()  as boolean'TVECTORUINT8_T
	
	
	
	
 End Function