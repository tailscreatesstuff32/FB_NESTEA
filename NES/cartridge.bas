
'#Include Once "nes/cartridge.bi"


#Include Once "windows.bi"
#Include Once "containers/vector.bi"
'#include "nes/Cartridge.bi"
#include "file.bi" 

'#include once "CRC32.bas"

'#Include Once "mapper_000.bas"
'#Include Once "mapper_002.bas"

'#Include Once "mapper_004_TL29-3.bas"
'#Include Once "mapper_004_TL29.bas"
'#Include Once "mapper_004_fixes-TL29.bas"

'#Include Once "mapper_004_TL29-2.bas"


'#Include Once "mapper_004_TL29-FIXES.bas"


'#Include Once "mapper_004.bas"

'

	MVectorTemplate(uint8_t)
	Dim Shared vPRGMemory As TVECTORUINT8_T
	Dim Shared vCHRMemory As TVECTORUINT8_T
	

dim shared romname as string


dim shared bat_saves as boolean

Type sHeader
	 
		Name1 As String* 4-1
		prg_rom_chunks As uint8_t 
		chr_rom_chunks As uint8_t  
	   mapper1 as uint8_t
		mapper2 As uint8_t 
		prg_ram_size As uint8_t 
		tv_system1 As uint8_t 
		tv_system2 As uint8_t  
		unused As String * 5-1
		
	End Type
	
	Dim Shared header As sHeader

#include "NES/mappers/mapper00_NEW_2.bas"
'#include "NES/mappers/mapper00_NEW.bas"
#include "NES/mappers/mapper01_NEW_2.bas"
'#include "NES/mappers/mapper01_NEW.bas"
#include "NES/mappers/mapper02_NEW_2.bas"
'#include "NES/mappers/mapper02_NEW.bas"
#include "NES/mappers/mapper03_NEW_2.bas"
'#include "NES/mappers/mapper03_NEW.bas"

#include "NES/mappers/mapper04_NEW_V2.bas"
'#include "NES/mappers/mapper04_NEW.bas"

'#include "NES/mappers/mapper66_NEW.bas"
#include "NES/mappers/mapper66_NEW_2.bas"
#include "NES/mappers/mapper69_NEW_2.bas"

'#include "NES/mappers/mapper07_NEW.bas"
#include "NES/mappers/mapper07_NEW_2.bas"

'#include "NES/mappers/mapper37_NEW.bas"

 '#include "NES/mappers/mapper34_NEW_2.bas"

'#include "NES/mappers/mapper65_NEW.bas




dim shared pmapper as mapper ptr 


	Declare Function cart_cpuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
	Declare function cart_cpuWrite(addr1 As uint16_t,  data1 As uint8_t)As bool

	'// Communication with PPU Bus
	
	Declare Function cart_ppuRead(addr1 As uint16_t, Byref data1 As uint8_t)As bool
	Declare Function cart_ppuWrite(addr1 As uint16_t,  data1 As uint8_t) As bool
   Declare Sub cart_reset(hard as boolean)
   Declare sub insert_cartridge( sFileName As String,hwnd as HWND = null)


   Dim shared As bool bImageValid = false
   
   Declare Function ImageValid() As bool
   Dim shared As uint8_t nMapperID  = 0 
	Dim Shared As uint8_t cart_nPRGBanks  = 0 
	Dim Shared As uint8_t cart_nCHRBanks  = 0 

	
	Dim Shared cart_mirror As MIRROR =  HORIZONTAL 
	
	
	
	
	
	bImageValid = FALSE
	

	'std::vector<uint8_t> vPRGMemory;
	'std::vector<uint8_t> vCHRMemory;
	Dim Shared hw_mirror As mirror
	
	'C:\Program Files (x86)\FreeBASIC107\my projects\OLCNES-FB'

	
	
	Sub insert_cartridge( sFileName As String,hwnd as HWND = null)
		Dim f As Integer = freefile

    romname = sFileName
	 romname = mid(romname,62,len(romname)-60)
'sleep
	
		Open  sFileName For Binary As #f
		
		  If FILEEXISTS(sFileName) THEN
        'PRINT "found rom"
        
        header.Name1 = ""
        header.prg_rom_chunks = 0
         header.chr_rom_chunks = 0
       header.mapper1 = 0
 header.mapper2 = 0
 header.prg_ram_size = 0
 header.tv_system1 = 0
 header.tv_system2 = 0
 header.prg_ram_size = 0
  header.unused = ""

   

       Open sFileName FOR BINARY AS #f


        GET #f, , header
        
        	
        IF header.mapper1 AND &H04 THEN
          ' PRINT "has trainer-skipping"
            SEEK #f, 512

        ELSE
          ' PRINT "no trainer"
        END IF
'sleep
		'// Determine Mapper ID
		nMapperID = ((header.mapper2 shr 4) shl 4) or(header.mapper1 shr 4) 
		hw_mirror = IIf((header.mapper1 And &H01), VERTICAL, HORIZONTAL )
		
		'WIP
		bat_saves =  iif((header.mapper1 And &H02),true,false)
		

		'// "Discover" File Format
		Dim As uint8_t nFileType = 1 
        
      If (nFileType = 0) Then
 
      End If
        
        
        
        
        if (nFileType  = 1) Then
		 
			cart_nPRGBanks = header.prg_rom_chunks 
			vPRGMemory.resize(cart_nPRGBanks * 16384) 
			
			 GET #f, ,vPRGMemory[0],vPRGMemory.size()
			
			'ifs.read((char*)vPRGMemory.data(), vPRGMemory.size());
			
			cart_nCHRBanks = header.chr_rom_chunks
			'nCHRbanks = header.chr_rom_chunks
			
       if (cart_nCHRBanks = 0) Then
     	vCHRMemory.resize(8192)
       Else
        	vCHRMemory.resize(cart_nCHRBanks * 8192)
       EndIf
        GET #f, ,vCHRMemory[0],vCHRMemory.size()
        End If
     
        
        
			'nCHRBanks = header.chr_rom_chunks;
			'if (nCHRBanks == 0)
			'{
		'		// Create CHR RAM
			'	vCHRMemory.resize(8192);
			'}
			'else
			'{
		'		// Allocate for ROM
			'	vCHRMemory.resize(nCHRBanks * 8192);
			'}
			'ifs.read((char*)vCHRMemory.data(), vCHRMemory.size());
       
        
        
        	if (nFileType = 2) Then
 
        	End If
        
        
        
        
'		'TODO add mappers again and see if it can use pointers
'        
'       ' // Load appropriate mapper
'		Select Case (nMapperID)
'		 
'		Case   0  'pMapper = std::make_shared<Mapper_000>(nPRGBanks, nCHRBanks); break;
'		pmapper = new mapper_00(nPRGBanks,nCHRBanks)
'		
'		'works fine
' 	'mapper_000(cart_nPRGBanks,cart_nCHRBanks)     
'		
'		'case   2 ' is actually working so far - but an issue
'	'mapper_002(cart_nPRGBanks,cart_nCHRBanks)  
'		
'		'//case   2: pMapper = std::make_shared<Mapper_002>(nPRGBanks, nCHRBanks); break;
'		'//case   3: pMapper = std::make_shared<Mapper_003>(nPRGBanks, nCHRBanks); break;
'	'Case   4: ' issues
'' mapper_004(cart_nPRGBanks,cart_nCHRBanks)  
'		'//case  66: pMapper = std::make_shared<Mapper_066>(nPRGBanks, nCHRBanks); break;
'		Case Else
'		Print "invalid mapper ";nMapperID;"! "; "only mapper 0 games for now..."
'		
'		Sleep
'		End
'		
'		End select 
        

       ' // Load appropriate mapper
		Select Case (nMapperID)
		 
			Case   0 	
			pmapper = new mapper_00(cart_nPRGBanks,cart_nCHRBanks ) 
			'mapper_000(cart_nPRGBanks,cart_nCHRBanks)
			Case   1 
			pmapper = new mapper_01(cart_nPRGBanks,cart_nCHRBanks) 
			case   2 ' issues needs desparate issue solving
			pmapper = new mapper_02(cart_nPRGBanks,cart_nCHRBanks )  
			case   3
			pmapper = new mapper_03(cart_nPRGBanks,cart_nCHRBanks)
			Case   4 ' issues needs desparate issue solving
			pmapper = new mapper_04(cart_nPRGBanks,cart_nCHRBanks)
			'mapper_004(cart_nPRGBanks,cart_nCHRBanks) 
			case 37
				
			'	pmapper = new mapper_37(cart_nPRGBanks,cart_nCHRBanks)
				Case 7
				pmapper = new mapper_07(cart_nPRGBanks,cart_nCHRBanks)
			'case  65
				
				'pmapper = new mapper_65(cart_nPRGBanks,cart_nCHRBanks)
				
			case  66
			pmapper = new mapper_66(cart_nPRGBanks,cart_nCHRBanks)
			
			case 69
				pmapper = new mapper_69(cart_nPRGBanks,cart_nCHRBanks)
			
			
		'	case 34
		  ' pmapper = new mapper_34(cart_nPRGBanks,cart_nCHRBanks)
			
			
			
			Case Else
			Print "invalid mapper ";nMapperID;"! "'; "only mapper 0 games for now..."
			Print " "
			Print "press any key to continue"
			if hwnd <> null then
				

	SendMessage(hwnd, WM_CLOSE, 0, 0)
	else
	
	End If
			Sleep
			End
		
		End select 
        
        
        
        
        
        
        
        
        
        
        
        
        
        
		Close #f
		bImageValid = true
	
		End If
		
	End Sub
	
	
	Sub cartqb64 ' for reference
	'	
	'	    DIM orig_dest AS _UNSIGNED LONG
   ' DIM nPRGBanks AS _UNSIGNED _BYTE
   ' DIM nCHRBanks AS _UNSIGNED _BYTE
   ' $CONSOLE

   ' _CONSOLETITLE "NEStea Debuggger"


   ' orig_dest = _DEST
   ' 'mapper ????
   ' IF cart_info_disp = 1 THEN
   '     _CONSOLE ON


   '     _DEST _CONSOLE
   ' ELSE
   '     _CONSOLE OFF


   ' END IF

   ' nMapperID = 0
   ' nPRGBanks = 0
   ' nCHRBanks = 0

   ' bImageValid = -1


   ' file$ = cart

   ' IF _FILEEXISTS(file$) THEN
   '     PRINT "found rom"



   '     OPEN file$ FOR BINARY AS #1


   '     GET #1, , header

   '     PRINT "magic number: ";
   '     PRINT header.name_nes



   '     IF header.mapper1 AND &H04 THEN
   '         PRINT "has trainer-skipping"
   '         SEEK #1, 512

   '     ELSE
   '         PRINT "no trainer"
   '     END IF



   '     IF header.mapper1 AND &H01 THEN

   '         cart_mirror = VERTICAL


   '         mirror$ = "VERTICAL"
   '     ELSE

   '         cart_mirror = HORIZONTAL



   '         mirror$ = "HORIZONTAL"
   '     END IF
   '     'ONESCREEN_LO
   '     'ONESCREEN_HI


   '     PRINT "mirror mode: "; mirror$


   '     nMapperID = _SHL(_SHR(header.mapper2, 4), 4) OR _SHR(header.mapper1, 4)



   '     nFileType = 1

   '     IF header.mapper2 AND &H0C = &H08 THEN
   '         nFileType = 2
   '     END IF



   '     IF nFileType = 0 THEN

   '     END IF

   '     IF nFileType = 1 THEN
   '         PRINT "file type:" + STR$(nFileType)
   '         nPRGBanks = header.prg_rom_chunks
   '         REDIM vPRGMemory((nPRGBanks * 16384) - 1)
   '         GET #1, , vPRGMemory()
   '         PRINT "NUM PRG BANKS:" + STR$(nPRGBanks)
   '         PRINT "PRG ROM SIZE:" + STR$(LEN(vPRGMemory()))

   '         nCHRBanks = header.chr_rom_chunks

   '         PRINT "NUM CHR BANKS:" + STR$(nCHRBanks)
   '         IF nCHRBanks = 0 THEN
   '             REDIM vCHRMemory(8192 - 1)
   '             PRINT "CHR RAM: " + STR$(LEN(vCHRMemory()))
   '         ELSE
   '             '   PRINT "CHR ROM"
   '             REDIM vCHRMemory((nCHRBanks * 8192) - 1)
   '             PRINT "CHR ROM SIZE:" + STR$(LEN(vCHRMemory()))

   '         END IF
   '         GET #1, , vCHRMemory()


   '     END IF


   '     IF nFileType = 2 THEN
   '         PRINT "file type WORK IN PROGRESS:" + STR$(nFileType)
   '     END IF

   '     SELECT CASE nMapperID

   '         CASE 0:

   '             PRINT "valid mapper-found Mapper:" + STR$(nMapperID)


   '             mapper_000 nPRGBanks, nCHRBanks

   '             'CASE 1:

   '             '    PRINT "valid mapper-found Mapper WORK IN PROGRESS: " + STR$(nMapperID)

   '             '    ' mapper_001 nPRGBanks, nCHRBanks

   '         CASE 2:

   '             PRINT "valid mapper-found Mapper WORKING:" + STR$(nMapperID)
   '             mapper_002 nPRGBanks, nCHRBanks

   '         CASE 3:

   '             PRINT "valid mapper-found Mapper WORKING:" + STR$(nMapperID)
   '             mapper_003 nPRGBanks, nCHRBanks


   '         CASE 4:

   '             PRINT "valid mapper-found Mapper WORK IN PROGRESS: " + STR$(nMapperID)

   '             mapper_004 nPRGBanks, nCHRBanks

   '             '    'CASE 65:

   '             '    '    PRINT "valid mapper-found Mapper WORK IN PROGRESS:" + STR$(nMapperID)

   '             '    ' mapper_065 nPRGBanks, nCHRBanks


   '         CASE 66:

   '             PRINT "valid mapper-found Mapper WORK IN PROGRESS:" + STR$(nMapperID)

   '             mapper_066 nPRGBanks, nCHRBanks

   '         CASE ELSE:
   '             PRINT "invalid mapper:" + STR$(nMapperID)
   '     END SELECT

   '     bImageValid = 1
   '     CLOSE #1

   ' ELSE
   '     bImageValid = 0
   '     PRINT "rom not found"
   ' END IF


   ' IF cart_info_disp = 1 THEN
   '     _DEST orig_dest
   ' END If
	End Sub
	
	
	Sub cart_reset(hard as boolean)
		
		
		if pmapper <> null  then
			
			pmapper->resetmappper(hard)
			
		EndIf
		
		
		
	   'TODO add reset mapper again
	'	resetmapper
		
	End Sub
	
	function getmapper() as Mapper ptr
		
		return pMapper 
		
	End function
	
	
	
	Function ImageValid() As bool
		return bImageValid
	End Function
	Function cart_cpuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
		Dim As uint32_t mapped_addr = 0
	if (pmapper->cpuMapRead(addr1, mapped_addr,data1)) Then

		if (mapped_addr = &HFFFFFFFF) then
	 
			'// Mapper has actually set the data value, for example cartridge based RAM
			return true 
		 
		else
	 
		'	// Mapper has produced an offset into cartridge bank memory
			Data1 = vPRGMemory[mapped_addr] 
		End If
		return true 
	 
	else
		return false 
		
End If
	End Function
	
	
	Function cart_cpuWrite( addr1 As uint16_t,  data1 As uint8_t) As bool 
    DIM mapped_addr AS  uint32_t


    mapped_addr = 0


    IF pmapper->cpuMapWrite(addr1, mapped_addr, data1) THEN ' cpuMapWrite(addr, mapped_addr, byte_data) THEN

        IF mapped_addr = &HFFFFFFFF  THEN

           
              Return TRUE

        ELSE
            vPRGMemory[mapped_addr] = data1
        END IF
      
        Return TRUE


    ELSE

           Return FALSE

    END If
		
	End Function
	
	
	Function cart_ppuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
		 DIM mapped_addr AS uint32_t = 0
	if (pmapper->ppuMapRead(addr1, mapped_addr)) then
	 
	data1 = vCHRMemory[mapped_addr] 
			 
		return true 
	 
	else
      return FALSE
	End If
	
	End Function
	
	Function cart_ppuWrite( addr1 As uint16_t,  data1 As uint8_t) As bool 
	    DIM mapped_addr AS uint32_t = 0

    mapped_addr = 0

    If pmapper->ppuMapWrite(addr1, mapped_addr) THEN
        vCHRMemory[mapped_addr] = data1

        'cart_ppuWrite = 1
        return true
    Else

      '  cart_ppuWrite = 0
        return false
    END If
	End Function
	

 Function cart_MirrorMode() As MIRROR
 
	'Dim m As Mirror = _mirror()
	'if (m = HARDWARE) Then
	'
	'	'// Mirror configuration was defined
	'	'// in hardware via soldering
	'	return hw_mirror
	' 
	'else
 
	''	// Mirror configuration can be
	''	// dynamically set via mapper
	'	Return m
	'End If
	'Return 	hw_mirror 'hw_mirror
 End Function

 Function cart_MirrorMode_NEW() As MIRROR
 
	Dim m As Mirror = pmapper->_mirror()
	if (m = HARDWARE) Then
	
		'// Mirror configuration was defined
		'// in hardware via soldering
		return hw_mirror
	 
	else
 
	'	// Mirror configuration can be
	'	// dynamically set via mapper
		Return m
	End If
	Return 	hw_mirror 'hw_mirror
 End Function





'sleep
