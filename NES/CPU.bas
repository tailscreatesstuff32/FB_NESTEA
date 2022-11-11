#pragma once


#Include Once "windows.bi"
#Include Once "containers/vector.bi"
#Include Once "containers/map.bi"
#Include Once "containers/list.bi"
#include Once "file.bi"
#include once "crt.bi"


#include once "NES\CPU.bi"

Type UINT16T As uint16_t

MMapTemplate(UINT16T ,String)
'#include "Bus.bi"
'#include "NSF.bi"




constructor CPU()

self = @this
'this._reset()

End Constructor
	
	




sub CPU.dobranch(test as bool,rel as uint32_t)
	if test then
		this.cyclesLeft+=1
		 if((this.br(PC) shr 8) <> ((this.br(PC) + rel) shr 8))  then
         ' // taken branch across page: another extra cycle
          this.cyclesLeft+=1
		 end if
        this.br(PC) += rel 

	EndIf
	
End Sub


sub CPU._write( _a as uint16_t,_d as uint8_t)
	
	
'this.bus->_write(a,d)

bus->_write(_a,_d)
	
End Sub
function CPU._read( _a as uint16_t) as uint8_t
	
	
return bus->_read(_a, false)
	
End function 


sub CPU.ConnectBus(_n as any ptr)
	
  this.bus = _n

End Sub


sub CPU._clock
	 
	if this.cyclesleft= 0 then
		dim _instr as uint32_t = this._read(this.br(PC))
		 this.br(PC)+=1
		dim _mode as uint32_t = this.addressingmodes(_instr)
		this.cyclesleft = this.cycles(_instr)
		' not(this.i))
		  
		 if this.nmiwanted orelse (this.irqwanted andalso this.i = false)  then
		'if this.nmiwanted or  (this.irqwanted and  this.i = false)  then 
		
			this.br(PC)-=1
			if this.nmiwanted then
			this.nmiwanted = false
			_instr = &H100
		 
			else
				
			_instr = &H101
			
			EndIf
			_mode = this._IMP
			 this.cyclesLeft = 7
		EndIf
		dim eff as uint32_t = this.getAdr(_mode)
    this.functions(_instr)(eff) 
	EndIf
	this.cyclesLeft-=1
	
			 
	'If (this.cyclesleft = 0) Then
	'
	'	dim _instr as uint32_t = this._read(this.br(PC))
	'	dim _mode as uint32_t = this.addressingmodes(_instr)
	'	
	'	'this.U = true
	' 
	'	this.br(PC)+=1
	'	
	'	this.cyclesleft = this.cycles(_instr)
	'
	'	'Dim additional_cycle1 As uint8_t = this.lookup(this.opcode).addrmode() 
	'	'Dim additional_cycle2 As uint8_t =  this.lookup(this.opcode).operate() 
	'	
	'	
	'	
	'	'this.cycles += (additional_cycle1 And additional_cycle2)
	'	
	'	'this.SetFlag(this.U, true)
	'	
	'	'this.U = true
	'	
	'			if this.nmiwanted or (this.irqwanted and not(this.i)) then
	'		'beep
	'		this.br(PC)-=1
	'		if this.nmiwanted then
	'		this.nmiwanted = false
	'		_instr = &H100
	'		else
	'			
	'		_instr = &H101
	'		
	'		EndIf
	'		_mode = this._IMP
	'		 this.cyclesLeft = 7
	'			EndIf
	'			
	'	dim eff as uint32_t = this.getAdr(_mode)
   '   this.functions(_instr)(eff) 
	'EndIf
	'
	''this.clock_count +=1
	'
	'this.cyclesLeft-=1
	'print this.cycles 
	
End Sub




sub CPU._reset()
this.r(A) = 0	
this.r(X) = 0		
this.r(Y) = 0	

this.r(SP) = &HFD



this.br(PC) = this._read(&Hfffc) or (this._read(&Hfffd) shl 8) 


this.n = false
this.v = false
this.d = false
this.i = true
this.z = false
this.c = false


this.irqwanted = false
this.nmiwanted = false

this.cyclesLeft = 7

End Sub
	
	
 
function CPU.getP(bFlag as bool)  as uint8_t
  dim value as uint8_t 

      value or= iif(this.n, &H80,0) 
      value or= iif(this.v, &H40,0)
      value or= iif(this.d, &H08,0) 
      value or= iif(this.i, &H04,0) 
      value or= iif(this.z, &H02,0)  
      value or= iif(this.c, &H01,0)
      value or= &H20  '// bit 5 is always set
      value or= iif(bFlag, &H10,0) 

      return value 
	
End Function

sub CPU.setP( value as uint8_t )
	   this.n = iif((value and &H80)  > 0,true,false) 
      this.v = iif((value and &H40)  > 0,true,false)
      this.d = iif((value and &H08)  > 0,true,false)
      this.i = iif((value and &H04)  > 0,true,false) 
      this.z = iif((value and &H02)  > 0,true,false)  
      this.c = iif((value and &H01)  > 0,true,false)  
End Sub

sub CPU.setZandN( value as uint8_t )
      value and= &HFF

      this.z = iif(value = 0,true,false) 
      this.n = iif(value > &H7f,true,false)
	 
End sub

function CPU.getsigned(value as uint8_t) as int8_t
	
	if value > 127 then
		
		
		return -(256 - value)
		
	
	EndIf
	return value
	
End Function


function CPU.getAdr(mode as uint8_t) as uint32_t

	select case mode 
		
		case this._IMP
		return 0
		return this.r(this.a)
		
		case this._IMM
		
		'self->addr_abs =  this.br(this.PC)	
		'this.br(this.PC)+=1
		'return self->addr_abs
		
		_temp =  this.br(this.PC)	
		this.br(this.PC)+=1
		return _temp 
		
		
		'self->addr_abs = self->pc:self->pc+=1
		'return self->addr_abs
		
		case this._ZP 
		'temp = this.br(this.PC)
		'this.br(this.PC)+=1
		'
		'return temp
		   
		   
	'self->addr_abs = self->_read(this.br(this.PC))
	'this.br(this.PC)+=1
   ''self->addr_abs And= &H00FF
   
   
   
   
   	_temp = self->_read(this.br(this.PC))
	this.br(this.PC)+=1
   'self->addr_abs And= &H00FF
	Return  _temp
		   
		   
		   
		   
	'this.addr_abs = this._read(self->pc)
	'this.pc+=1
   'this.addr_abs And= &H00FF
	'Return 	 this.addr_abs
	'	
		case this._ZPX
			
			dim adr as uint32_t = this._read(this.br(this.PC))
			this.br(this.PC) +=1
			return (adr + this.r(this.X)) and &HFF 
			
			  'this.addr_abs = (this._read(this.br(this.PC)) + this.r(this.x))
	        'this.pc+=1
           'this.addr_abs And= &H00FF
			
			
		case this._ZPY
			
			dim adr as uint32_t = this._read(this.br(this.PC))
			this.br(this.PC) +=1
			return (adr + this.r(this.Y)) and &HFF 
			
			
			'  this.addr_abs = (this._read(this.br(this.PC)) + this.r(this.Y))
	      '  this.pc+=1
         '  this.addr_abs And= &H00FF
			'
			'return this.addr_abs
			
			
		case 	this._IZX
			dim adr as uint32_t = (this._read(this.br(this.PC)) + this.r(this.X)) and &HFF
			this.br(this.PC) +=1
			return this._read(adr) or (this._read((adr+1) and &HFF) shl 8)
			
			
			
'	Dim t As uint16_t = self->_read(this.br(this.PC))
'	this.br(this.PC)+=1
'
'	Dim lo As uint16_t = self->_read(cast(uint16_t,t + Cast(uint16_t,this.r(this.X))) And &H00FF)
'	dim hi  As  uint16_t = self->_read(cast(uint16_t,t + Cast(uint16_t,this.r(this.X)+ 1)) And &H00FF)
'
'	self->addr_abs = (hi shl 8) or lo
''
'	return self->addr_abs
			
			
			
		case 	this._IZY
			dim adr as uint32_t = this._read(this.br(this.PC))
			this.br(this.PC) +=1
			dim radr as uint32_t = this._read(adr) or (this._read((adr+1) and &HFF) shl 8)
			
			return (radr + this.r(this.Y)) and &HFFFF
			
		case 	this._IZYr
			dim adr as uint32_t = this._read(this.br(this.PC))
			this.br(this.PC) +=1
			dim radr as uint32_t = this._read(adr) or (this._read((adr+1) and &HFF) shl 8)
			    if((radr shr 8) < ((radr + this.r(Y)) shr 8))  then
            this.cyclesLeft+=1 
          end if
       
			return (radr + this.r(this.Y)) and &HFFFF
			
			
		case this._ABS 
          '// absolute
          dim adr as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          adr or= (this._read(this.br(PC)) shl 8)
          this.br(this.PC) +=1
          return adr
       
			
			 case this._ABX 
          '// absolute, indexed by x (for RMW and writes)
          dim adr as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          adr or= (this._read(this.br(PC)) shl 8)
          this.br(this.PC) +=1
          return (adr + this.r(X)) and &Hffff 
        
			
			 case this._ABXr 
          '// absolute, indexed by x (for reads)
          dim adr as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          adr or= (this._read(this.br(PC)) shl 8)
          this.br(this.PC) +=1
          if((adr shr 8) < ((adr + this.r(X)) shr 8)) then
            this.cyclesLeft+=1
          end if
          return (adr + this.r(X)) and &Hffff 
        
			
			  case this._ABY
          '// absolute, indexed by y (for RMW and writes)
          dim adr as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          adr or= (this._read(this.br(PC)) shl 8)
          this.br(this.PC) +=1
          return (adr + this.r(Y)) and &Hffff 
      
			 case this._ABYr
          '// absolute, indexed by y (for reads)
          dim adr as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          adr or= (this._read(this.br(PC)) shl 8)
          this.br(this.PC) +=1
          
          if((adr shr 8) < ((adr + this.r(Y)) shr 8)) then
            this.cyclesLeft+=1
          end if
             return (adr + this.r(Y)) and &Hffff
       
			 case this._IND
          '// indirect, doesn't loop pages properly
          dim adrl as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          dim adrh as uint32_t = this._read(this.br(this.PC))
			 this.br(this.PC) +=1
          dim radr as uint32_t = this._read(adrl or (adrh shl 8)) 
          radr or= (this._read(((adrl + 1) and &Hff) or (adrh shl 8))) shl 8 
          return radr 




	'dim ptr_lo  As uint16 = this._read(self->pc)
	'this.pc+=1
	'Dim  ptr_hi As uint16 = this._read(self->pc)
	'this.pc+=1

	'dim ptr1 As uint16_t = (ptr_hi shl 8) or ptr_lo

	'if (ptr_lo = &H00FF) Then' Simulate page boundary hardware bug
	'
	'	this.addr_abs = (this._read(ptr1 and &HFF00) Shl 8) Ot this._read(ptr1 + 0)
	'
	'else ' Behave normally
	'
	'	this.addr_abs = (this._read(ptr1 + 1) Shl 8) or this._read(ptr1 + 0)
	'
	'End If

   'return this.addr_abs





		case this._REL
			dim rel as uint32_t = this._read(this.br(this.PC))
			this.br(this.PC) +=1
			return this.getsigned(rel)
			
			
			
	'this.addr_rel = this._read(this.br(this.PC))
	'this.br(this.PC)+=1
	'If (this.addr_rel And &H80) Then
	'	
	'	this.addr_rel Or= &HFF00
	'	
	'EndIf
	'		
	'	return this.addr_rel	
	'		
 End Select
	
	
	
End Function


sub CPU._UNI(adr as uint32_t)
	
	
End Sub
sub CPU._ORA(adr as uint32_t)
	self->r(A) or= self->_read(adr)
	self->setZandN(self->r(A))
	
End Sub
sub CPU._AND(adr as uint32_t)
	self->r(A) and= self->_read(adr)
	self->setZandN(self->r(A))
	
End Sub
sub CPU._EOR(adr as uint32_t)
	self->r(A) xor= self->_read(adr)
	self->setZandN(self->r(A))
End Sub
sub CPU._ADC(adr as uint32_t)
	dim value as uint8_t = (self->_read(adr))
	dim result as uint32_t = self->r(A) + value + iif(self->c,1,0)
	self->c = iif(result > &HFF,true,false) 
		      self->v = iif( _
        (self->r(A) and &H80) = (value and &H80) and _
        (value and &H80) <> (result and &H80), _
         true,false) 
	
	
	self->r(A) = result
	self->setZandN(self->r(A))
End Sub
sub CPU._SBC(adr as uint32_t)
	dim value as uint8_t = (self->_read(adr)) xor &HFF
	dim result as uint32_t = self->r(A) + value + iif(self->c,1,0)
	self->c = iif(result > &HFF,true,false) 
	      self->v = iif( _
        (self->r(A) and &H80) = (value and &H80) and _
        (value and &H80) <> (result and &H80), _
         true,false) 
	
	
	
	self->r(A) = result
	self->setZandN(self->r(A))

	'
	
End Sub
sub CPU._CMP(adr as uint32_t)
	dim value as uint8_t =  self->_read(adr)  xor &HFF
	dim result as uint32_t = self->r(A) + value + 1
   self->c = iif(result > &HFF,true,false) 
	self->setZandN(result and &Hff)
End Sub
sub CPU._CPX(adr as uint32_t)
	dim value as uint8_t = self->_read(adr)  xor &HFF
	dim result as uint32_t = self->r(X) + value + 1 
	self->c = iif(result > &HFF,true,false) 
	self->setZandN(result and &Hff)
End Sub
sub CPU._CPY(adr as uint32_t)
	dim value as uint8_t = (self->_read(adr)) xor &HFF
	dim result as uint32_t = self->r(Y) + value + 1
	self->c = iif(result > &HFF,true,false) 
	self->setZandN(result and &Hff)
End Sub
sub CPU._DEC(adr as uint32_t)
   dim result as uint32_t = (self->_read(adr) - 1) and &HFF
	self->setZandN(result)
	self->_write(adr,result)
End Sub
sub CPU._DEX(adr as uint32_t)
   self->r(X)-=1
	self->setZandN(self->r(X))
End Sub
sub CPU._DEY(adr as uint32_t)
   self->r(Y)-=1
	self->setZandN(self->r(Y))
End Sub
sub CPU._INC(adr as uint32_t)
   
	dim result as uint32_t = (self->_read(adr) + 1) and &HFF
	self->setZandN(result)
   self->_write(adr,result)
End Sub
sub CPU._INX(adr as uint32_t)
   self->r(X)+=1
	self->setZandN(self->r(X))
End Sub
sub CPU._INY(adr as uint32_t)
	
   self->r(Y)+=1
	self->setZandN(self->r(Y))
	
	
End Sub
sub CPU._ASLa(adr as uint32_t)
  
	dim result as uint32_t = self->r(A) shl 1
	self->c = iif(result > &HFF ,true,false)
	self->setZandN(result)
	self->r(A) = result
End Sub
sub CPU._ASL(adr as uint32_t)

 
	dim result as uint32_t = self->_read(adr) shl 1
	self->c = iif(result > &HFF ,true,false)
	self->setZandN(result)
	self->_write(adr,result)
End Sub
sub CPU._ROLA(adr as uint32_t)
	
	dim result as uint32_t = (self->r(A) shl 1) or (iif(self->c,1,0))
	self->c = iif(result > &HFF,true,false)
	self->setZandN(result)
	self->r(A) = result
	
End Sub
sub CPU._ROL(adr as uint32_t)

	dim result as uint32_t = (self->_read(adr) shl 1) or (iif(self->c,1,0))
	self->c = iif(result > &HFF,true,false) 
	self->setZandN(result)
	self->_write(adr,result)
	
End Sub
sub CPU._LSRa(adr as uint32_t)
   dim carry as uint8_t = self->r(A) and &H1
	dim result as uint32_t = self->r(A) shr 1
	self->c = iif(carry > 0 ,true,false)
	self->setZandN(result)
	self->r(A) = result
End Sub
sub CPU._LSR(adr as uint32_t)
	dim value as uint8_t = self->_read(adr)
   dim carry as uint8_t = value and &H1
	dim result as uint32_t = value shr 1
	self->c = iif(carry > 0 ,true,false)
	self->setZandN(result)
	self->_write(adr,result)
End Sub
sub CPU._RORA(adr as uint32_t)
	dim carry as uint8_t = self->r(A) and &H1
	dim result as uint32_t = (self->r(A) shr 1) or (iif(self->c,1,0) shl 7)
	self->c = iif(carry > 0 ,true,false)
	self->setZandN(result)
	self->r(A) = result
	
End Sub
sub CPU._ROR(adr as uint32_t)
	dim value as uint8_t = self->_read(adr)
	dim carry as uint8_t = value and &H1
	dim result as uint32_t = (value shr 1) or (iif(self->c,1,0) shl 7)
	self->c = iif(carry > 0 ,true,false)
	self->setZandN(result)
	self->_write(adr,result)
	
End Sub
sub CPU._LDA(adr as uint32_t)
	
self->r(self->a) = self->_read(adr)
self->setZandN(self->r(self->a))

End Sub
sub CPU._STA(adr as uint32_t)
self->_write(adr,self->r(a))
End Sub
sub CPU._LDX(adr as uint32_t)
	
self->r(x) = self->_read(adr)
self->setZandN(self->r(x))

End Sub
sub CPU._STX(adr as uint32_t)
self->_write(adr,self->r(x))
End Sub
sub CPU._LDY(adr as uint32_t)
	
self->r(y) = self->_read(adr)
self->setZandN(self->r(y))

End Sub
sub CPU._STY(adr as uint32_t)
self->_write(adr,self->r(y))
End Sub
sub CPU._TAX(adr as uint32_t)
	  self->r(x) = self->r(a)
	  self->setZandN(self->r(x))
End Sub
sub CPU._TXA(adr as uint32_t)
	  self->r(A) = self->r(X)
	  self->setZandN(self->r(A))
End Sub
sub CPU._TAY(adr as uint32_t)
	  self->r(y) = self->r(a)
	  self->setZandN(self->r(y))
End Sub
sub CPU._TYA(adr as uint32_t)
	  self->r(a) = self->r(y)
	  self->setZandN(self->r(a))
End Sub
sub CPU._TSX(adr as uint32_t)
	  self->r(x) =self->r(SP)
	  self->setZandN(self->r(x))
End Sub
sub CPU._TXS(adr as uint32_t)
	self->r(SP) = self->r(x)
End Sub
sub CPU._PLA(adr as uint32_t)
    	self->r(SP)+=1
      self->r(a) = self->_read(&H100 + ((self->r(SP)) and &Hff))  
      self->setZandN(self->r(a))  
    
    End Sub
sub CPU._PHA(adr as uint32_t)
	
	self->_write(&H100 + self->r(SP), self->r(a)) 
	self->r(SP)-=1
	
End Sub
sub CPU._PLP(adr as uint32_t)
	self->r(SP)+=1
	self->setP(self->_read(&H100 + ((self->r(SP)) and &Hff))) 
	
End Sub
sub CPU._PHP(adr as uint32_t)
	
	
	self->_write(&H100 + self->r(SP), self->getP(true)) 
	self->r(SP)-=1
End Sub
sub CPU._BPL(adr as uint32_t)
	
	self->doBranch(iif(self->n = 0,true,false), adr)
	
	
End Sub
sub CPU._BMI(adr as uint32_t)
	
	self->doBranch(self->n, adr)
	
	
End Sub
sub CPU._BVC(adr as uint32_t)
	
	self->doBranch(iif(self->v = 0,true,false), adr)
	
	
End Sub
sub CPU._BVS(adr as uint32_t)
	
	self->doBranch(self->v, adr)
	
	
End Sub
sub CPU._BCC(adr as uint32_t)
	
	self->doBranch(iif(self->c = 0,true,false), adr)
	
	
End Sub
sub CPU._BCS(adr as uint32_t)
	
	self->doBranch(self->c, adr)
	
	
End Sub
sub CPU._BNE(adr as uint32_t)
	
	self->doBranch(iif(self->z = 0,true,false), adr)
	
	
End Sub
sub CPU._BEQ(adr as uint32_t)
	
	self->doBranch(self->z, adr)
	
	
End Sub
sub CPU._BRK(adr as uint32_t)
	
	   dim pushPc as uint16_t = (self->br(PC) + 1) and &Hffff 
	  
      self->_write(&H100 + self->r(SP), pushPc shr 8)
      self->r(SP)-=1
      self->_write(&H100 + self->r(SP), pushPc and &Hff) 
      self->r(SP)-=1
      self->_write(&H100 + self->r(SP), self->getP(true)) 
      self->r(SP)-=1
      self->i = true 
      self->br(PC) = self->_read(&Hfffe) or (self->_read(&Hffff) shl 8) 
End Sub
sub CPU._RTI(adr as uint32_t)
self->r(SP)+=1
self->setP(self->_read(&H100 + ((self->r(SP)) and &Hff)))
self->r(SP)+=1
dim pullPC as uint16_t = self->_read(&H100 + ((self->r(SP)) and &Hff)) 
self->r(SP)+=1

pullPc or= (self->_read(&H100 + ((self->r(SP)) and &Hff)) shl 8) 
self->br(PC) = pullPc 

	'self->stkp+=1
	' self->setP(self->_read(&H0100 + self->stkp)) 
	' self->status  And= Not(self->B)
	' self->status  And= Not(self->U)

	'self->stkp+=1
	'self->pc = cast(uint16_t,self->_read(&H0100 + self->stkp))
	'self->stkp+=1
	'self->pc Or= cast(uint16_t,self->_read(&H0100 + self->stkp))  Shl 8 
	'return 0



      'this.r(SP)+=1
      'this.setP(this._read(&H100 + ((this.r(SP)) and &Hff))) 
      'this.r(SP)+=1
      'dim pullPc as uint32_t = this._read(&H100 + ((this.r(SP)) and &Hff)) 
      'this.r(SP)+=1
      'pullPc or= (this._read(&H100 + ((this.r(SP)) and &Hff)) shl 8) 
      'this.br(PC) = pullPc 



End Sub
sub CPU._JSR(adr as uint32_t)
	
      dim pushPC as uint16_t  = (self->br(PC) - 1) and &Hffff 
      self->_write(&H100 + self->r(SP), pushPc shr 8) 
      self->r(SP)-=1
      self->_write(&H100 + self->r(SP), pushPc and &Hff)
      self->r(SP)-=1
      self->br(PC) = adr
End Sub
sub CPU._RTS(adr as uint32_t)
	    
	   self->r(SP)+=1
      dim pullPC as uint16_t = self->_read(&H100 + (( self->r(SP)) and &Hff)) 
      
      self->r(SP)+=1
      pullPc or= ( self->_read(&H100 + (( self->r(SP)) and &Hff)) shl 8)
      
      self->br(PC) = pullPc + 1 
End Sub
sub CPU._JMP(adr as uint32_t)
	
	self->br(PC) = adr
	
End Sub
sub CPU._BIT(adr as uint32_t)
	
	dim value as uint8_t = self->_read(adr)
   self->n = iif((value and &H80) > 0,true,false)
	self->v = iif((value and &H40) > 0,true,false)
	dim _res as uint8_t = self->r(a) and value
	self->z = iif(_res = 0,true,false)
	
End Sub
sub CPU._CLC(adr as uint32_t)
	
	
self->c = false
	
	
	
End Sub
sub CPU._SEC(adr as uint32_t)
	
	
self->c = true
	
	
	
End Sub
sub CPU._CLD(adr as uint32_t)
	
	
self->d = false
	
	
	
End Sub
sub CPU._SED(adr as uint32_t)
	
	
self->d = true
	
	
	
End Sub
sub CPU._CLI(adr as uint32_t)
	
	
self->i = false
	
	
	
End Sub
sub CPU._SEI(adr as uint32_t)
	
	
self->i = true
	
	
	
End Sub

sub CPU._CLV(adr as uint32_t)
	
	
self->v = false
	
	
	
End Sub
sub CPU._NOP(adr as uint32_t)	
'does nothing	
End Sub
sub CPU._IRQ(adr as uint32_t) 
	dim pushPC as uint16_t = self->br(PC)
	
	   self->_write(&H100 + self->r(SP), pushPc shr 8) 
      self->r(SP)-=1
      
      self->_write(&H100 + self->r(SP), pushPc and &Hff) 
      self->r(SP)-=1
       
      self->_write(&H100 + self->r(SP), self->getP(false)) 
      self->r(SP)-=1

      self->i = true
      self->br(PC) = self->_read(&Hfffe) or (self->_read(&Hffff) shl 8) 
	
	
	
	
End Sub 
sub CPU._NMI(adr as uint32_t)
	
		dim pushPC as uint16_t = self->br(PC)
	
	   self->_write(&H100 + self->r(SP), pushPc shr 8) 
      self->r(SP)-=1
      
      self->_write(&H100 + self->r(SP), pushPc and &Hff) 
      self->r(SP)-=1
       
      self->_write(&H100 + self->r(SP), self->getP(false)) 
      self->r(SP)-=1
      
      self->i = true
      self->br(PC) = self->_read(&Hfffa) or (self->_read(&Hfffb) shl 8) 
      
   'self->_write(&H0100 + self->r(SP), (self->br(PC) Shr 8) and &H00FF) 
	'self->r(SP)-=1
	'self->_write(&H0100 + self->r(SP), self->br(PC) And &H00FF) 
	'self->r(SP)-=1
	'
	'self->_I = true
	' 
	'self->_write(&H0100 + self->r(SP), self->getP(false)) 
	'self->r(SP)-=1 

	'self->addr_abs = &HFFFA 
	'dim lo As uint16_t = self->_read(self->addr_abs + 0) 
	'Dim hi As uint16_t = self->_read(self->addr_abs + 1) 
	'self->br(PC) = (hi shl 8) or lo 

	'self->cyclesleft = 8
   '   
      
      
End Sub

'unoffial opcodes
sub CPU._KIL(adr as uint32_t)
	' self->r(PC)-=1
End Sub


sub CPU._SLO(adr as uint32_t)

End Sub

sub CPU._RLA(adr as uint32_t)

End Sub


sub CPU._SRE(adr as uint32_t)

End Sub

sub CPU._RRA(adr as uint32_t)

End Sub

sub CPU._SAX(adr as uint32_t)

End Sub

sub CPU._LAX(adr as uint32_t)

End Sub



sub CPU._DCP(adr as uint32_t)

End Sub

sub CPU._ISC(adr as uint32_t)

End Sub

sub CPU._ANC(adr as uint32_t)

End Sub


sub CPU._ALR(adr as uint32_t)

End Sub

sub CPU._ARR(adr as uint32_t)

End Sub

sub CPU._AXS(adr as uint32_t)

End Sub



Function CPU.hex1(_n As uint32_t,  _d As uint8_t) As  string
	
		 Dim s As String = String(_d, "0")
		 Dim _i As Integer 
		
		'for (int i = d - 1; i >= 0; i--, n >>= 4)
		_i = _d-1
		While _i >= 0
			s[_i] = Asc("0123456789ABCDEF", (_n And &Hf)+1)  '[n And &HF]
				_n shr= 4
			   _i-=1
		Wend
		'	s[i] = "0123456789ABCDEF"[n & &HF];
		'Next
		
		return s
End Function
Function CPU.disassemble(nStart As uint16_t,nStop As uint16_t)   As TMAPUINT16TSTRING ' TMAPUINT16TSTRING 'done until i figure out MAPS in freebasic
	Dim addr1 As uint32_t = nStart
   Dim i As Integer
	
	Dim maplines As  TMAPUINT16TSTRING
	
	
	Dim As uint8_t value,lo,hi
	Dim As uint16_t line_addr 
	Dim sInst As String
	Dim opcode As uint8_t

	 While (addr1  <= Cast(uint32_t,nStop))
	 '	
	 	line_addr = addr1
	 '	
	 '	i +=0
	 	sInst = "$" + this.hex1(addr1,4) + ": "

	 	opcode = bus->_read(addr1,TRUE):addr1+=1
	
 	  	sInst += opnames(opcode) + " "
 
	 	If this.addressingmodes(opcode) = _IMP Then
	 		
	 		sInst += " {IMP}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _IMM Then
	 		
	 		value = bus->_read(addr1):addr1+=1
	 		sInst += "#$" + hex1(value, 2) + " {IMM}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _ZP Then
	 		
	 		lo = bus->_read(addr1):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + " {ZP0}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _ZPX Then
	 		
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + ", X {ZPX}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _ZPY Then
	 		
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + ", Y {ZPY}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _IZX Then
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "($" + hex1(lo, 2) + ", X) {IZX}"
	 		
	 	ElseIf this.addressingmodes(opcode)=  _IZY Then
	 		
	 		 lo = bus->_read(addr1,TRUE):addr1+=1
	 	    hi = 0
	 		 sInst += "($" + hex1(lo, 2) + "), Y {IZY}"
	 		 	
	 	ElseIf this.addressingmodes(opcode) =  _ABS Then 
	 		
	 		 lo = bus->_read(addr1,TRUE):addr1+=1
	 		 hi = bus->_read(addr1,TRUE):addr1+=1
	 		 sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + " {ABS}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _ABX Then
	 		
	 	     lo = bus->_read(addr1,TRUE):addr1+=1
	 		  hi = bus->_read(addr1,TRUE):addr1+=1
	 		  sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", X {ABX}"
	 				
	 	ElseIf this.addressingmodes(opcode) =  _ABY Then
	 		
	 	      lo = bus->_read(addr1,TRUE):addr1+=1
	 	      hi = bus->_read(addr1,TRUE):addr1+=1
	 	      sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", Y {ABY}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _IND Then 
	 		
	 		    lo = bus->_read(addr1,TRUE):addr1+=1
	 		    hi = bus->_read(addr1,TRUE):addr1+=1
	 		    sInst += "($" + hex1(Cast(uint16_t,hi Shl 8) Or lo, 4) + ") {IND}"
	 		
	 	ElseIf this.addressingmodes(opcode) =  _REL Then	
	 		
	 	       value = bus->_read(addr1,TRUE):addr1+=1
	 		    sInst += "$" + hex1(value, 2) + " [$" + hex1(addr1 + cast(int8_t,value), 4) + "] {REL}" 'cast(int8_t,value)
	 		
	 	EndIf
	 'locate ,40
	 'if line_addr = &H8000 then
	 '	color 14
	
	 'else
	 '	 	color 255
	
	 '	
	 'EndIf
	
	 'if 	line_addr = &H7FE6 then
	 '	 locate ,40
	 'print
	 ' locate ,40
	 'print sInst	
	 'end if
	 '
	 'if 	line_addr > &H7FE6 and sInst <> "" and line_addr <= &H801B then

	 'print sInst	
	 'end if
'	 if line_addr = &H3FF0 then 
'	print sInst
'EndIf


	maplines.insert(line_addr,sInst)
	
	
	 Wend
	 
	' sleep
	 
	this.map_InOrder(maplines.proot) 
	 
	 
	 

	Return maplines
'	
End Function
Sub CPU.map_InOrder (pRoot As MAPNODEUINT16TSTRING Ptr)  
			
			If pRoot <> 0 Then
				
				 map_inOrder(pRoot->pLeft)
				
			'	mapvec1.push_back(*proot->ndata)
				
				 map_inOrder(pRoot->pRight)
				     
				
			Endif   
			
End Sub

 
Function CPU.complete() As BOOL ' finished
 	
	'Return iif(this.cyclesleft = 0,true,false)
		Return iif(this.cyclesleft = 0 andalso this.cyclesLeft =  0 andalso this.Bus->indma = false andalso (this.Bus->cycles mod 3)= 0,true,false)
 	
End Function
 




Function CPU.Getflag(f As FLAGS6502) as uint8_t ' finished
	
	'Return IIf((( this.status  And f) > 0),1,0)
	
	
	
End Function


Sub CPU.irq()
	dim pushPC as uint16_t = self->br(PC)
	
	   self->_write(&H100 + self->r(SP), pushPc shr 8) 
      self->r(SP)-=1
      
      self->_write(&H100 + self->r(SP), pushPc and &Hff) 
      self->r(SP)-=1
       
      self->_write(&H100 + self->r(SP), self->getP(false)) 
      self->r(SP)-=1

      self->i = true
      self->br(PC) = self->_read(&Hfffe) or (self->_read(&Hffff) shl 8) 
	
End Sub

Sub CPU.nmi()
		dim pushPC as uint16_t = self->br(PC)
	
	   self->_write(&H100 + self->r(SP), pushPc shr 8) 
      self->r(SP)-=1
      
      self->_write(&H100 + self->r(SP), pushPc and &Hff) 
      self->r(SP)-=1
       
      self->_write(&H100 + self->r(SP), self->getP(false)) 
      self->r(SP)-=1
      
      self->i = true
      self->br(PC) = self->_read(&Hfffa) or (self->_read(&Hfffb) shl 8) 

	
End Sub





