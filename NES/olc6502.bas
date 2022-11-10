#pragma once

#include "crt.bi"
#include "nes\olc6502.bi"
'#include "Bus.bi"
'#include "NSF.bi"





'print "in olc6502.bas"


constructor olc6502()
self = @this 'to create an instance

End Constructor
Sub olc6502._clock()' finished
	
	If (this.cycles = 0) Then
		
		this.opcode = this._read(this.pc)
		
		this.SetFlag(this.U,true)
		
		this.pc += 1
		
		this.cycles = this.lookup(this.opcode).cycles
	
		Dim additional_cycle1 As uint8_t = this.lookup(this.opcode).addrmode() 
		Dim additional_cycle2 As uint8_t =  this.lookup(this.opcode).operate() 
		
		
		
		this.cycles += (additional_cycle1 And additional_cycle2)
		
		this.SetFlag(this.U, true)
		
	EndIf
	
	this.clock_count +=1
	
	this.cycles -= 1 
	'print this.cycles 
	
	
	
End Sub
sub olc6502.ConnectBus(n as any ptr)
	
  this.bus = n
	'bus = n
End Sub

'sub olc6502.ConnectBus(n as _NSF ptr)
'	
'  this.bus = n
'
'End Sub


sub olc6502._write( a as uint16_t,d as uint8_t)
	
	
'this.bus->_write(a,d)

bus->_write(a,d)
	
End Sub
function olc6502._read( a as uint16_t) as uint8_t
	
	
return bus->_read(a, false)
	
End function 

Sub olc6502._reset() ' finished
	
	this.addr_abs = &HFFFC
	dim lo As uint16_t = this._read(addr_abs + 0)
	Dim hi As uint16_t = this._read(addr_abs + 1)
	
	this.pc = (hi shl 8) Or lo


	this.a = 0
	this.x = 0
	this.y = 0
	this.stkp = &HFD
	this.status  = &H00 Or U
	this.status  = &H00 Or I


   
		'this.SetFlag(this.U, true)
		'this.SetFlag(this.I, true)

	
	
	this.addr_rel = &H0000
	this.addr_abs = &H0000
	this.fetched = &H00


	this.cycles = 8-1
	
	
	
End Sub

Sub olc6502.SetFlag(f As FLAGS6502,v As BOOL) ' finished
	
	If (v) Then
		
		 this.status  Or= f
	Else
		 this.status  And= Not f
		
	EndIf
	
End Sub
Function olc6502.Getflag(f As FLAGS6502) as uint8_t ' finished
	
	Return IIf((( this.status  And f) > 0),1,0)
	
	
	
End Function

Function olc6502.ad_IMP() As uint8_t ' finished
'
	self->fetched = self->a
	return 0
'
End Function 
'
Function olc6502.ad_IMM() As uint8_t ' finished
'	
'	
'	
	self->addr_abs = self->pc:self->pc+=1
'	
'	
'	
	Return 	0
'	
End Function
'



Sub olc6502.irq()
	'  If (this.getflag(I)) = 0  Then
		
		'	// Push the program counter to the stack. It's 16-bits dont
	'	// forget so that takes two pushes
		this._write(&H0100 + this.stkp, (this.pc shr 8) And &H00FF) 
		this.stkp-=1 
		this._write(&H0100 + this.stkp, this.pc And &H00FF) 
		this.stkp-=1

		'// Then Push the cpu_cpustatus register to the stack
		this.SetFlag(this.B, false)
		this.SetFlag(this.U, true)
		this.SetFlag(this.I, true)
		this._write(&H0100 + this.stkp, this.status)
		stkp-=1 

		'// Read new program counter location from fixed address
		this.addr_abs = &HFFFE
		dim lo As uint16_t = this._read(this.addr_abs + 0) 
		Dim hi As uint16_t = this._read(this.addr_abs + 1) 
		this.pc = (hi Shl 8) Or lo 

		'// IRQs take time
		this.cycles = 7 
		
		
 ' EndIf

	
End Sub

Sub olc6502.nmi()
	this._write(&H0100 + this.stkp, (this.pc Shr 8) and &H00FF) 
	this.stkp-=1 
	this._write(&H0100 + this.stkp, this.pc And &H00FF) 
	this.stkp-=1 

	this.SetFlag(this.B, 0) 
	this.SetFlag(this.U, 1) 
	this.SetFlag(this.I, 1) 
	this._write(&H0100 + this.stkp, this.status) 
	this.stkp-=1 

	this.addr_abs = &HFFFA 
	dim lo As uint16_t = this._read(this.addr_abs + 0) 
	Dim hi As uint16_t = this._read(this.addr_abs + 1) 
	this.pc = (hi shl 8) or lo 

	this.cycles = 8

	
End Sub


'
'
''added all//////////////////////////////////

Function olc6502.ad_ZPO() As uint8_t ' finished
'	
   
   
	self->addr_abs = self->_read(self->pc)
	self->pc+=1
   self->addr_abs And= &H00FF
	Return 	0
'
End Function



Function olc6502.ad_ZPX() As uint8_t ' finished
	'dim self as olc6502
	
'	
'
 	self->addr_abs = (self->_read(self->pc) + self->x)
	self->pc+=1
   self->addr_abs And= &H00FF
	Return 	0
'	
'	
End Function
Function olc6502.ad_ZPY() As uint8_t ' finished
'	
	self->addr_abs = (self->_read(self->pc) + self->y)
	self->pc+=1
   self->addr_abs And= &H00FF
	Return 	0
'	
End Function
Function olc6502.ad_REL() As uint8_t ' finished
'	
	self->addr_rel = self->_read(self->pc)
	self->pc+=1
	If (self->addr_rel And &H80) Then
		
		self->addr_rel Or= &HFF00
		
	EndIf
Return 0
End Function

Function olc6502.ad_IND() As uint8_t ' finished
'
'
	dim ptr_lo  As uint16 = self->_read(self->pc)
	self->pc+=1
	Dim  ptr_hi As uint16 = self->_read(self->pc)
	self->pc+=1

	dim ptr1 As uint16_t = (ptr_hi shl 8) or ptr_lo

	if (ptr_lo = &H00FF) Then' Simulate page boundary hardware bug
	
		self->addr_abs = (self->_read(ptr1 and &HFF00) Shl 8) Or self->_read(ptr1 + 0)
	
	else ' Behave normally
	
		self->addr_abs = (self->_read(ptr1 + 1) Shl 8) or self->_read(ptr1 + 0)
	
	End If
'	
	return 0
'
'
End Function

Function olc6502.ad_IZX() As uint8_t ' finished
'	
	Dim t As uint16_t = self->_read(self->pc)
	self->pc+=1

	Dim lo As uint16_t = self->_read(cast(uint16_t,t + Cast(uint16_t,self->x)) And &H00FF)
	dim hi  As  uint16_t = self->_read(cast(uint16_t,t + Cast(uint16_t,self->x+ 1)) And &H00FF)

	self->addr_abs = (hi shl 8) or lo
'
	return 0
'	
'
End Function
Function olc6502.ad_IZY() As uint8_t ' finished
'	
	dim t As uint16_t = self->_read(self->pc)
	self->pc+=1

	dim lo As uint16_t = self->_read(t And &H00FF)
	dim hi As uint16_t = self->_read((t + 1) and &H00FF)

	self->addr_abs = (hi shl 8) Or lo
	self->addr_abs += self->y

	if ((self->addr_abs and &HFF00) <> (hi Shl 8)) Then
		return 1
	else
		return 0
	End If
'	
End Function
Function olc6502.ad_ABS() As uint8_t ' finished


	Dim lo As uint16_t = self->_read(self->pc)
	self->pc+=1
	Dim hi As uint16_t  = self->_read(self->pc)
	self->pc+=1

	self->addr_abs = (hi Shl 8) or lo
'
	return 0
'
'	
End Function
Function olc6502.ad_ABX() As uint8_t ' finished
	Dim lo As uint16_t = self->_read(self->pc)
	self->pc+=1
	Dim hi As uint16_t  = self->_read(self->pc)
	self->pc+=1

	self->addr_abs = (hi shl 8) Or lo
	self->addr_abs += self->x

	if ((self->addr_abs and &HFF00) <> (hi Shl 8)) Then
		return 1
	else
	return 0
   End if	
'
End Function
Function olc6502.ad_ABY() As uint8_t ' finished
	Dim lo As uint16_t = self->_read(self->pc)
	self->pc+=1
	Dim hi As uint16_t  = self->_read(self->pc)
	self->pc+=1
'
	self->addr_abs = (hi shl 8) Or lo
	self->addr_abs += self->y
'
	if ((self->addr_abs and &HFF00) <> (hi Shl 8)) Then
		return 1
	else
		return 0
	End if	
'
'
End Function
''////////////////////////////////////////////
'
Function olc6502.complete() As BOOL ' finished
'	
	Return iif(this.cycles = 0,true,false)
'	 
'	
End Function
Function olc6502.fetch() As uint8_t' finished

	If (this.lookup(opcode).addrmode = @ad_IMP) = 0 Then
		this.fetched = this._read(this.addr_abs)
		
	End If
	Return this.fetched 
End Function

'
Function olc6502.op_ADC() As uint8_t' finished
	
	    self->fetch()
	    self->temp = Cast(uint16_t,self->a) + Cast(uint16_t,self->fetched) + Cast(uint16_t,self->getflag(c))
	    
	    self->setflag(self->c,self->temp > 255)
	    self->SetFlag(self->Z, (self->temp and &H00FF) = 0)
	    self->SetFlag(self->V, (not(Cast(uint16_t,self->a) Xor Cast(uint16_t,self->fetched)) and (cast(uint16_t,self->a) xor Cast(uint16_t,self->temp))) and &H0080)

	    'The negative flag is set to the most significant bit of the result
	    self->SetFlag(self->N, self->temp And &H80)

	    'Load the result into the accumulator (it's 8-bit dont forget!)
	    self->a = self->temp and &H00FF

	    'This instruction has the potential to require an additional clock cycle
	    return 1
End Function
Function olc6502.op_SBC() As uint8_t' finished
'	
	self->fetch()
	
	Dim value As uint16_t = Cast(uint16,self->fetched) Xor &H00FF
	self->temp = Cast(uint16_t,self->a) + value + Cast(uint16_t,self->getflag(self->c))
	self->SetFlag(self->C, self->temp and &HFF00)
	self->SetFlag(self->Z, ((self->temp and &H00FF) = 0))
	self->SetFlag(self->V, (self->temp xor cast(uint16_t,self->a)) and (self->temp xor value) and &H0080) 
	self->SetFlag(self->N, self->temp And &H0080)
	self->a = self->temp And &H00FF 
'	
	Return 1
'	
End Function
Function olc6502.op_AND() As uint8_t' finished
'	
	self->fetch()
	self->a = self->a And self->fetched
	self->SetFlag(self->Z, self->a = &H00)
	self->SetFlag(self->N, self->a and &H80)
	Return 1
'		
End Function
Function olc6502.op_ASL() As uint8_t' finished
'	
	self->fetch()
	self->temp = cast(uint16_t,self->fetched shl 1)
	self->SetFlag(self->C, (self->temp and &HFF00) > 0)
	self->SetFlag(self->Z, (self->temp And &H00FF) = &H00)
	self->SetFlag(self->N, self->temp And &H80)
	if (self->lookup(self->opcode).addrmode = @ad_IMP) Then
		self->a = self->temp And &H00FF
	else
		self->_write(self->addr_abs, self->temp And &H00FF)
	End if
	return 0
'
'		
End Function
Function olc6502.op_BCC() As uint8_t' finished
'
	if (self->GetFlag(C) = 0) Then
	 
		self->cycles+=1
		self->addr_abs = self->pc + self->addr_rel

		if((self->addr_abs and &HFF00) <> (self->pc And &HFF00)) Then
			self->cycles+=1
		End If

		self->pc = self->addr_abs
	End If
	return 0
'	
End Function
Function olc6502.op_BCS() As uint8_t' finished
		if (self->GetFlag(C) = 1) Then
	
		self->cycles+= 1
		self->addr_abs = self->pc + self->addr_rel

		if ((self->addr_abs And &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1
		End if
		self->pc = self->addr_abs
		End If

	Return 0
'	
End Function
Function olc6502.op_BEQ() As uint8_t' finished
	If (self->GetFlag(self->Z) = 1) Then
	
		self->cycles+=1 
		self->addr_abs = self->pc + self->addr_rel 

		if ((self->addr_abs and &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1  
		End If

		self->pc = self->addr_abs 
	End if
	
	Return 0
	
	
End Function
Function olc6502.op_BIT() As uint8_t' finished
'	
	self->fetch()
	self->temp = self->a And self->fetched
	self->SetFlag(self->Z, (self->temp and &H00FF) =  &H00)
	self->SetFlag(self->N, self->fetched And (1 Shl 7))
	self->SetFlag(self->V, self->fetched And (1 Shl 6))
	return 0
'	
'	
End Function
Function olc6502.op_BMI() As uint8_t' finished
'	
'	
		if (self->GetFlag(self->N) =  1) Then
	 
		self->cycles+=1 
		self->addr_abs = self->pc + self->addr_rel 

		if ((self->addr_abs And &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1
		End If

		self->pc = self->addr_abs
	End If
	return 0
End Function
Function olc6502.op_BNE() As uint8_t' finished
 
		if (self->GetFlag(self->Z) = 0) Then
			
	 
			self->cycles+=1 
			self->addr_abs = self->pc + self->addr_rel 

			
	   if ((self->addr_abs And &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1
	   End If
	
		self->pc = self->addr_abs
		End If
'		
	return 0
'	
End Function
Function olc6502.op_BPL() As uint8_t' finished
		if (self->GetFlag(self->N) = 0) Then
	 
		self->cycles+=1
		self->addr_abs = self->pc + self->addr_rel 

		if ((self->addr_abs and &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1 
	   End If
		self->pc = self->addr_abs 
		End If
	return 0 
'		
End Function
Function olc6502.op_BRK() As uint8_t' finished
'
		self->pc+=1

	self->SetFlag(self->I, True)
	self->_write(&H0100 + self->stkp, (self->pc shr 8) and &H0FF)
	self->stkp-=1
	self->_write(&H0100 + self->stkp, self->pc and &H00FF)
	self->stkp-=1

	self->SetFlag(self->B, True)
	self->_write(&H0100 + self->stkp, self->status) 
	self->stkp-=1
	self->SetFlag(self->B, False) 
'
   self->pc = Cast(uint16,self->_Read(&HFFFE)) Or Cast(uint16_t,self->_read(&HFFFF) Shl 8)
	return 0
'
'	
End Function
Function olc6502.op_CLC() As uint8_t' finished
'
'	
	self->SetFlag(self->C,FALSE)
	Return 	0
'	
End Function
Function olc6502.op_CLD() As uint8_t' finished
'
	self->SetFlag(self->D,FALSE)
	Return 	0
'	
End Function
Function olc6502.op_CLI() As uint8_t' finished
	
	self->SetFlag(self->I,FALSE)
	Return 	0
'	
End Function
Function olc6502.op_CLV() As uint8_t' finished
   self->SetFlag(self->V,FALSE)
'	
	Return 	0
'	
End Function
Function olc6502.op_SEC() As uint8_t' finished
'	

	self->SetFlag(self->C,TRUE)
	Return 	0
'	
End Function
Function olc6502.op_SED() As uint8_t' finished
'	
'	

	self->SetFlag(self->D,TRUE)
	Return 	0
'	
End Function
Function olc6502.op_SEI() As uint8_t' finished
'	

	self->SetFlag(self->I,TRUE)
	Return 0
'	
End Function
Function olc6502.op_XXX() As uint8_t' finished
'
	Return 	0
'	
End Function
Function olc6502.op_TXS() As uint8_t' finished
'	
	self->stkp = self->x
	Return 	0
'	
End Function
Function olc6502.op_TYA() As uint8_t' finished
	self->a = self->y
	self->SetFlag(self->Z,self->a = &H00)
	self->SetFlag(self->N,self->a and &H80)
	Return 	0
'	
End Function
Function olc6502.op_NOP() As uint8_t' finished
'
	Select Case  (self->opcode)  
		case &H1C
		case &H3C
		case &H5C
		case &H7C
		case &HDC
		case &HFC
		return 1 
	End Select
	return 0
End Function
Function olc6502.op_TXA() As uint8_t' finished
	self->a = self->x
	self->SetFlag(self->Z, self->a = &H00)
	self->SetFlag(self->N, self->a And &H80)
	return 0
'	
End Function
Function olc6502.op_TSX() As uint8_t' finished
	self->x = self->stkp
	self->SetFlag(self->Z, self->x = &H00)
	self->SetFlag(self->N, self->x And &H80)
	return 0
End Function
Function olc6502.op_TAY() As uint8_t' finished
'	
	self->y = self->a
	self->SetFlag(self->Z, self->y = &H00)
	self->SetFlag(self->N, self->y And &H80)
	return 0
'	
End Function
Function olc6502.op_TAX() As uint8_t' finished
   self->x = self->a 
	self->SetFlag(self->Z, self->x =  &H00) 
	self->SetFlag(self->N, self->x And &H80) 
	Return 0 
End Function
Function olc6502.op_STX() As uint8_t' finished
'
'	
	self->_write(self->addr_abs, self->x) 
	return 0
'	
End Function
Function olc6502.op_STY() As uint8_t' finished
'
	self->_write(self->addr_abs, self->y) 
	return 0 
'	
End Function
Function olc6502.op_STA() As uint8_t' finished
' 
	self->_write(self->addr_abs, self->a) 
	return 0
'	
End Function
Function olc6502.op_PLP() As uint8_t' finished
'	
	self->stkp+=1 
	 self->status  = self->_read(&H0100 + self->stkp) 
	self->SetFlag(self->U, 1) 
	return 0 
'	
End Function
Function olc6502.op_RTI() As uint8_t' finished
'	 
	self->stkp+=1
	 self->status  = self->_read(&H0100 + self->stkp) 
	 self->status  And= Not(self->B)
	 self->status  And= Not(self->U)

	self->stkp+=1
	self->pc = cast(uint16_t,self->_read(&H0100 + self->stkp))
	self->stkp+=1
	self->pc Or= cast(uint16_t,self->_read(&H0100 + self->stkp))  Shl 8 
	return 0
'	
End Function
Function olc6502.op_RTS() As uint8_t' finished
	 
	self->stkp+=1
	self->pc = cast(uint16_t,self->_read(&H0100 + self->stkp))
	self->stkp+=1
	self->pc Or= cast(uint16_t,self->_read(&H0100 + self->stkp))  Shl 8

	self->pc+=1
	return 0 
'	
End Function
Function olc6502.op_ROL() As uint8_t' finished
'
	self->fetch()
	self->temp = Cast(uint16_t,self->fetched  Shl 1) Or self->GetFlag(self->C)
	self->SetFlag(self->C, self->temp And &HFF00)
	self->SetFlag(self->Z, (self->temp and &H00FF)  = &H0000)
	self->SetFlag(self->N, self->temp And &H0080)
	if (self->lookup(self->opcode).addrmode = @ad_IMP) then
		self->a = self->temp And &H00FF
	else
		self->_write(self->addr_abs, self->temp And &H00FF)
	End If
	return 0
End Function
Function olc6502.op_ROR() As uint8_t' finished
'	
	self->fetch()
	self->temp = cast(uint16_t,self->GetFlag(self->C) shl 7) or (self->fetched Shr 1)
	self->SetFlag(self->C, self->fetched and &H01)
	self->SetFlag(self->Z, (self->temp And &H00FF) = &H00)
	self->SetFlag(self->N, self->temp And &H0080)
	
	if (self->lookup(self->opcode).addrmode = @ad_IMP) Then
		self->a = self->temp And &H00FF
	else
		self->_write(self->addr_abs, self->temp and &H00FF)
	End if
	return 0
'	
End Function
Function olc6502.op_EOR() As uint8_t' finished
	
	self->fetch()
	self->a = self->a xor self->fetched 
	self->SetFlag(self->Z, self->a =  &H00) 
	self->SetFlag(self->N, self->a And &H80) 
	return 1  
End Function
Function olc6502.op_INX() As uint8_t' finished
	
	self->x+=1
	self->SetFlag(self->Z, self->x =  &H00)
	self->SetFlag(self->N, self->x and &H80)
	return 0 
'	
End Function
Function olc6502.op_INY() As uint8_t' finished
'	
'	
	self->y+=1
	self->SetFlag(self->Z, self->y = &H00)
	self->SetFlag(self->N, self->y and &H80)
	return 0
'	
End Function
Function olc6502.op_DEX() As uint8_t' finished
'	
	self->x-=1
	self->SetFlag(self->Z, self->x = &H00)
	self->SetFlag(self->N, self->x And &H80)
	return 0
'	
End Function
Function olc6502.op_DEY() As uint8_t' finished
'
	self->y-=1
	self->SetFlag(self->Z, self->y = &H00)
	self->SetFlag(N, self->y And &H80)
	return 0
'	
End Function
Function olc6502.op_DEC() As uint8_t' finished
'	
	self->fetch() 
	self->temp = self->fetched - 1 
	self->_write(self->addr_abs, self->temp and &H00FF) 
	self->SetFlag(self->Z, (self->temp and &H00FF) =  &H0000) 
	self->SetFlag(self->N, self->temp and &H0080) 
	return 0 
'
End Function
Function olc6502.op_INC() As uint8_t' finished
'
	self->fetch()
	self->temp = self->fetched + 1
	self->_write(self->addr_abs, self->temp and &H00FF)
	self->SetFlag(self->Z, (self->temp And &H00FF) = &H0000)
	self->SetFlag(self->N, self->temp And &H0080)
	return 0
'
End Function
Function olc6502.op_JMP() As uint8_t' finished
'	
'	
	self->pc = self->addr_abs
	return 0
'	
End Function
Function olc6502.op_JSR() As uint8_t' finished
	self->pc-=1

	self->_write(&H0100 + self->stkp, (self->pc Shr 8) And &H00FF)
	self->stkp-=1
	self->_write(&H100 + self->stkp, self->pc And &H00FF)
	self->stkp-=1

	self->pc = self->addr_abs
	return 0
End Function
Function olc6502.op_BVC() As uint8_t' finished
'	 
		if (self->GetFlag(self->V) = 0) Then
	 
		self->cycles+=1 
		self->addr_abs = self->pc + self->addr_rel 

		if ((self->addr_abs and &HFF00) <> (self->pc and &HFF00)) Then
			self->cycles+=1 
		End If

		self->pc = self->addr_abs 
	End If
	return 0 
'	
End Function
Function olc6502.op_BVS() As uint8_t' finished
'	
	If (self->GetFlag(self->V)  = 1) Then
	 
		self->cycles+=1
		self->addr_abs = self->pc + self->addr_rel 

		if ((self->addr_abs And &HFF00) <> (self->pc and &HFF00)) then
			self->cycles+=1
	End If

		self->pc = self->addr_abs 
	End If
	return 0 
End Function
Function olc6502.op_CMP() As uint8_t' finished
'	 
	self->fetch()
	self->temp = cast(uint16_t,self->a) - Cast(uint16_t,self->fetched)
	self->SetFlag(self->C, self->a >= self->fetched)
	self->SetFlag(self->Z, (self->temp and &H00FF) = &H0000) 
	self->SetFlag(self->N, self->temp And &H0080)
	return 1
'	
End Function
Function olc6502.op_PHA() As uint8_t' finished
	
	self->_write(&H0100 + self->stkp, self->a) 
	self->stkp-=1
	return 0 
'	
End Function
Function olc6502.op_PHP() As uint8_t' finished
'	
	self->_write(&H0100 + self->stkp, self->status or self->B or self->U) 
	self->SetFlag(self->B, 0) 
	self->SetFlag(self->U, 0) 
	self->stkp-=1 
	return 0 
'	
End Function
Function olc6502.op_LDA() As uint8_t' finished
'	
'	
	self->fetch()
	self->a = self->fetched
	self->SetFlag(self->Z, self->a =  &H00)
	self->SetFlag(self->N, self->a And &H80)
	return 1
'	
End Function
Function olc6502.op_PLA() As uint8_t' finished
'	
	self->stkp+=1 
	self->a = self->_read(&H0100 + self->stkp) 
	self->SetFlag(self->Z, self->a =  &H00) 
	self->SetFlag(self->N, self->a and &H80) 
	return 0 
'
'	
End Function
Function olc6502.op_ORA() As uint8_t' finished
	self->fetch() 
	self->a = self->a or self->fetched 
	self->SetFlag(self->Z, self->a =  &H00) 
	self->SetFlag(self->N, self->a And &H80) 
	return 1 
End Function
Function olc6502.op_LSR() As uint8_t' finished
'	
	self->fetch() 
	self->SetFlag(self->C, self->fetched And  &H0001) 
	self->temp = self->fetched Shr  1
	self->SetFlag(self->Z, (self->temp And &H00FF) =  &H0000) 
	self->SetFlag(self->N, self->temp And &H0080) 
	if (self->lookup(self->opcode).addrmode =  @ad_IMP) Then
		self->a = self->temp And &H00FF 
	else
		self->_write(self->addr_abs, self->temp and &H00FF)
	End If 
	return 0 
End Function
Function olc6502.op_LDX() As uint8_t' finished
'	
	self->fetch()  
	self->x = self->fetched 
	self->SetFlag(self->Z, self->x =  &H00) 
	self->SetFlag(self->N, self->x And &H80) 
	return 1 
'
'	
End Function
Function olc6502.op_LDY() As uint8_t' finished
	
	self->fetch() 
	self->y = self->fetched 
	self->SetFlag(self->Z, self->y =  &H00) 
	self->SetFlag(self->N, self->y and &H80) 
	
return 1 
'	
End Function
Function olc6502.op_CPX() As uint8_t' finished
'	
	self->fetch()
	self->temp = Cast(uint16_t,self->x) - cast(uint16_t,self->fetched)
	self->SetFlag(self->C, self->x >= self->fetched)
	self->SetFlag(self->Z, (self->temp and &H00FF)  = &H0000)
	self->SetFlag(self->N, self->temp And &H0080)
	return 0
'	
End Function
Function olc6502.op_CPY() As uint8_t' finished
'	
	self->fetch()
	self->temp = cast(uint16_t,self->y )- cast(uint16_t,self->fetched)
	self->SetFlag(self->C, self->y >= self->fetched)
	self->SetFlag(self->Z, (self->temp And &H00FF) =  &H0000)
	self->SetFlag(self->N, self->temp And &H0080)
	return 0
'	
End Function

	Function olc6502.hex1 (n As uint32_t,  d As uint8_t) As  string
	
		 Dim s As String = String(d, "0")
		 Dim i As Integer 
		
		'for (int i = d - 1; i >= 0; i--, n >>= 4)
		i = d-1
		While i >= 0
			s[i] = Asc("0123456789ABCDEF", (n And &Hf)+1)  '[n And &HF]
				n shr= 4
			   i-=1
		Wend
		'	s[i] = "0123456789ABCDEF"[n & &HF];
		'Next
		
		return s
	End Function

Function olc6502.disassemble(nStart As uint16_t,nStop As uint16_t)   As TMAPUINT16TSTRING ' TMAPUINT16TSTRING 'done until i figure out MAPS in freebasic
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
	
	 	sInst += this.lookup(opcode).name + " "
	 	
	 	
	 	If lookup(opcode).addrmode = @ad_IMP Then
	 		
	 		sInst += " {IMP}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_IMM Then
	 		
	 		value = bus->_read(addr1,TRUE):addr1+=1
	 		sInst += "#$" + hex1(value, 2) + " {IMM}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_ZPO Then
	 		
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + " {ZP0}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_ZPX Then
	 		
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + ", X {ZPX}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_ZPY Then
	 		
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "$" + hex1(lo, 2) + ", Y {ZPY}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_IZX Then
	 		lo = bus->_read(addr1,TRUE):addr1+=1
	 		hi = 0
	 		sInst += "($" + hex1(lo, 2) + ", X) {IZX}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_IZY Then
	 		
	 		 lo = bus->_read(addr1,TRUE):addr1+=1
	 	    hi = 0
	 		 sInst += "($" + hex1(lo, 2) + "), Y {IZY}"
	 		 	
	 	ElseIf lookup(opcode).addrmode = @ad_ABS Then 
	 		
	 		 lo = bus->_read(addr1,TRUE):addr1+=1
	 		 hi = bus->_read(addr1,TRUE):addr1+=1
	 		 sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + " {ABS}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_ABX Then
	 		
	 	    lo = bus->_read(addr1,TRUE):addr1+=1
	 		  hi = bus->_read(addr1,TRUE):addr1+=1
	 		  sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", X {ABX}"
	 				
	 	ElseIf lookup(opcode).addrmode = @ad_ABY Then
	 		
	 	      lo = bus->_read(addr1,TRUE):addr1+=1
	 	     hi = bus->_read(addr1,TRUE):addr1+=1
	 	      sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", Y {ABY}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_IND Then 
	 		
	 		    lo = bus->_read(addr1,TRUE):addr1+=1
	 		   hi = bus->_read(addr1,TRUE):addr1+=1
	 		    sInst += "($" + hex1(Cast(uint16_t,hi Shl 8) Or lo, 4) + ") {IND}"
	 		
	 	ElseIf lookup(opcode).addrmode = @ad_REL Then	
	 		
	 	       value = bus->_read(addr1,TRUE):addr1+=1
	 		    sInst += "$" + hex1(value, 2) + " [$" + hex1(addr1 + value, 4) + "] {REL}" 'cast(int8_t,value)
	 		
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
	
End Function

Sub olc6502.map_InOrder (pRoot As MAPNODEUINT16TSTRING Ptr)  
			
			If pRoot <> 0 Then
				
				 map_inOrder(pRoot->pLeft)
				
			'	mapvec1.push_back(*proot->ndata)
				
				 map_inOrder(pRoot->pRight)
				     
				
			Endif   
			
End Sub

