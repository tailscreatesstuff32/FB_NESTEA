
#pragma once


#Include Once "windows.bi"
#Include Once "containers/vector.bi"
#Include Once "containers/map.bi"
#Include Once "containers/list.bi"
#include Once "file.bi"
#include once "crt.bi"

'print "in olc6502.bi"


Type UINT16T As uint16_t

MMapTemplate(UINT16T ,String)

Type INSTRUCTION
	Name As String * 3
   operate As  Function() As uint8_t '= NULL
	addrmode  As  Function() As uint8_t' = NULL
	cycles As uint8_t '= 0
	
End Type	



'type as NSFplayer _Bus


type olc6502
	
	     r(4) as uint8_t
     br(1) as uint32_t
	  const SP = 3 
	nmiwanted as boolean

	public:
	
	declare constructor()
	
	
	public:
a As uint8_t 
x As uint8_t
y As uint8_t
stkp As uint8_t
pc As uint16_t

status As uint8_t

Declare  Sub _reset()
Declare  Sub irq()
Declare  Sub nmi()
Declare  Sub _clock()

Declare  Function complete As bool
	
	
	
	
	Bus as _Bus ptr
	'NSF as _NSF ptr
	
	declare sub ConnectBus(n as any ptr)
	'declare sub ConnectBus(n as _NSF ptr)
Enum FLAGS6502
	C = (1 Shl 0)
	Z = (1 Shl 1)
	I = (1 Shl 2)
	D = (1 Shl 3)
	B = (1 Shl 4)
	U = (1 Shl 5)
	V = (1 Shl 6)
	N = (1 Shl 7)

End Enum
		
Declare Function Getflag(f As FLAGS6502) As uint8_t
Declare Sub SetFlag(f As FLAGS6502,v As BOOL)
Declare function GetCycles() as uint32_t


private:
	
fetched As uint8_t
temp As uint16_t
addr_abs As uint16_t
addr_rel As uint16_t
opcode As uint8_t
cycles As uint8_t
clock_count As uint32_t
	
	
static self as olc6502 ptr

		declare sub _write( a as uint16_t,d as uint8_t)
		declare function _read( a as uint16_t) as uint8_t
		
	declare Function fetch() As uint8_t
	
	public:
	
	irqwanted as boolean = false
	
	'NES address modes
declare  static Function ad_IMP() As uint8_t:Declare  static Function ad_IMM() As uint8_t
declare  static Function ad_ZPO() As uint8_t:Declare  static Function ad_ZPX() As uint8_t
declare  static Function ad_ZPY() As uint8_t:Declare  static Function ad_REL() As uint8_t
declare  static Function ad_ABS() As uint8_t:Declare  static Function ad_ABX() As uint8_t
declare  static Function ad_ABY() As uint8_t:Declare  static Function ad_IND() As uint8_t
declare  static Function ad_IZX() As uint8_t:Declare  static Function ad_IZY() As uint8_t

	public:
	
	'NES opcodes
declare  static Function op_ADC() As uint8_t: Declare  static Function op_AND() As uint8_t
declare  static Function op_ASL() As uint8_t: Declare  static Function op_BCC() As uint8_t
declare  static Function op_BCS() As uint8_t: Declare  static Function op_BEQ() As uint8_t
declare  static Function op_BIT() As uint8_t: Declare  static Function op_BMI() As uint8_t
declare  static Function op_BNE() As uint8_t: Declare  static Function op_BPL() As uint8_t
declare  static Function op_BRK() As uint8_t: Declare  static Function op_BVC() As uint8_t
declare  static Function op_BVS() As uint8_t: Declare  static Function op_CLC() As uint8_t
declare  static Function op_CLD() As uint8_t: Declare  static Function op_CLI() As uint8_t
declare  static Function op_CLV() As uint8_t: Declare  static Function op_CMP() As uint8_t
declare  static Function op_CPX() As uint8_t: Declare  static Function op_CPY() As uint8_t
declare  static Function op_DEC() As uint8_t: Declare  static Function op_DEX() As uint8_t
declare  static Function op_DEY() As uint8_t: Declare  static Function op_EOR() As uint8_t
declare  static Function op_INC() As uint8_t: Declare  static Function op_INX() As uint8_t
declare  static Function op_INY() As uint8_t: Declare  static Function op_JMP() As uint8_t
declare  static Function op_JSR() As uint8_t: Declare  static Function op_LDA() As uint8_t
declare  static Function op_LDX() As uint8_t: Declare  static Function op_LDY() As uint8_t
declare  static Function op_LSR() As uint8_t: Declare  static Function op_NOP() As uint8_t
declare  static Function op_ORA() As uint8_t: Declare  static Function op_PHA() As uint8_t
declare  static Function op_PHP() As uint8_t: Declare  static Function op_PLA() As uint8_t
declare  static Function op_PLP() As uint8_t: Declare  static Function op_ROL() As uint8_t
declare  static Function op_ROR() As uint8_t: Declare  static Function op_RTI() As uint8_t
declare  static Function op_RTS() As uint8_t: Declare  static Function op_SBC() As uint8_t
declare  static Function op_SEC() As uint8_t: Declare  static Function op_SED() As uint8_t
declare  static Function op_SEI() As uint8_t: Declare  static Function op_STA() As uint8_t
declare  static Function op_STX() As uint8_t: Declare  static Function op_STY() As uint8_t
declare  static Function op_TAX() As uint8_t: Declare  static Function op_TAY() As uint8_t
declare  static Function op_TSX() As uint8_t: Declare  static Function op_TXA() As uint8_t
declare  static Function op_TXS() As uint8_t: Declare  static Function op_TYA() As uint8_t
Declare  static Function op_XXX() As uint8_t

dim As INSTRUCTION lookup(0 To 256-1)  => _ ' recheck
{ _ ''''
("BRK",@op_BRK,@ad_IMM,7),("ORA",@op_ORA,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ORA",@op_ORA,@ad_ZPO,3),("ASL",@op_ASL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHP",@op_PHP,@ad_IMP,3),("ORA",@op_ORA,@ad_IMM,2),("ASL",@op_ASL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABS,4),("ASL",@op_ASL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _                    ''''
("BPL",@op_BPL,@ad_REL,2),("ORA",@op_ORA,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ZPX,4),("ASL",@op_ASL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLC",@op_CLC,@ad_IMP,2),("ORA",@op_ORA,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABX,4),("ASL",@op_ASL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("JSR",@op_JSR,@ad_ABS,6),("AND",@op_AND,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("BIT",@op_BIT,@ad_ZPO,3),("AND",@op_AND,@ad_ZPO,3),("ROL",@op_ROL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLP",@op_PLP,@ad_IMP,4),("AND",@op_AND,@ad_IMM,2),("ROL",@op_ROL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("BIT",@op_BIT,@ad_ABS,4),("AND",@op_AND,@ad_ABS,4),("ROL",@op_ROL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BMI",@op_BMI,@ad_REL,2),("AND",@op_AND,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ZPX,4),("ROL",@op_ROL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEC",@op_SEC,@ad_IMP,2),("AND",@op_AND,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ABX,4),("ROL",@op_ROL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("RTI",@op_RTI,@ad_IMP,6),("EOR",@op_EOR,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("EOR",@op_EOR,@ad_ZPO,3),("LSR",@op_LSR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHA",@op_PHA,@ad_IMP,3),("EOR",@op_EOR,@ad_IMM,2),("LSR",@op_LSR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_ABS,3),("EOR",@op_EOR,@ad_ABS,4),("LSR",@op_LSR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BVC",@op_BVC,@ad_REL,2),("EOR",@op_EOR,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ZPX,4),("LSR",@op_LSR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLI",@op_CLI,@ad_IMP,2),("EOR",@op_EOR,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ABX,4),("LSR",@op_LSR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("RTS",@op_RTS,@ad_IMP,6),("ADC",@op_ADC,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ADC",@op_ADC,@ad_ZPO,3),("ROR",@op_ROR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLA",@op_PLA,@ad_IMP,4),("ADC",@op_ADC,@ad_IMM,2),("ROR",@op_ROR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_IND,5),("ADC",@op_ADC,@ad_ABS,4),("ROR",@op_ROR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BVS",@op_BVS,@ad_REL,2),("ADC",@op_ADC,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ZPX,4),("ROR",@op_ROR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEI",@op_SEI,@ad_IMP,2),("ADC",@op_ADC,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ABX,4),("ROR",@op_ROR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("???",@op_NOP,@ad_IMP,2),("STA",@op_STA,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPO,3),("STA",@op_STA,@ad_ZPO,3),("STX",@op_STX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("DEY",@op_DEY,@ad_IMP,2),("???",@op_NOP,@ad_IMP,2),("TXA",@op_TXA,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("STY",@op_STY,@ad_ABS,4),("STA",@op_STA,@ad_ABS,4),("STX",@op_STX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
("BCC",@op_BCC,@ad_REL,2),("STA",@op_STA,@ad_IZY,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPX,4),("STA",@op_STA,@ad_ZPX,4),("STX",@op_STX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("TYA",@op_TYA,@ad_IMP,2),("STA",@op_STA,@ad_ABY,5),("TXS",@op_TXS,@ad_IMP,2),("???",@op_XXX,@ad_IMP,5),("???",@op_NOP,@ad_IMP,5),("STA",@op_STA,@ad_ABX,5),("???",@op_XXX,@ad_IMP,5),("???",@op_XXX,@ad_IMP,5), _
("LDY",@op_LDY,@ad_IMM,2),("LDA",@op_LDA,@ad_IZX,6),("LDX",@op_LDX,@ad_IMM,2),("???",@op_XXX,@ad_IMP,6),("LDY",@op_LDY,@ad_ZPO,3),("LDA",@op_LDA,@ad_ZPO,3),("LDX",@op_LDX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("TAY",@op_TAY,@ad_IMP,2),("LDA",@op_LDA,@ad_IMM,2),("TAX",@op_TAX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("LDY",@op_LDY,@ad_ABS,4),("LDA",@op_LDA,@ad_ABS,4),("LDX",@op_LDX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
("BCS",@op_BCS,@ad_REL,2),("LDA",@op_LDA,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,5),("LDY",@op_LDY,@ad_ZPX,4),("LDA",@op_LDA,@ad_ZPX,4),("LDX",@op_LDX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("CLV",@op_CLV,@ad_IMP,2),("LDA",@op_LDA,@ad_ABY,4),("TSX",@op_TSX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,4),("LDY",@op_LDY,@ad_ABX,4),("LDA",@op_LDA,@ad_ABX,4),("LDX",@op_LDX,@ad_ABY,4),("???",@op_XXX,@ad_IMP,4), _
("CPY",@op_CPY,@ad_IMM,2),("CMP",@op_CMP,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("CPY",@op_CPY,@ad_ZPO,3),("CMP",@op_CMP,@ad_ZPO,3),("DEC",@op_DEC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INY",@op_INY,@ad_IMP,2),("CMP",@op_CMP,@ad_IMM,2),("DEX",@op_DEX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("CPY",@op_CPY,@ad_ABS,4),("CMP",@op_CMP,@ad_ABS,4),("DEC",@op_DEC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BNE",@op_BNE,@ad_REL,2),("CMP",@op_CMP,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ZPX,4),("DEC",@op_DEC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLD",@op_CLD,@ad_IMP,2),("CMP",@op_CMP,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ABX,4),("DEC",@op_DEC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("CPX",@op_CPX,@ad_IMM,2),("SBC",@op_SBC,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("CPX",@op_CPX,@ad_ZPO,3),("SBC",@op_SBC,@ad_ZPO,3),("INC",@op_INC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INX",@op_INX,@ad_IMP,2),("SBC",@op_SBC,@ad_IMM,2),("NOP",@op_NOP,@ad_IMP,2),("???",@op_SBC,@ad_IMP,2),("CPX",@op_CPX,@ad_ABS,4),("SBC",@op_SBC,@ad_ABS,4),("INC",@op_INC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BEQ",@op_BEQ,@ad_REL,2),("SBC",@op_SBC,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ZPX,4),("INC",@op_INC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SED",@op_SED,@ad_IMP,2),("SBC",@op_SBC,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ABX,4),("INC",@op_INC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7)  _
}


'UNOFFICIAL OPCODE WIP/////////////////////////////////////////
'dim As INSTRUCTION lookup(0 To 256-1)  => _ ' recheck
'{ _ ''''
'("BRK",@op_BRK,@ad_IMM,7),("ORA",@op_ORA,@ad_IZX,6),("KIL",@op_XXX,@ad_IMP,2),("SLO",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ORA",@op_ORA,@ad_ZPO,3),("ASL",@op_ASL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHP",@op_PHP,@ad_IMP,3),("ORA",@op_ORA,@ad_IMM,2),("ASL",@op_ASL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABS,4),("ASL",@op_ASL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _                    ''''
'("BPL",@op_BPL,@ad_REL,2),("ORA",@op_ORA,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("SLO",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ZPX,4),("ASL",@op_ASL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLC",@op_CLC,@ad_IMP,2),("ORA",@op_ORA,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABX,4),("ASL",@op_ASL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
'("JSR",@op_JSR,@ad_ABS,6),("AND",@op_AND,@ad_IZX,6),("KIL",@op_XXX,@ad_IMP,2),("RLA",@op_XXX,@ad_IMP,8),("BIT",@op_BIT,@ad_ZPO,3),("AND",@op_AND,@ad_ZPO,3),("ROL",@op_ROL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLP",@op_PLP,@ad_IMP,4),("AND",@op_AND,@ad_IMM,2),("ROL",@op_ROL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("BIT",@op_BIT,@ad_ABS,4),("AND",@op_AND,@ad_ABS,4),("ROL",@op_ROL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
'("BMI",@op_BMI,@ad_REL,2),("AND",@op_AND,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("RLA",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ZPX,4),("ROL",@op_ROL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEC",@op_SEC,@ad_IMP,2),("AND",@op_AND,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ABX,4),("ROL",@op_ROL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
'("RTI",@op_RTI,@ad_IMP,6),("EOR",@op_EOR,@ad_IZX,6),("KIL",@op_XXX,@ad_IMP,2),("SRE",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("EOR",@op_EOR,@ad_ZPO,3),("LSR",@op_LSR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHA",@op_PHA,@ad_IMP,3),("EOR",@op_EOR,@ad_IMM,2),("LSR",@op_LSR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_ABS,3),("EOR",@op_EOR,@ad_ABS,4),("LSR",@op_LSR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
'("BVC",@op_BVC,@ad_REL,2),("EOR",@op_EOR,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("SRE",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ZPX,4),("LSR",@op_LSR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLI",@op_CLI,@ad_IMP,2),("EOR",@op_EOR,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ABX,4),("LSR",@op_LSR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
'("RTS",@op_RTS,@ad_IMP,6),("ADC",@op_ADC,@ad_IZX,6),("KIL",@op_XXX,@ad_IMP,2),("RRA",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ADC",@op_ADC,@ad_ZPO,3),("ROR",@op_ROR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLA",@op_PLA,@ad_IMP,4),("ADC",@op_ADC,@ad_IMM,2),("ROR",@op_ROR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_IND,5),("ADC",@op_ADC,@ad_ABS,4),("ROR",@op_ROR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
'("BVS",@op_BVS,@ad_REL,2),("ADC",@op_ADC,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("RRA",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ZPX,4),("ROR",@op_ROR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEI",@op_SEI,@ad_IMP,2),("ADC",@op_ADC,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ABX,4),("ROR",@op_ROR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
'("???",@op_NOP,@ad_IMP,2),("STA",@op_STA,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("SAX",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPO,3),("STA",@op_STA,@ad_ZPO,3),("STX",@op_STX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("DEY",@op_DEY,@ad_IMP,2),("???",@op_NOP,@ad_IMP,2),("TXA",@op_TXA,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("STY",@op_STY,@ad_ABS,4),("STA",@op_STA,@ad_ABS,4),("STX",@op_STX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
'("BCC",@op_BCC,@ad_REL,2),("STA",@op_STA,@ad_IZY,6),("KIL",@op_XXX,@ad_IMP,2),("SAX",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPX,4),("STA",@op_STA,@ad_ZPX,4),("STX",@op_STX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("TYA",@op_TYA,@ad_IMP,2),("STA",@op_STA,@ad_ABY,5),("TXS",@op_TXS,@ad_IMP,2),("???",@op_XXX,@ad_IMP,5),("???",@op_NOP,@ad_IMP,5),("STA",@op_STA,@ad_ABX,5),("???",@op_XXX,@ad_IMP,5),("???",@op_XXX,@ad_IMP,5), _
'("LDY",@op_LDY,@ad_IMM,2),("LDA",@op_LDA,@ad_IZX,6),("LDX",@op_LDX,@ad_IMM,2),("LAX",@op_XXX,@ad_IMP,6),("LDY",@op_LDY,@ad_ZPO,3),("LDA",@op_LDA,@ad_ZPO,3),("LDX",@op_LDX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("TAY",@op_TAY,@ad_IMP,2),("LDA",@op_LDA,@ad_IMM,2),("TAX",@op_TAX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("LDY",@op_LDY,@ad_ABS,4),("LDA",@op_LDA,@ad_ABS,4),("LDX",@op_LDX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
'("BCS",@op_BCS,@ad_REL,2),("LDA",@op_LDA,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("LAX",@op_XXX,@ad_IMP,5),("LDY",@op_LDY,@ad_ZPX,4),("LDA",@op_LDA,@ad_ZPX,4),("LDX",@op_LDX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("CLV",@op_CLV,@ad_IMP,2),("LDA",@op_LDA,@ad_ABY,4),("TSX",@op_TSX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,4),("LDY",@op_LDY,@ad_ABX,4),("LDA",@op_LDA,@ad_ABX,4),("LDX",@op_LDX,@ad_ABY,4),("???",@op_XXX,@ad_IMP,4), _
'("CPY",@op_CPY,@ad_IMM,2),("CMP",@op_CMP,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("DCP",@op_XXX,@ad_IMP,8),("CPY",@op_CPY,@ad_ZPO,3),("CMP",@op_CMP,@ad_ZPO,3),("DEC",@op_DEC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INY",@op_INY,@ad_IMP,2),("CMP",@op_CMP,@ad_IMM,2),("DEX",@op_DEX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("CPY",@op_CPY,@ad_ABS,4),("CMP",@op_CMP,@ad_ABS,4),("DEC",@op_DEC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
'("BNE",@op_BNE,@ad_REL,2),("CMP",@op_CMP,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("DCP",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ZPX,4),("DEC",@op_DEC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLD",@op_CLD,@ad_IMP,2),("CMP",@op_CMP,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ABX,4),("DEC",@op_DEC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
'("CPX",@op_CPX,@ad_IMM,2),("SBC",@op_SBC,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("ISC",@op_XXX,@ad_IMP,8),("CPX",@op_CPX,@ad_ZPO,3),("SBC",@op_SBC,@ad_ZPO,3),("INC",@op_INC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INX",@op_INX,@ad_IMP,2),("SBC",@op_SBC,@ad_IMM,2),("NOP",@op_NOP,@ad_IMP,2),("???",@op_SBC,@ad_IMP,2),("CPX",@op_CPX,@ad_ABS,4),("SBC",@op_SBC,@ad_ABS,4),("INC",@op_INC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
'("BEQ",@op_BEQ,@ad_REL,2),("SBC",@op_SBC,@ad_IZY,5),("KIL",@op_XXX,@ad_IMP,2),("ISC",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ZPX,4),("INC",@op_INC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SED",@op_SED,@ad_IMP,2),("SBC",@op_SBC,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ABX,4),("INC",@op_INC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7)  _
'}







Declare Sub map_InOrder (pRoot As MAPNODEUINT16TSTRING Ptr)



declare Function disassemble(nStart As uint16_t,nStop As uint16_t)    As TMAPUINT16TSTRING

Declare Function hex1 (n As uint32_t,  d As uint8_t) As  string

private:
 


	
End Type

dim olc6502.self as olc6502 ptr


'
'dim olc6502.a as uint8_t = 0 
'dim olc6502.x as uint8_t = 0 
'dim olc6502.y as uint8_t = 0 
'dim olc6502.stkp as uint8_t = 0 
'dim olc6502.pc as uint16_t = 0 
'
'dim olc6502.fetched As uint8_t
'dim olc6502.temp As uint16_t
'dim olc6502.addr_abs As uint16_t
'dim olc6502.addr_rel As uint16_t
'dim olc6502.opcode As uint8_t
'dim olc6502.cycles As uint8_t
'dim olc6502.clock_count As uint32_t
