
#pragma once


#Include Once "windows.bi"
#Include Once "containers/vector.bi"
#Include Once "containers/map.bi"
#Include Once "containers/list.bi"
#include Once "file.bi"
#include once "crt.bi"




Type UINT16T As uint16_t









MMapTemplate(UINT16T ,String)


'type as Bus _Bus


type CPU
	
fetched As uint8_t
temp As uint16_t
addr_abs As uint16_t
addr_rel As uint16_t
opcode As uint8_t

clock_count As uint32_t

_temp As uint16_t

	Declare  Sub irq()
Declare  Sub nmi()
		declare Function fetch() As uint8_t

	stkp As uint8_t

status As uint8_t

	Declare  Function complete As bool

	
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

	
	public:
	

	
	declare constructor()
	
  const _IMP = 0
  const _IMM = 1
  const _ZP = 2 
  const _ZPX = 3 
  const _ZPY = 4 
  const _IZX = 5 
  const _IZY = 6 
  const _ABS = 7 
  const _ABX = 8 
  const _ABY = 9 
  const _IND = 11 
  const _REL = 12 
  const _IZYr = 13  '// for read instructions, with  tional extra cycle
  const _ABXr = 14  '// RMW and writes always have the extra cycle
  const _ABYr = 15 

 ' // register indexes in arrays
  const A = 0 
  const X = 1
  const Y = 2 
  const SP = 3 
  const PC = 0 



   '// registers
     r(4) as uint8_t
     br(1) as uint32_t
	
	Declare  Sub _reset()
	
	
		
	
	
   Bus as _Bus ptr
	declare sub ConnectBus(_n as any ptr)

	
	n as bool
	v as bool
	d as bool
	i as bool
	z as bool
	c as bool
	
	irqwanted as boolean
	nmiwanted as boolean
	
	
	cyclesleft as uint32_t
	
addressingmodes(&HFF) as uint8_t=> _
  { _ ' //x0    x1    x2   x3    x4    x5    x6    x7    x8    x9    xa    xb    xc    xd    xe    xf
      _IMP, _IZX, _IMP, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//0x
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX, _ '//1x
      _ABS, _IZX, _IMP, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//2x
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX, _ '//3x
      _IMP, _IZX, _IMP, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//4x
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX, _ '//5x
      _IMP, _IZX, _IMP, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _IND, _ABS, _ABS, _ABS, _ '//6x
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX, _ '//7x
      _IMM, _IZX, _IMM, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//8x
      _REL, _IZY, _IMP, _IZY, _ZPX, _ZPX, _ZPY, _ZPY, _IMP, _ABY, _IMP, _ABY, _ABX, _ABX, _ABY, _ABY, _ '//9x
      _IMM, _IZX, _IMM, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//ax
      _REL, _IZYr,_IMP, _IZYr,_ZPX, _ZPX, _ZPY, _ZPY, _IMP, _ABYr,_IMP, _ABYr,_ABXr,_ABXr,_ABYr,_ABYr, _'//bx
      _IMM, _IZX, _IMM, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//cx
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX, _ '//dx
      _IMM, _IZX, _IMM, _IZX, _ZP , _ZP , _ZP , _ZP , _IMP, _IMM, _IMP, _IMM, _ABS, _ABS, _ABS, _ABS, _ '//ex
      _REL, _IZYr,_IMP, _IZY, _ZPX, _ZPX, _ZPX, _ZPX, _IMP, _ABYr,_IMP, _ABY, _ABXr,_ABXr,_ABX, _ABX _  '//fx
  }
	
	
	cycles(&HFF) as uint8_t => _
	{ _ '//0x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf
	   7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, _ '//0x
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, _ '//1x
      6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, _ '//2x
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, _ '//3x
      6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, _ '//4x
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, _ '//5x
      6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, _ '//6x
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, _ '//7x
      2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, _ '//8x
      2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5, _ '//9x
      2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, _ '//ax
      2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4, _ '//bx
      2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, _ '//cx
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, _ '//dx
      2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, _ '//ex
      2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7  _ '//fx
	}
	
	
	 opNames(&HFF) as string => { _
    "brk", "ora", "kil", "slo", "nop", "ora", "asl", "slo", "php", "ora", "asl", "anc", "nop", "ora", "asl", "slo", _'//0x
    "bpl", "ora", "kil", "slo", "nop", "ora", "asl", "slo", "clc", "ora", "nop", "slo", "nop", "ora", "asl", "slo", _'//1x
    "jsr", "and", "kil", "rla", "bit", "and", "rol", "rla", "plp", "and", "rol", "anc", "bit", "and", "rol", "rla", _'//2x
    "bmi", "and", "kil", "rla", "nop", "and", "rol", "rla", "sec", "and", "nop", "rla", "nop", "and", "rol", "rla", _'//3x
    "rti", "eor", "kil", "sre", "nop", "eor", "lsr", "sre", "pha", "eor", "lsr", "alr", "jmp", "eor", "lsr", "sre", _'//4x
    "bvc", "eor", "kil", "sre", "nop", "eor", "lsr", "sre", "cli", "eor", "nop", "sre", "nop", "eor", "lsr", "sre", _'//5x
    "rts", "adc", "kil", "rra", "nop", "adc", "ror", "rra", "pla", "adc", "ror", "arr", "jmp", "adc", "ror", "rra", _'//6x
    "bvs", "adc", "kil", "rra", "nop", "adc", "ror", "rra", "sei", "adc", "nop", "rra", "nop", "adc", "ror", "rra", _'//7x
    "nop", "sta", "nop", "sax", "sty", "sta", "stx", "sax", "dey", "nop", "txa", "uni", "sty", "sta", "stx", "sax", _'//8x
    "bcc", "sta", "kil", "uni", "sty", "sta", "stx", "sax", "tya", "sta", "txs", "uni", "uni", "sta", "uni", "uni", _'//9x
    "ldy", "lda", "ldx", "lax", "ldy", "lda", "ldx", "lax", "tay", "lda", "tax", "uni", "ldy", "lda", "ldx", "lax", _'//ax
    "bcs", "lda", "kil", "lax", "ldy", "lda", "ldx", "lax", "clv", "lda", "tsx", "uni", "ldy", "lda", "ldx", "lax", _'//bx
    "cpy", "cmp", "nop", "dcp", "cpy", "cmp", "dec", "dcp", "iny", "cmp", "dex", "axs", "cpy", "cmp", "dec", "dcp", _'//cx
    "bne", "cmp", "kil", "dcp", "nop", "cmp", "dec", "dcp", "cld", "cmp", "nop", "dcp", "nop", "cmp", "dec", "dcp", _'//dx
    "cpx", "sbc", "nop", "isc", "cpx", "sbc", "inc", "isc", "inx", "sbc", "nop", "sbc", "cpx", "sbc", "inc", "isc", _'//ex
    "beq", "sbc", "kil", "isc", "nop", "sbc", "inc", "isc", "sed", "sbc", "nop", "isc", "nop", "sbc", "inc", "isc" _'//fx
  }
	
	
	
		Declare  Sub _clock()
		
		
		declare function getP(bFlag as bool) as uint8_t
		
		declare sub setP(value as uint8_t)
		
		
		declare sub setZandN(value as uint8_t)
		

      declare function getsigned(value as uint8_t) as int8_t
	
	declare sub dobranch(test as bool,rel as uint32_t)
	
	declare function getAdr(mode as uint8_t) as uint32_t
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	Declare Sub map_InOrder (pRoot As MAPNODEUINT16TSTRING Ptr)



declare Function disassemble(nStart As uint16_t,nSt  As uint16_t)    As TMAPUINT16TSTRING

Declare Function hex1 (_n As uint32_t,  _d as uint8_t) As  string

	
	
	
	
		declare sub _write( _a as uint16_t,_d as uint8_t)
		declare function _read( _a as uint16_t) as uint8_t
	
	
	
declare static sub _uni(adr as uint32_t)
declare static sub  _ADC(adr as uint32_t) : declare static sub  _AND(adr as uint32_t) 
declare static sub  _ASL(adr as uint32_t) : declare static sub  _BCC(adr as uint32_t) 
declare static sub  _BCS(adr as uint32_t) : declare static sub  _BEQ(adr as uint32_t) 
declare static sub  _BIT(adr as uint32_t) : declare static sub  _BMI(adr as uint32_t) 
declare static sub  _BNE(adr as uint32_t) : declare static sub  _BPL(adr as uint32_t) 
declare static sub  _BRK(adr as uint32_t) : declare static sub  _BVC(adr as uint32_t) 
declare static sub  _BVS(adr as uint32_t) : declare static sub  _CLC(adr as uint32_t) 
declare static sub  _CLD(adr as uint32_t) : declare static sub  _CLI(adr as uint32_t) 
declare static sub  _CLV(adr as uint32_t) : declare static sub  _CMP(adr as uint32_t) 
declare static sub  _CPX(adr as uint32_t) : declare static sub  _CPY(adr as uint32_t) 
declare static sub  _DEC(adr as uint32_t) : declare static sub  _DEX(adr as uint32_t) 
declare static sub  _DEY(adr as uint32_t) : declare static sub  _EOR(adr as uint32_t) 
declare static sub  _INC(adr as uint32_t) : declare static sub  _INX(adr as uint32_t) 
declare static sub  _INY(adr as uint32_t) : declare static sub  _JMP(adr as uint32_t) 
declare static sub  _JSR(adr as uint32_t) : declare static sub  _LDA(adr as uint32_t) 
declare static sub  _LDX(adr as uint32_t) : declare static sub  _LDY(adr as uint32_t) 
declare static sub  _LSR(adr as uint32_t) : declare static sub  _NOP(adr as uint32_t) 
declare static sub  _ORA(adr as uint32_t) : declare static sub  _PHA(adr as uint32_t) 
declare static sub  _PHP(adr as uint32_t) : declare static sub  _PLA(adr as uint32_t) 
declare static sub  _PLP(adr as uint32_t) : declare static sub  _ROL(adr as uint32_t) 
declare static sub  _ROR(adr as uint32_t) : declare static sub  _RTI(adr as uint32_t) 
declare static sub  _RTS(adr as uint32_t) : declare static sub  _SBC(adr as uint32_t) 
declare static sub  _SEC(adr as uint32_t) : declare static sub  _SED(adr as uint32_t) 
declare static sub  _SEI(adr as uint32_t) : declare static sub  _STA(adr as uint32_t) 
declare static sub  _STX(adr as uint32_t) : declare static sub  _STY(adr as uint32_t) 
declare static sub  _TAX(adr as uint32_t) : declare static sub  _TAY(adr as uint32_t) 
declare static sub  _TSX(adr as uint32_t) : declare static sub  _TXA(adr as uint32_t) 
declare static sub  _TXS(adr as uint32_t) : declare static sub  _TYA(adr as uint32_t) 
declare static sub  _XXX(adr as uint32_t) 
	
	
declare static sub  _ASLa(adr as uint32_t) 	
declare static sub  _ROLa(adr as uint32_t) 
declare static sub  _LSRa(adr as uint32_t) 	
declare static sub  _RORa(adr as uint32_t)
 		
declare static sub  _NMI(adr as uint32_t) 		
declare static sub  _IRQ(adr as uint32_t)  		
 		
'undocumented opcodes

	
declare static sub _KIL(adr as uint32_t)
declare static sub  _SLO(adr as uint32_t) : declare static sub  _rla(adr as uint32_t) 
declare static sub  _SRE(adr as uint32_t) : declare static sub  _RRA(adr as uint32_t) 
declare static sub  _SAX(adr as uint32_t) : declare static sub  _LAX(adr as uint32_t) 
declare static sub  _DCP(adr as uint32_t) : declare static sub  _ISC(adr as uint32_t) 
declare static sub  _ANC(adr as uint32_t) : declare static sub  _ALR(adr as uint32_t) 
declare static sub  _ARR(adr as uint32_t) : declare static sub  _AXS(adr as uint32_t) 	
	
	
	
	
	
	
	
	
	
'declare sub uni(adr as uint32_t)
'declare  static  sub  _ADC() : Declare  static  sub  _AND() 
'declare  static  sub  _ASL() : Declare  static  sub  _BCC() 
'declare  static  sub  _BCS() : Declare  static  sub  _BEQ() 
'declare  static  sub  _BIT() : Declare  static  sub  _BMI() 
'declare  static  sub  _BNE() : Declare  static  sub  _BPL() 
'declare  static  sub  _BRK() : Declare  static  sub  _BVC() 
'declare  static  sub  _BVS() : Declare  static  sub  _CLC() 
'declare  static  sub  _CLD() : Declare  static  sub  _CLI() 
'declare  static  sub  _CLV() : Declare  static  sub  _CMP() 
'declare  static  sub  _CPX() : Declare  static  sub  _CPY() 
'declare  static  sub  _DEC() : Declare  static  sub  _DEX() 
'declare  static  sub  _DEY() : Declare  static  sub  _EOR() 
'declare  static  sub  _INC() : Declare  static  sub  _INX() 
'declare  static  sub  _INY() : Declare  static  sub  _JMP() 
'declare  static  sub  _JSR() : Declare  static  sub  _LDA() 
'declare  static  sub  _LDX() : Declare  static  sub  _LDY() 
'declare  static  sub  _LSR() : Declare  static  sub  _NOP() 
'declare  static  sub  _ORA() : Declare  static  sub  _PHA() 
'declare  static  sub  _PHP() : Declare  static  sub  _PLA() 
'declare  static  sub  _PLP() : Declare  static  sub  _ROL() 
'declare  static  sub  _ROR() : Declare  static  sub  _RTI() 
'declare  static  sub  _RTS() : Declare  static  sub  _SBC() 
'declare  static  sub  _SEC() : Declare  static  sub  _SED() 
'declare  static  sub  _SEI() : Declare  static  sub  _STA() 
'declare  static  sub  _STX() : Declare  static  sub  _STY() 
'declare  static  sub  _TAX() : Declare  static  sub  _TAY() 
'declare  static  sub  _TSX() : Declare  static  sub  _TXA() 
'declare  static  sub  _TXS() : Declare  static  sub  _TYA() 
'Declare  static  sub  _XXX() 




	 '   functions(&H101) as sub(addr1 as uint32_t) => _ 
    '  { _'//x0      x1        x2        x3        x4        x5        x6        x7        x8        x9        xa        xb        xc        xd        xe        xf
    '   _brk,  _ora,  _kil,  _slo,  _nop,  _ora,  _asl,  _slo,  _php,  _ora,  _asla, _anc,  _nop,  _ora,  _asl,  _slo, _' //0x
    '   _bpl,  _ora,  _kil,  _slo,  _nop,  _ora,  _asl,  _slo,  _clc,  _ora,  _nop,  _slo,  _nop,  _ora,  _asl,  _slo, _'//1x
    '   _jsr,  _and,  _kil,  _rla,  _bit,  _and,  _rol,  _rla,  _plp,  _and,  _rola, _anc,  _bit,  _and,  _rol,  _rla, _'//2x
    '   _bmi,  _and,  _kil,  _rla,  _nop,  _and,  _rol,  _rla,  _sec,  _and,  _nop,  _rla,  _nop,  _and,  _rol,  _rla, _'//3x
    '   _rti,  _eor,  _kil,  _sre,  _nop,  _eor,  _lsr,  _sre,  _pha,  _eor,  _lsra, _alr,  _jmp,  _eor,  _lsr,  _sre, _'//4x
    '   _bvc,  _eor,  _kil,  _sre,  _nop,  _eor,  _lsr,  _sre,  _cli,  _eor,  _nop,  _sre,  _nop,  _eor,  _lsr,  _sre, _'//5x
    '   _rts,  _adc,  _kil,  _rra,  _nop,  _adc,  _ror,  _rra,  _pla,  _adc,  _rora, _arr,  _jmp,  _adc,  _ror,  _rra, _'//6x
    '   _bvs,  _adc,  _kil,  _rra,  _nop,  _adc,  _ror,  _rra,  _sei,  _adc,  _nop,  _rra,  _nop,  _adc,  _ror,  _rra, _'//7x
    '   _nop,  _sta,  _nop,  _sax,  _sty,  _sta,  _stx,  _sax,  _dey,  _nop,  _txa,  _uni,  _sty,  _sta,  _stx,  _sax, _'//8x
    '   _bcc,  _sta,  _kil,  _uni,  _sty,  _sta,  _stx,  _sax,  _tya,  _sta,  _txs,  _uni,  _uni,  _sta,  _uni,  _uni, _'//9x
    '   _ldy,  _lda,  _ldx,  _lax,  _ldy,  _lda,  _ldx,  _lax,  _tay,  _lda,  _tax,  _uni,  _ldy,  _lda,  _ldx,  _lax, _'//ax
    '   _bcs,  _lda,  _kil,  _lax,  _ldy,  _lda,  _ldx,  _lax,  _clv,  _lda,  _tsx,  _uni,  _ldy,  _lda,  _ldx,  _lax, _'//bx
    '   _cpy,  _cmp,  _nop,  _dcp,  _cpy,  _cmp,  _dec,  _dcp,  _iny,  _cmp,  _dex,  _axs,  _cpy,  _cmp,  _dec,  _dcp, _'//cx
    '   _bne,  _cmp,  _kil,  _dcp,  _nop,  _cmp,  _dec,  _dcp,  _cld,  _cmp,  _nop,  _dcp,  _nop,  _cmp,  _dec,  _dcp, _'//dx
    '   _cpx,  _sbc,  _nop,  _isc,  _cpx,  _sbc,  _inc,  _isc,  _inx,  _sbc,  _nop,  _sbc,  _cpx,  _sbc,  _inc,  _isc, _'//ex
    '   _beq,  _sbc,  _kil,  _isc,  _nop,  _sbc,  _inc,  _isc,  _sed,  _sbc,  _nop,  _isc,  _nop,  _sbc,  _inc,  _isc, _'//fx
    '   _nmi,  _irq _ '// 0x100: NMI, 0x101: IRQ
    '}

		   ' // function table
    functions(&H101) as sub(addr1 as uint32_t) => _ 
      { _'//x0      x1        x2        x3        x4        x5        x6        x7        x8        x9        xa        xb        xc        xd        xe        xf
       @_brk,  @_ora,  @_kil,  @_slo,  @_nop,  @_ora,  @_asl,  @_slo,  @_php,  @_ora,  @_asla, @_anc,  @_nop,  @_ora,  @_asl,  @_slo, _' //0x
       @_bpl,  @_ora,  @_kil,  @_slo,  @_nop,  @_ora,  @_asl,  @_slo,  @_clc,  @_ora,  @_nop,  @_slo,  @_nop,  @_ora,  @_asl,  @_slo, _'//1x
       @_jsr,  @_and,  @_kil,  @_rla,  @_bit,  @_and,  @_rol,  @_rla,  @_plp,  @_and,  @_rola, @_anc,  @_bit,  @_and,  @_rol,  @_rla, _'//2x
       @_bmi,  @_and,  @_kil,  @_rla,  @_nop,  @_and,  @_rol,  @_rla,  @_sec,  @_and,  @_nop,  @_rla,  @_nop,  @_and,  @_rol,  @_rla, _'//3x
       @_rti,  @_eor,  @_kil,  @_sre,  @_nop,  @_eor,  @_lsr,  @_sre,  @_pha,  @_eor,  @_lsra, @_alr,  @_jmp,  @_eor,  @_lsr,  @_sre, _'//4x
       @_bvc,  @_eor,  @_kil,  @_sre,  @_nop,  @_eor,  @_lsr,  @_sre,  @_cli,  @_eor,  @_nop,  @_sre,  @_nop,  @_eor,  @_lsr,  @_sre, _'//5x
       @_rts,  @_adc,  @_kil,  @_rra,  @_nop,  @_adc,  @_ror,  @_rra,  @_pla,  @_adc,  @_rora, @_arr,  @_jmp,  @_adc,  @_ror,  @_rra, _'//6x
       @_bvs,  @_adc,  @_kil,  @_rra,  @_nop,  @_adc,  @_ror,  @_rra,  @_sei,  @_adc,  @_nop,  @_rra,  @_nop,  @_adc,  @_ror,  @_rra, _'//7x
       @_nop,  @_sta,  @_nop,  @_sax,  @_sty,  @_sta,  @_stx,  @_sax,  @_dey,  @_nop,  @_txa,  @_uni,  @_sty,  @_sta,  @_stx,  @_sax, _'//8x
       @_bcc,  @_sta,  @_kil,  @_uni,  @_sty,  @_sta,  @_stx,  @_sax,  @_tya,  @_sta,  @_txs,  @_uni,  @_uni,  @_sta,  @_uni,  @_uni, _'//9x
       @_ldy,  @_lda,  @_ldx,  @_lax,  @_ldy,  @_lda,  @_ldx,  @_lax,  @_tay,  @_lda,  @_tax,  @_uni,  @_ldy,  @_lda,  @_ldx,  @_lax, _'//ax
       @_bcs,  @_lda,  @_kil,  @_lax,  @_ldy,  @_lda,  @_ldx,  @_lax,  @_clv,  @_lda,  @_tsx,  @_uni,  @_ldy,  @_lda,  @_ldx,  @_lax, _'//bx
       @_cpy,  @_cmp,  @_nop,  @_dcp,  @_cpy,  @_cmp,  @_dec,  @_dcp,  @_iny,  @_cmp,  @_dex,  @_axs,  @_cpy,  @_cmp,  @_dec,  @_dcp, _'//cx
       @_bne,  @_cmp,  @_kil,  @_dcp,  @_nop,  @_cmp,  @_dec,  @_dcp,  @_cld,  @_cmp,  @_nop,  @_dcp,  @_nop,  @_cmp,  @_dec,  @_dcp, _'//dx
       @_cpx,  @_sbc,  @_nop,  @_isc,  @_cpx,  @_sbc,  @_inc,  @_isc,  @_inx,  @_sbc,  @_nop,  @_sbc,  @_cpx,  @_sbc,  @_inc,  @_isc, _'//ex
       @_beq,  @_sbc,  @_kil,  @_isc,  @_nop,  @_sbc,  @_inc,  @_isc,  @_sed,  @_sbc,  @_nop,  @_isc,  @_nop,  @_sbc,  @_inc,  @_isc, _'//fx
       @_nmi,  @_irq _ '// 0x100: NMI, 0x101: IRQ
    }



















	
		   ' // function table
    'this.functions(&HFF) => 
    '  { _'//x0      x1        x2        x3        x4        x5        x6        x7        x8        x9        xa        xb        xc        xd        xe        xf
    '   .brk,  .ora,  .kil,  .slo,  .nop,  .ora,  .asl,  .slo,  .php,  .ora,  .asla, .anc,  .nop,  .ora,  .asl,  .slo, _' //0x
    '   .bpl,  .ora,  .kil,  .slo,  .nop,  .ora,  .asl,  .slo,  .clc,  .ora,  .nop,  .slo,  .nop,  .ora,  .asl,  .slo, _'//1x
    '   .jsr,  .and,  .kil,  .rla,  .bit,  .and,  .rol,  .rla,  .plp,  .and,  .rola, .anc,  .bit,  .and,  .rol,  .rla, _'//2x
    '   .bmi,  .and,  .kil,  .rla,  .nop,  .and,  .rol,  .rla,  .sec,  .and,  .nop,  .rla,  .nop,  .and,  .rol,  .rla, _'//3x
    '   .rti,  .eor,  .kil,  .sre,  .nop,  .eor,  .lsr,  .sre,  .pha,  .eor,  .lsra, .alr,  .jmp,  .eor,  .lsr,  .sre, _'//4x
    '   .bvc,  .eor,  .kil,  .sre,  .nop,  .eor,  .lsr,  .sre,  .cli,  .eor,  .nop,  .sre,  .nop,  .eor,  .lsr,  .sre, _'//5x
    '   .rts,  .adc,  .kil,  .rra,  .nop,  .adc,  .ror,  .rra,  .pla,  .adc,  .rora, .arr,  .jmp,  .adc,  .ror,  .rra, _'//6x
    '   .bvs,  .adc,  .kil,  .rra,  .nop,  .adc,  .ror,  .rra,  .sei,  .adc,  .nop,  .rra,  .nop,  .adc,  .ror,  .rra, _'//7x
    '   .nop,  .sta,  .nop,  .sax,  .sty,  .sta,  .stx,  .sax,  .dey,  .nop,  .txa,  .uni,  .sty,  .sta,  .stx,  .sax, _'//8x
    '   .bcc,  .sta,  .kil,  .uni,  .sty,  .sta,  .stx,  .sax,  .tya,  .sta,  .txs,  .uni,  .uni,  .sta,  .uni,  .uni, _'//9x
    '   .ldy,  .lda,  .ldx,  .lax,  .ldy,  .lda,  .ldx,  .lax,  .tay,  .lda,  .tax,  .uni,  .ldy,  .lda,  .ldx,  .lax, _'//ax
    '   .bcs,  .lda,  .kil,  .lax,  .ldy,  .lda,  .ldx,  .lax,  .clv,  .lda,  .tsx,  .uni,  .ldy,  .lda,  .ldx,  .lax, _'//bx
    '   .cpy,  .cmp,  .nop,  .dcp,  .cpy,  .cmp,  .dec,  .dcp,  .iny,  .cmp,  .dex,  .axs,  .cpy,  .cmp,  .dec,  .dcp, _'//cx
    '   .bne,  .cmp,  .kil,  .dcp,  .nop,  .cmp,  .dec,  .dcp,  .cld,  .cmp,  .nop,  .dcp,  .nop,  .cmp,  .dec,  .dcp, _'//dx
    '   .cpx,  .sbc,  .nop,  .isc,  .cpx,  .sbc,  .inc,  .isc,  .inx,  .sbc,  .nop,  .sbc,  .cpx,  .sbc,  .inc,  .isc, _'//ex
    '   .beq,  .sbc,  .kil,  .isc,  .nop,  .sbc,  .inc,  .isc,  .sed,  .sbc,  .nop,  .isc,  .nop,  .sbc,  .inc,  .isc, _'//fx
    '   .nmi,  .irq '// 0x100: NMI, 0x101: IRQ
    '}
	
	   ' // function table
    'this.functions(&HFF) => 
    '  { _'//x0      x1        x2        x3        x4        x5        x6        x7        x8        x9        xa        xb        xc        xd        xe        xf
    '  this.brk, this.ora, this.kil, this.slo, this.nop, this.ora, this.asl, this.slo, this.php, this.ora, this.asla,this.anc, this.nop, this.ora, this.asl, this.slo, _' //0x
    '  this.bpl, this.ora, this.kil, this.slo, this.nop, this.ora, this.asl, this.slo, this.clc, this.ora, this.nop, this.slo, this.nop, this.ora, this.asl, this.slo, _'//1x
    '  this.jsr, this.and, this.kil, this.rla, this.bit, this.and, this.rol, this.rla, this.plp, this.and, this.rola,this.anc, this.bit, this.and, this.rol, this.rla, _'//2x
    '  this.bmi, this.and, this.kil, this.rla, this.nop, this.and, this.rol, this.rla, this.sec, this.and, this.nop, this.rla, this.nop, this.and, this.rol, this.rla, _'//3x
    '  this.rti, this.eor, this.kil, this.sre, this.nop, this.eor, this.lsr, this.sre, this.pha, this.eor, this.lsra,this.alr, this.jmp, this.eor, this.lsr, this.sre, _'//4x
    '  this.bvc, this.eor, this.kil, this.sre, this.nop, this.eor, this.lsr, this.sre, this.cli, this.eor, this.nop, this.sre, this.nop, this.eor, this.lsr, this.sre, _'//5x
    '  this.rts, this.adc, this.kil, this.rra, this.nop, this.adc, this.ror, this.rra, this.pla, this.adc, this.rora,this.arr, this.jmp, this.adc, this.ror, this.rra, _'//6x
    '  this.bvs, this.adc, this.kil, this.rra, this.nop, this.adc, this.ror, this.rra, this.sei, this.adc, this.nop, this.rra, this.nop, this.adc, this.ror, this.rra, _'//7x
    '  this.nop, this.sta, this.nop, this.sax, this.sty, this.sta, this.stx, this.sax, this.dey, this.nop, this.txa, this.uni, this.sty, this.sta, this.stx, this.sax, _'//8x
    '  this.bcc, this.sta, this.kil, this.uni, this.sty, this.sta, this.stx, this.sax, this.tya, this.sta, this.txs, this.uni, this.uni, this.sta, this.uni, this.uni, _'//9x
    '  this.ldy, this.lda, this.ldx, this.lax, this.ldy, this.lda, this.ldx, this.lax, this.tay, this.lda, this.tax, this.uni, this.ldy, this.lda, this.ldx, this.lax, _'//ax
    '  this.bcs, this.lda, this.kil, this.lax, this.ldy, this.lda, this.ldx, this.lax, this.clv, this.lda, this.tsx, this.uni, this.ldy, this.lda, this.ldx, this.lax, _'//bx
    '  this.cpy, this.cmp, this.nop, this.dcp, this.cpy, this.cmp, this.dec, this.dcp, this.iny, this.cmp, this.dex, this.axs, this.cpy, this.cmp, this.dec, this.dcp, _'//cx
    '  this.bne, this.cmp, this.kil, this.dcp, this.nop, this.cmp, this.dec, this.dcp, this.cld, this.cmp, this.nop, this.dcp, this.nop, this.cmp, this.dec, this.dcp, _'//dx
    '  this.cpx, this.sbc, this.nop, this.isc, this.cpx, this.sbc, this.inc, this.isc, this.inx, this.sbc, this.nop, this.sbc, this.cpx, this.sbc, this.inc, this.isc, _'//ex
    '  this.beq, this.sbc, this.kil, this.isc, this.nop, this.sbc, this.inc, this.isc, this.sed, this.sbc, this.nop, this.isc, this.nop, this.sbc, this.inc, this.isc, _'//fx
    '  this.nmi, this.irq '// 0x100: NMI, 0x101: IRQ
    '} 
	
	
	
  static self as CPU ptr



	
End Type

 dim CPU.self as CPU ptr


