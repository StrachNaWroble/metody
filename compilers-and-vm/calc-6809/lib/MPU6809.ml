type label = string

type word8  = int
type word16 = int

type register =
  | RegD  | RegX | RegY | RegU  | RegS
  | RegPC | RegA | RegB | RegCC | RegDP

type index_register = IxX | IxY | IxU | IxS

type indexed_addr =
  | IA_Index         of int * index_register (* n, R     *)
  | IA_IndexA        of index_register       (* A, R     *)
  | IA_IndexB        of index_register       (* B, R     *)
  | IA_IndexD        of index_register       (* D, R     *)
  | IA_Incr1         of index_register       (* , R+     *)
  | IA_Incr2         of index_register       (* , R++    *)
  | IA_Decr1         of index_register       (* , -R     *)
  | IA_Decr2         of index_register       (* , --R    *)
  | IA_Label         of int * label          (* n+l      *)
  | IA_PCOff         of int                  (* n, PCR   *)
  | IA_IndirIndex    of int * index_register (* [n, R]   *)
  | IA_IndirIndexA   of index_register       (* [A, R]   *)
  | IA_IndirIndexB   of index_register       (* [B, R]   *)
  | IA_IndirIndexD   of index_register       (* [D, R]   *)
  | IA_IndirIncr2    of index_register       (* [,R++]   *)
  | IA_IndirDecr2    of index_register       (* [,--R]   *)
  | IA_IndirLabel    of int * label          (* [n+l]    *)
  | IA_IndirPCOff    of int                  (* [n, PCR] *)
  | IA_IndirExtended of word16               (* [n]      *)

type addr =
  | ADirect   of word8
  | AIndexed  of indexed_addr
  | AExtended of word16

type addr_imm8 =
  | Imm8  of word8
  | Addr8 of addr

type addr_imm16 =
  | Imm16  of word16
  | Addr16 of addr

type rel8 =
  | R8Label of label
  | R8Imm   of int

type rel16 =
  | R16Label of label
  | R16Imm   of int

type instr =
  | Label  of label
  | Asciiz of string
  | ABX
  | ADCA   of addr_imm8
  | ADCB   of addr_imm8
  | ADDA   of addr_imm8
  | ADDB   of addr_imm8
  | ADDD   of addr_imm16
  | ANDA   of addr_imm8
  | ANDB   of addr_imm8
  | ANDCC  of word8
  | ASLA
  | ASLB
  | ASL    of addr
  | ASRA
  | ASRB
  | ASR    of addr
  | BCC    of rel8
  | BCS    of rel8
  | BEQ    of rel8
  | BGE    of rel8
  | BGT    of rel8
  | BHI    of rel8
  | BHS    of rel8
  | BITA   of addr_imm8
  | BITB   of addr_imm8
  | BLE    of rel8
  | BLO    of rel8
  | BLS    of rel8
  | BLT    of rel8
  | BMI    of rel8
  | BNE    of rel8
  | BPL    of rel8
  | BRA    of rel8
  | BRN    of rel8
  | BSR    of rel8
  | BVC    of rel8
  | BVS    of rel8
  | CLRA
  | CLRB
  | CLR    of addr
  | CMPA   of addr_imm8
  | CMPB   of addr_imm8
  | CMPD   of addr_imm16
  | CMPS   of addr_imm16
  | CMPU   of addr_imm16
  | CMPX   of addr_imm16
  | CMPY   of addr_imm16
  | COMA
  | COMB
  | COM    of addr
  | CWAI   of word8
  | DAA
  | DECA
  | DECB
  | DEC    of addr
  | EORA   of addr_imm8
  | EORB   of addr_imm8
  | EXG    of register * register
  | INCA
  | INCB
  | INC    of addr
  | JMP    of addr
  | JSR    of addr
  | LBCC   of rel16
  | LBCS   of rel16
  | LBEQ   of rel16
  | LBGE   of rel16
  | LBGT   of rel16
  | LBHI   of rel16
  | LBHS   of rel16
  | LBLE   of rel16
  | LBLO   of rel16
  | LBLS   of rel16
  | LBLT   of rel16
  | LBMI   of rel16
  | LBNE   of rel16
  | LBPL   of rel16
  | LBRA   of rel16
  | LBRN   of rel16
  | LBSR   of rel16
  | LBVC   of rel16
  | LBVS   of rel16
  | LDA    of addr_imm8
  | LDB    of addr_imm8
  | LDD    of addr_imm16
  | LDS    of addr_imm16
  | LDU    of addr_imm16
  | LDX    of addr_imm16
  | LDY    of addr_imm16
  | LEAS   of indexed_addr
  | LEAU   of indexed_addr
  | LEAX   of indexed_addr
  | LEAY   of indexed_addr
  | LSLA
  | LSLB
  | LSL    of addr
  | LSRA
  | LSRB
  | LSR    of addr
  | MUL
  | NEGA
  | NEGB
  | NEG    of addr
  | NOP
  | ORA    of addr_imm8
  | ORB    of addr_imm8
  | ORCC   of word8
  | PSHS   of register list
  | PSHU   of register list
  | PULS   of register list
  | PULU   of register list
  | ROLA
  | ROLB
  | ROL    of addr
  | RORA
  | RORB
  | ROR    of addr
  | RTI
  | RTS
  | SBCA   of addr_imm8
  | SBCB   of addr_imm8
  | SEX
  | STA    of addr
  | STB    of addr
  | STD    of addr
  | STS    of addr
  | STU    of addr
  | STX    of addr
  | STY    of addr
  | SUBA   of addr_imm8
  | SUBB   of addr_imm8
  | SUBD   of addr_imm16
  | SWI
  | SWI2
  | SWI3
  | SYNC
  | TFR    of register * register
  | TSTA
  | TSTB
  | TST    of addr

let a_index x r = AIndexed(IA_Index(x, r))
let a_decr2 r = AIndexed(IA_Decr2(r))

let a8_incr1 r = Addr8(AIndexed(IA_Incr1(r)))

let a16_index x r = Addr16(AIndexed(IA_Index(x, r)))
let a16_incr2 r = Addr16(AIndexed(IA_Incr2(r)))

(* ========================================================================= *)

let string_of_register r =
  match r with
  | RegD  -> "D"
  | RegX  -> "X"
  | RegY  -> "Y"
  | RegU  -> "U"
  | RegS  -> "S"
  | RegPC -> "PC"
  | RegA  -> "A"
  | RegB  -> "B"
  | RegCC -> "CC"
  | RegDP -> "DP"

let string_of_index_register r =
  match r with
  | IxX -> "X"
  | IxY -> "Y"
  | IxU -> "U"
  | IxS -> "S"

let string_of_indexed_addr a =
  match a with
  | IA_Index(0, r) -> Printf.sprintf ", %s" (string_of_index_register r)
  | IA_Index(n, r) -> Printf.sprintf "%d, %s" n (string_of_index_register r)
  | IA_IndexA r    -> Printf.sprintf "A, %s" (string_of_index_register r)
  | IA_IndexB r    -> Printf.sprintf "B, %s" (string_of_index_register r)
  | IA_IndexD r    -> Printf.sprintf "D, %s" (string_of_index_register r)
  | IA_Incr1  r    -> Printf.sprintf ", %s+" (string_of_index_register r)
  | IA_Incr2  r    -> Printf.sprintf ", %s++" (string_of_index_register r)
  | IA_Decr1  r    -> Printf.sprintf ", -%s" (string_of_index_register r)
  | IA_Decr2  r    -> Printf.sprintf ", --%s" (string_of_index_register r)
  | IA_Label(0, l) -> Printf.sprintf "%s, PCR" l
  | IA_Label(n, l) -> Printf.sprintf "%d+%s, PCR" n l
  | IA_PCOff n     -> Printf.sprintf "%d+ *, PCR" n
  | IA_IndirIndex(n, r) ->
      Printf.sprintf "[%d, %s]" n (string_of_index_register r)
  | IA_IndirIndexA r ->
      Printf.sprintf "[A, %s]" (string_of_index_register r)
  | IA_IndirIndexB r ->
      Printf.sprintf "[B, %s]" (string_of_index_register r)
  | IA_IndirIndexD r ->
      Printf.sprintf "[D, %s]" (string_of_index_register r)
  | IA_IndirIncr2  r ->
      Printf.sprintf "[, %s++]" (string_of_index_register r)
  | IA_IndirDecr2  r ->
      Printf.sprintf "[, --%s]" (string_of_index_register r)
  | IA_IndirLabel(0, l) -> Printf.sprintf "[%s, PCR]" l
  | IA_IndirLabel(n, l) -> Printf.sprintf "[%d+%s, PCR]" n l
  | IA_IndirPCOff n     -> Printf.sprintf "[%d+ *, PCR]" n
  | IA_IndirExtended n  -> Printf.sprintf "[%d]" n

let string_of_addr a =
  match a with
  | ADirect   n -> Printf.sprintf "< %d" n
  | AIndexed  a -> string_of_indexed_addr a
  | AExtended n -> Printf.sprintf "%d" n

let string_of_addr_imm8 a =
  match a with
  | Imm8   n -> Printf.sprintf "#%d" n
  | Addr8  a -> string_of_addr a

let string_of_addr_imm16 a =
  match a with
  | Imm16   n -> Printf.sprintf "#%d" n
  | Addr16  a -> string_of_addr a

let string_of_rel8 r =
  match r with
  | R8Label l -> l
  | R8Imm   n -> Printf.sprintf "#%d" n

let string_of_rel16 r =
  match r with
  | R16Label l -> l
  | R16Imm   n -> Printf.sprintf "#%d" n

let string_of_instr instr =
  match instr with
  | Label  l -> l
  | Asciiz s -> Printf.sprintf "    FCB       \"%s\", 0" (String.escaped s)
  | ABX      -> "    ABX"
  | ADCA   a -> Printf.sprintf "    ADCA      %s" (string_of_addr_imm8  a)
  | ADCB   a -> Printf.sprintf "    ADCB      %s" (string_of_addr_imm8  a)
  | ADDA   a -> Printf.sprintf "    ADDA      %s" (string_of_addr_imm8  a)
  | ADDB   a -> Printf.sprintf "    ADDB      %s" (string_of_addr_imm8  a)
  | ADDD   a -> Printf.sprintf "    ADDD      %s" (string_of_addr_imm16 a)
  | ANDA   a -> Printf.sprintf "    ANDA      %s" (string_of_addr_imm8  a)
  | ANDB   a -> Printf.sprintf "    ANDB      %s" (string_of_addr_imm8  a)
  | ANDCC  a -> Printf.sprintf "    ANDCC     #%d" a
  | ASLA     -> "    ASLA"
  | ASLB     -> "    ASLB"
  | ASL    a -> Printf.sprintf "    ASL       %s" (string_of_addr a)
  | ASRA     -> "    ASRA"
  | ASRB     -> "    ASRB"
  | ASR    a -> Printf.sprintf "    ASR       %s" (string_of_addr a)
  | BCC    a -> Printf.sprintf "    BCC       %s" (string_of_rel8 a)
  | BCS    a -> Printf.sprintf "    BCS       %s" (string_of_rel8 a)
  | BEQ    a -> Printf.sprintf "    BEQ       %s" (string_of_rel8 a)
  | BGE    a -> Printf.sprintf "    BGE       %s" (string_of_rel8 a)
  | BGT    a -> Printf.sprintf "    BGT       %s" (string_of_rel8 a)
  | BHI    a -> Printf.sprintf "    BHI       %s" (string_of_rel8 a)
  | BHS    a -> Printf.sprintf "    BHS       %s" (string_of_rel8 a)
  | BITA   a -> Printf.sprintf "    BITA      %s" (string_of_addr_imm8  a)
  | BITB   a -> Printf.sprintf "    BITB      %s" (string_of_addr_imm8  a)
  | BLE    a -> Printf.sprintf "    BLE       %s" (string_of_rel8 a)
  | BLO    a -> Printf.sprintf "    BLO       %s" (string_of_rel8 a)
  | BLS    a -> Printf.sprintf "    BLS       %s" (string_of_rel8 a)
  | BLT    a -> Printf.sprintf "    BLT       %s" (string_of_rel8 a)
  | BMI    a -> Printf.sprintf "    BMI       %s" (string_of_rel8 a)
  | BNE    a -> Printf.sprintf "    BNE       %s" (string_of_rel8 a)
  | BPL    a -> Printf.sprintf "    BPL       %s" (string_of_rel8 a)
  | BRA    a -> Printf.sprintf "    BRA       %s" (string_of_rel8 a)
  | BRN    a -> Printf.sprintf "    BRN       %s" (string_of_rel8 a)
  | BSR    a -> Printf.sprintf "    BSR       %s" (string_of_rel8 a)
  | BVC    a -> Printf.sprintf "    BVC       %s" (string_of_rel8 a)
  | BVS    a -> Printf.sprintf "    BVS       %s" (string_of_rel8 a)
  | CLRA     -> "    CLRA"
  | CLRB     -> "    CLRB"
  | CLR    a -> Printf.sprintf "    CLR       %s" (string_of_addr a)
  | CMPA   a -> Printf.sprintf "    CMPA      %s" (string_of_addr_imm8  a)
  | CMPB   a -> Printf.sprintf "    CMPB      %s" (string_of_addr_imm8  a)
  | CMPD   a -> Printf.sprintf "    CMPD      %s" (string_of_addr_imm16 a)
  | CMPS   a -> Printf.sprintf "    CMPS      %s" (string_of_addr_imm16 a)
  | CMPU   a -> Printf.sprintf "    CMPU      %s" (string_of_addr_imm16 a)
  | CMPX   a -> Printf.sprintf "    CMPX      %s" (string_of_addr_imm16 a)
  | CMPY   a -> Printf.sprintf "    CMPY      %s" (string_of_addr_imm16 a)
  | COMA     -> "    COMA"
  | COMB     -> "    COMB"
  | COM    a -> Printf.sprintf "    COM       %s" (string_of_addr a)
  | CWAI   a -> Printf.sprintf "    CWAI      #%d" a
  | DAA      -> "    DAA"
  | DECA     -> "    DECA"
  | DECB     -> "    DECB"
  | DEC    a -> Printf.sprintf "    DEC       %s" (string_of_addr a)
  | EORA   a -> Printf.sprintf "    EORA      %s" (string_of_addr_imm8  a)
  | EORB   a -> Printf.sprintf "    EORB      %s" (string_of_addr_imm8  a)
  | EXG(r1, r2) ->
      Printf.sprintf "    EXG       %s, %s"
        (string_of_register r1)
        (string_of_register r2)
  | INCA     -> "    INCA"
  | INCB     -> "    INCB"
  | INC    a -> Printf.sprintf "    INC       %s" (string_of_addr a)
  | JMP    a -> Printf.sprintf "    JMP       %s" (string_of_addr a)
  | JSR    a -> Printf.sprintf "    JSR       %s" (string_of_addr a)
  | LBCC   a -> Printf.sprintf "    LBCC      %s" (string_of_rel16 a)
  | LBCS   a -> Printf.sprintf "    LBCS      %s" (string_of_rel16 a)
  | LBEQ   a -> Printf.sprintf "    LBEQ      %s" (string_of_rel16 a)
  | LBGE   a -> Printf.sprintf "    LBGE      %s" (string_of_rel16 a)
  | LBGT   a -> Printf.sprintf "    LBGT      %s" (string_of_rel16 a)
  | LBHI   a -> Printf.sprintf "    LBHI      %s" (string_of_rel16 a)
  | LBHS   a -> Printf.sprintf "    LBHS      %s" (string_of_rel16 a)
  | LBLE   a -> Printf.sprintf "    LBLE      %s" (string_of_rel16 a)
  | LBLO   a -> Printf.sprintf "    LBLO      %s" (string_of_rel16 a)
  | LBLS   a -> Printf.sprintf "    LBLS      %s" (string_of_rel16 a)
  | LBLT   a -> Printf.sprintf "    LBLT      %s" (string_of_rel16 a)
  | LBMI   a -> Printf.sprintf "    LBMI      %s" (string_of_rel16 a)
  | LBNE   a -> Printf.sprintf "    LBNE      %s" (string_of_rel16 a)
  | LBPL   a -> Printf.sprintf "    LBPL      %s" (string_of_rel16 a)
  | LBRA   a -> Printf.sprintf "    LBRA      %s" (string_of_rel16 a)
  | LBRN   a -> Printf.sprintf "    LBRN      %s" (string_of_rel16 a)
  | LBSR   a -> Printf.sprintf "    LBSR      %s" (string_of_rel16 a)
  | LBVC   a -> Printf.sprintf "    LBVC      %s" (string_of_rel16 a)
  | LBVS   a -> Printf.sprintf "    LBVS      %s" (string_of_rel16 a)
  | LDA    a -> Printf.sprintf "    LDA       %s" (string_of_addr_imm8  a)
  | LDB    a -> Printf.sprintf "    LDB       %s" (string_of_addr_imm8  a)
  | LDD    a -> Printf.sprintf "    LDD       %s" (string_of_addr_imm16 a)
  | LDS    a -> Printf.sprintf "    LDS       %s" (string_of_addr_imm16 a)
  | LDU    a -> Printf.sprintf "    LDU       %s" (string_of_addr_imm16 a)
  | LDX    a -> Printf.sprintf "    LDX       %s" (string_of_addr_imm16 a)
  | LDY    a -> Printf.sprintf "    LDY       %s" (string_of_addr_imm16 a)
  | LEAS   a -> Printf.sprintf "    LEAS      %s" (string_of_indexed_addr a)
  | LEAU   a -> Printf.sprintf "    LEAU      %s" (string_of_indexed_addr a)
  | LEAX   a -> Printf.sprintf "    LEAX      %s" (string_of_indexed_addr a)
  | LEAY   a -> Printf.sprintf "    LEAY      %s" (string_of_indexed_addr a)
  | LSLA     -> "    LSLA"
  | LSLB     -> "    LSLB"
  | LSL    a -> Printf.sprintf "    LSL       %s" (string_of_addr a)
  | LSRA     -> "    LSRA"
  | LSRB     -> "    LSRB"
  | LSR    a -> Printf.sprintf "    LSR       %s" (string_of_addr a)
  | MUL      -> "    MUL"
  | NEGA     -> "    NEGA"
  | NEGB     -> "    NEGB"
  | NEG    a -> Printf.sprintf "    NEG       %s" (string_of_addr a)
  | NOP      -> "    NOP"
  | ORA    a -> Printf.sprintf "    ORA       %s" (string_of_addr_imm8  a)
  | ORB    a -> Printf.sprintf "    ORB       %s" (string_of_addr_imm8  a)
  | ORCC   a -> Printf.sprintf "    ORCC      #%d" a
  | PSHS   rs ->
      Printf.sprintf "    PSHS      %s"
        (String.concat ", " (List.map string_of_register rs))
  | PSHU   rs ->
      Printf.sprintf "    PSHU      %s"
        (String.concat ", " (List.map string_of_register rs))
  | PULS   rs ->
      Printf.sprintf "    PULS      %s"
        (String.concat ", " (List.map string_of_register rs))
  | PULU   rs ->
      Printf.sprintf "    PULU      %s"
        (String.concat ", " (List.map string_of_register rs))
  | ROLA     -> "    ROLA"
  | ROLB     -> "    ROLB"
  | ROL    a -> Printf.sprintf "    ROL       %s" (string_of_addr a)
  | RORA     -> "    RORA"
  | RORB     -> "    RORB"
  | ROR    a -> Printf.sprintf "    ROR       %s" (string_of_addr a)
  | RTI      -> "    RTI"
  | RTS      -> "    RTS"
  | SBCA   a -> Printf.sprintf "    SBCA      %s" (string_of_addr_imm8  a)
  | SBCB   a -> Printf.sprintf "    SBCB      %s" (string_of_addr_imm8  a)
  | SEX      -> "    SEX" 
  | STA    a -> Printf.sprintf "    STA       %s" (string_of_addr a)
  | STB    a -> Printf.sprintf "    STB       %s" (string_of_addr a)
  | STD    a -> Printf.sprintf "    STD       %s" (string_of_addr a)
  | STS    a -> Printf.sprintf "    STS       %s" (string_of_addr a)
  | STU    a -> Printf.sprintf "    STU       %s" (string_of_addr a)
  | STX    a -> Printf.sprintf "    STX       %s" (string_of_addr a)
  | STY    a -> Printf.sprintf "    STY       %s" (string_of_addr a)
  | SUBA   a -> Printf.sprintf "    SUBA      %s" (string_of_addr_imm8  a)
  | SUBB   a -> Printf.sprintf "    SUBB      %s" (string_of_addr_imm8  a)
  | SUBD   a -> Printf.sprintf "    SUBD      %s" (string_of_addr_imm16 a)
  | SWI      -> "    SWI"
  | SWI2     -> "    SWI2"
  | SWI3     -> "    SWI3"
  | SYNC     -> "    SYNC"
  | TFR(r1, r2) ->
      Printf.sprintf "    TFR       %s, %s"
        (string_of_register r1)
        (string_of_register r2)
  | TSTA     -> "    TSTA"
  | TSTB     -> "    TSTB"
  | TST    a -> Printf.sprintf "    TST       %s" (string_of_addr a)
