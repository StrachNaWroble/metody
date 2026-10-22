type state =
  {         code    : Buffer.t;
            symbols : (string, int) Hashtbl.t;
    mutable patches : (int * (unit -> int)) list;
  }

let get_symbol state name =
  try Hashtbl.find state.symbols name
  with Not_found -> failwith ("Undefined label: " ^ name)

let emit_byte state byte =
  Buffer.add_char state.code (Char.chr (byte land 0xFF))

let emit_word state word =
  emit_byte state ((word lsr 8) land 0xFF);
  emit_byte state (word land 0xFF)

let emit_byte_patch state label get_value =
  let offset = Buffer.length state.code in
  emit_byte state 0; (* placeholder *)
  let patch () =
    let offset = get_value (get_symbol state label) in
    if offset < -128 || offset > 127 then
      failwith ("Branch target out of range for label: " ^ label);
    offset land 0xFF
  in
  state.patches <- (offset, patch) :: state.patches

let emit_word_patch state label get_value =
  let offset = Buffer.length state.code in
  emit_byte state 0; (* placeholder *)
  emit_byte state 0; (* placeholder *)
  let patch_h () = get_value (get_symbol state label) lsr 8 in
  let patch_l () = get_value (get_symbol state label) in
  state.patches <- (offset, patch_h) :: (offset + 1, patch_l) :: state.patches

let index_register (r : MPU6809.index_register) =
  match r with
  | IxX -> 0
  | IxY -> 1
  | IxU -> 2
  | IxS -> 3

let reg_nibble (r : MPU6809.register) =
  match r with
  | RegD  -> 0
  | RegX  -> 1
  | RegY  -> 2
  | RegU  -> 3
  | RegS  -> 4
  | RegPC -> 5
  | RegA  -> 8
  | RegB  -> 9
  | RegCC -> 10
  | RegDP -> 11

let reg2_byte r0 r1 =
  (reg_nibble r0 lsl 4) lor reg_nibble r1

let register_stack_mask =
  List.fold_left
    (fun acc (r : MPU6809.register) -> acc lor
      match r with
      | RegD  -> 0x06
      | RegX  -> 0x10
      | RegY  -> 0x20
      | RegU  -> 0x40
      | RegS  -> 0x40
      | RegPC -> 0x80
      | RegA  -> 0x02
      | RegB  -> 0x04
      | RegCC -> 0x01
      | RegDP -> 0x08)
    0

let indexed_address state (addr : MPU6809.indexed_addr) =
  match addr with
  | IA_Index(0, r) -> emit_byte state (0x84 lor (index_register r lsl 5))
  | IA_Index(n, r) when n >= -16 && n <= 15 ->
      emit_byte state (((n land 0x1F) lor (index_register r lsl 5)))
  | IA_Index(n, r) when n >= -128 && n <= 127 ->
      emit_byte state (0x88 lor (index_register r lsl 5));
      emit_byte state n
  | IA_Index(n, r) ->
      emit_byte state (0x89 lor (index_register r lsl 5));
      emit_word state n
  | IA_IndexA r -> emit_byte state (0x86 lor (index_register r lsl 5))
  | IA_IndexB r -> emit_byte state (0x85 lor (index_register r lsl 5))
  | IA_IndexD r -> emit_byte state (0x8B lor (index_register r lsl 5))
  | IA_Incr1  r -> emit_byte state (0x80 lor (index_register r lsl 5))
  | IA_Incr2  r -> emit_byte state (0x81 lor (index_register r lsl 5))
  | IA_Decr1  r -> emit_byte state (0x82 lor (index_register r lsl 5))
  | IA_Decr2  r -> emit_byte state (0x83 lor (index_register r lsl 5))
  | IA_Label(n, l) ->
      let cur_pos = Buffer.length state.code in
      emit_byte state 0x8D;
      emit_word_patch state l (fun lbl_pos -> lbl_pos + n - cur_pos - 3)
  | IA_PCOff n when n >= -128 && n <= 127 ->
      emit_byte state 0x8C;
      emit_byte state n
  | IA_PCOff n ->
      emit_byte state 0x8D;
      emit_word state n
  | IA_IndirIndex(0, r) -> emit_byte state (0x94 lor (index_register r lsl 5))
  | IA_IndirIndex(n, r) when n >= -128 && n <= 127 ->
      emit_byte state (0x98 lor (index_register r lsl 5));
      emit_byte state n
  | IA_IndirIndex(n, r) ->
      emit_byte state (0x99 lor (index_register r lsl 5));
      emit_word state n
  | IA_IndirIndexA r -> emit_byte state (0x96 lor (index_register r lsl 5))
  | IA_IndirIndexB r -> emit_byte state (0x95 lor (index_register r lsl 5))
  | IA_IndirIndexD r -> emit_byte state (0x9B lor (index_register r lsl 5))
  | IA_IndirIncr2  r -> emit_byte state (0x91 lor (index_register r lsl 5))
  | IA_IndirDecr2  r -> emit_byte state (0x93 lor (index_register r lsl 5))
  | IA_IndirLabel(n, l) ->
      let cur_pos = Buffer.length state.code in
      emit_byte state 0x9D;
      emit_word_patch state l (fun lbl_pos -> lbl_pos + n - cur_pos - 3)
  | IA_IndirPCOff n when n >= -128 && n <= 127 ->
      emit_byte state 0x9C;
      emit_byte state n
  | IA_IndirPCOff n ->
      emit_byte state 0x9D;
      emit_word state n
  | IA_IndirExtended n ->
      emit_byte state 0x9F;
      emit_word state n

let addr state opd opi ope (arg : MPU6809.addr) =
  match arg with
  | ADirect   n -> emit_byte state opd; emit_byte state n
  | AIndexed  a -> emit_byte state opi; indexed_address state a
  | AExtended n -> emit_byte state ope; emit_word state n

let addr_imm8 state op opd opi ope (arg : MPU6809.addr_imm8) =
  match arg with
  | Imm8  b -> emit_byte state op; emit_byte state b
  | Addr8 a -> addr state opd opi ope a

let addr_imm16 state op opd opi ope (arg : MPU6809.addr_imm16) =
  match arg with
  | Imm16  w -> emit_byte state op; emit_word state w
  | Addr16 a -> addr state opd opi ope a

let rel8 state (r : MPU6809.rel8) =
  match r with
  | R8Label l ->
      let cur_pos = Buffer.length state.code in
      emit_byte_patch state l (fun lbl_pos -> lbl_pos - cur_pos - 1)
  | R8Imm n -> emit_byte state n


let rel16 state (r : MPU6809.rel16) =
  match r with
  | R16Label l ->
      let cur_pos = Buffer.length state.code in
      emit_word_patch state l (fun lbl_pos -> lbl_pos - cur_pos - 2)
  | R16Imm n -> emit_word state n

let gen_code state (instr : MPU6809.instr) =
  match instr with
  | Label name ->
      Hashtbl.add state.symbols name (Buffer.length state.code)
  | Asciiz str ->
      Buffer.add_string state.code str;
      emit_byte state 0
  | ABX     -> emit_byte  state 0x3A
  | ADCA  x -> addr_imm8  state 0x89 0x99 0xA9 0xB9 x
  | ADCB  x -> addr_imm8  state 0xC9 0xD9 0xE9 0xF9 x
  | ADDA  x -> addr_imm8  state 0x8B 0x9B 0xAB 0xBB x
  | ADDB  x -> addr_imm8  state 0xCB 0xDB 0xEB 0xFB x
  | ADDD  x -> addr_imm16 state 0xC3 0xD3 0xE3 0xF3 x
  | ANDA  x -> addr_imm8  state 0x84 0x94 0xA4 0xB4 x
  | ANDB  x -> addr_imm8  state 0xC4 0xD4 0xE4 0xF4 x
  | ANDCC n -> emit_byte state 0x1C; emit_byte state n
  | ASLA    -> emit_byte state 0x48
  | ASLB    -> emit_byte state 0x58
  | ASL a   -> addr state 0x08 0x68 0x78 a
  | ASRA    -> emit_byte state 0x47
  | ASRB    -> emit_byte state 0x57
  | ASR a   -> addr state 0x07 0x67 0x77 a
  | BCC r   -> emit_byte state 0x24; rel8 state r
  | BCS r   -> emit_byte state 0x25; rel8 state r
  | BEQ r   -> emit_byte state 0x27; rel8 state r
  | BGE r   -> emit_byte state 0x2C; rel8 state r
  | BGT r   -> emit_byte state 0x2E; rel8 state r
  | BHI r   -> emit_byte state 0x22; rel8 state r
  | BHS r   -> emit_byte state 0x24; rel8 state r
  | BITA x  -> addr_imm8  state 0x85 0x95 0xA5 0xB5 x
  | BITB x  -> addr_imm8  state 0xC5 0xD5 0xE5 0xF5 x
  | BLE r   -> emit_byte state 0x2F; rel8 state r
  | BLO r   -> emit_byte state 0x25; rel8 state r
  | BLS r   -> emit_byte state 0x23; rel8 state r
  | BLT r   -> emit_byte state 0x2D; rel8 state r
  | BMI r   -> emit_byte state 0x2B; rel8 state r
  | BNE r   -> emit_byte state 0x26; rel8 state r
  | BPL r   -> emit_byte state 0x2A; rel8 state r
  | BRA r   -> emit_byte state 0x20; rel8 state r
  | BRN r   -> emit_byte state 0x21; rel8 state r
  | BSR r   -> emit_byte state 0x8D; rel8 state r
  | BVC r   -> emit_byte state 0x28; rel8 state r
  | BVS r   -> emit_byte state 0x29; rel8 state r
  | CLRA    -> emit_byte state 0x4F
  | CLRB    -> emit_byte state 0x5F
  | CLR a   -> addr state 0x0F 0x6F 0x7F a
  | CMPA x  -> addr_imm8  state 0x81 0x91 0xA1 0xB1 x
  | CMPB x  -> addr_imm8  state 0xC1 0xD1 0xE1 0xF1 x
  | CMPD x  -> emit_byte state 0x10; addr_imm16 state 0x83 0x93 0xA3 0xB3 x
  | CMPS x  -> emit_byte state 0x11; addr_imm16 state 0x8C 0x9C 0xAC 0xBC x
  | CMPU x  -> emit_byte state 0x11; addr_imm16 state 0x83 0x93 0xA3 0xB3 x
  | CMPX x  -> addr_imm16 state 0x8C 0x9C 0xAC 0xBC x
  | CMPY x  -> emit_byte state 0x10; addr_imm16 state 0x8C 0x9C 0xAC 0xBC x
  | COMA    -> emit_byte state 0x43
  | COMB    -> emit_byte state 0x53
  | COM a   -> addr state 0x03 0x63 0x73 a
  | CWAI n  -> emit_byte state 0x3C; emit_byte state n
  | DAA     -> emit_byte state 0x19
  | DECA    -> emit_byte state 0x4A
  | DECB    -> emit_byte state 0x5A
  | DEC a   -> addr state 0x0A 0x6A 0x7A a
  | EORA x  -> addr_imm8  state 0x88 0x98 0xA8 0xB8 x
  | EORB x  -> addr_imm8  state 0xC8 0xD8 0xE8 0xF8 x
  | EXG(r0, r1) ->
      emit_byte state 0x1E;
      emit_byte state (reg2_byte r0 r1)
  | INCA    -> emit_byte state 0x4C
  | INCB    -> emit_byte state 0x5C
  | INC a   -> addr state 0x0C 0x6C 0x7C a
  | JMP a   -> addr state 0x0E 0x6E 0x7E a
  | JSR a   -> addr state 0x9D 0xAD 0xBD a
  | LBCC r  -> emit_word state 0x1024; rel16 state r
  | LBCS r  -> emit_word state 0x1025; rel16 state r
  | LBEQ r  -> emit_word state 0x1027; rel16 state r
  | LBGE r  -> emit_word state 0x102C; rel16 state r
  | LBGT r  -> emit_word state 0x102E; rel16 state r
  | LBHI r  -> emit_word state 0x1022; rel16 state r
  | LBHS r  -> emit_word state 0x1024; rel16 state r
  | LBLE r  -> emit_word state 0x102F; rel16 state r
  | LBLO r  -> emit_word state 0x1025; rel16 state r
  | LBLS r  -> emit_word state 0x1023; rel16 state r
  | LBLT r  -> emit_word state 0x102D; rel16 state r
  | LBMI r  -> emit_word state 0x102B; rel16 state r
  | LBNE r  -> emit_word state 0x1026; rel16 state r
  | LBPL r  -> emit_word state 0x102A; rel16 state r
  | LBRA r  -> emit_byte state 0x16; rel16 state r
  | LBRN r  -> emit_word state 0x1021; rel16 state r
  | LBSR r  -> emit_byte state 0x17; rel16 state r
  | LBVC r  -> emit_word state 0x1028; rel16 state r
  | LBVS r  -> emit_word state 0x1029; rel16 state r
  | LDA  x  -> addr_imm8  state 0x86 0x96 0xA6 0xB6 x
  | LDB  x  -> addr_imm8  state 0xC6 0xD6 0xE6 0xF6 x
  | LDD  x  -> addr_imm16 state 0xCC 0xDC 0xEC 0xFC x
  | LDS  x  -> emit_byte state 0x10; addr_imm16 state 0xCE 0xDE 0xEE 0xFE x
  | LDU  x  -> addr_imm16 state 0xCE 0xDE 0xEE 0xFE x
  | LDX  x  -> addr_imm16 state 0x8E 0x9E 0xAE 0xBE x
  | LDY  x  -> emit_byte state 0x10; addr_imm16 state 0x8E 0x9E 0xAE 0xBE x
  | LEAS a  -> emit_byte state 0x32; indexed_address state a
  | LEAU a  -> emit_byte state 0x33; indexed_address state a
  | LEAX a  -> emit_byte state 0x30; indexed_address state a
  | LEAY a  -> emit_byte state 0x31; indexed_address state a
  | LSLA    -> emit_byte state 0x48
  | LSLB    -> emit_byte state 0x58
  | LSL  a  -> addr state 0x08 0x68 0x78 a
  | LSRA    -> emit_byte state 0x44
  | LSRB    -> emit_byte state 0x54
  | LSR  a  -> addr state 0x04 0x64 0x74 a
  | MUL     -> emit_byte state 0x3D
  | NEGA    -> emit_byte state 0x40
  | NEGB    -> emit_byte state 0x50
  | NEG  a  -> addr state 0x00 0x60 0x70 a
  | NOP     -> emit_byte state 0x12
  | ORA  x  -> addr_imm8  state 0x8A 0x9A 0xAA 0xBA x
  | ORB  x  -> addr_imm8  state 0xCA 0xDA 0xEA 0xFA x
  | ORCC n  -> emit_byte state 0x1A; emit_byte state n
  | PSHS rs -> emit_byte state 0x34; emit_byte state (register_stack_mask rs)
  | PSHU rs -> emit_byte state 0x36; emit_byte state (register_stack_mask rs)
  | PULS rs -> emit_byte state 0x35; emit_byte state (register_stack_mask rs)
  | PULU rs -> emit_byte state 0x37; emit_byte state (register_stack_mask rs)
  | ROLA    -> emit_byte state 0x49
  | ROLB    -> emit_byte state 0x59
  | ROL  a  -> addr state 0x09 0x69 0x79 a
  | RORA    -> emit_byte state 0x46
  | RORB    -> emit_byte state 0x56
  | ROR  a  -> addr state 0x06 0x66 0x76 a
  | RTI     -> emit_byte state 0x3B
  | RTS     -> emit_byte state 0x39
  | SBCA x  -> addr_imm8  state 0x82 0x92 0xA2 0xB2 x
  | SBCB x  -> addr_imm8  state 0xC2 0xD2 0xE2 0xF2 x
  | SEX     -> emit_byte state 0x1D
  | STA  a  -> addr state 0x97 0xA7 0xB7 a
  | STB  a  -> addr state 0xD7 0xE7 0xF7 a
  | STD  a  -> addr state 0xDD 0xED 0xFD a
  | STS  a  -> emit_byte state 0x10; addr state 0xDF 0xEF 0xFF a
  | STU  a  -> addr state 0xDF 0xEF 0xFF a
  | STX  a  -> addr state 0x9F 0xAF 0xBF a
  | STY  a  -> emit_byte state 0x10; addr state 0x9F 0xAF 0xBF a
  | SUBA x  -> addr_imm8  state 0x80 0x90 0xA0 0xB0 x
  | SUBB x  -> addr_imm8  state 0xC0 0xD0 0xE0 0xF0 x
  | SUBD x  -> addr_imm16 state 0x83 0x93 0xA3 0xB3 x
  | SWI     -> emit_byte state 0x3F
  | SWI2    -> emit_word state 0x103F
  | SWI3    -> emit_word state 0x113F
  | SYNC    -> emit_byte state 0x13
  | TFR(r0, r1) ->
      emit_byte state 0x1F;
      emit_byte state (reg2_byte r0 r1)
  | TSTA    -> emit_byte state 0x4D
  | TSTB    -> emit_byte state 0x5D
  | TST a   -> addr state 0x0D 0x6D 0x7D a

let patch_code code (offset, get_value) =
  Bytes.set code offset (Char.chr (get_value () lor 0xFF))

let assemble instrs =
  let state = {
      code    = Buffer.create 1024;
      symbols = Hashtbl.create 32;
      patches = [];
    } in
  List.iter (gen_code state) instrs;
  let code = Buffer.to_bytes state.code in
  List.iter (patch_code code) state.patches;
  code
