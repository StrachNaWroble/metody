type label = string

type condition =
  | MC_1  | MC_0  | MC_Z  | MC_NZ
  | MC_P  | MC_NP | MC_N  | MC_NN
  | MC_EQ | MC_NE
  | MC_LT | MC_GE | MC_LE | MC_GT

type instr =
  | MLabel    of label
  | MInstr    of MPU6809.instr
  | MConst    of int
  | MGetLabel of label
  | MGetLocal of int
  | MSetLocal of int
  | MPush
  | MPopN     of int
  | MPopAcc
  | MNot
  | MNeg
  | MAnd
  | MOr
  | MAdd
  | MSub
  | MMul
  | MDiv
  | MMod
  | MAlloc    of int
  | MGet      of int
  | MSet      of int
  | MBranch   of condition * label
  | MJump     of label
  | MJumpAcc
  | MCall     of label
  | MCallAcc
  | MRet

let string_of_condition cond =
  match cond with
  | MC_1  -> "1 "
  | MC_0  -> "0 "
  | MC_Z  -> "Z "
  | MC_NZ -> "NZ"
  | MC_P  -> "P "
  | MC_NP -> "NP"
  | MC_N  -> "N "
  | MC_NN -> "NN"
  | MC_EQ -> "EQ"
  | MC_NE -> "NE"
  | MC_LT -> "LT"
  | MC_GE -> "GE"
  | MC_LE -> "LE"
  | MC_GT -> "GT"

let string_of_instr instr =
  match instr with
  | MLabel    l   -> l
  | MInstr    i   -> MPU6809.string_of_instr i
  | MConst    n   -> Printf.sprintf "    MConst    %d" n
  | MGetLabel l   -> Printf.sprintf "    MGetLabel %s" l
  | MGetLocal n   -> Printf.sprintf "    MGetLocal %d" n
  | MSetLocal n   -> Printf.sprintf "    MSetLocal %d" n
  | MPush         -> "    MPush"
  | MPopN     n   -> Printf.sprintf "    MPopN     %d" n
  | MPopAcc       -> "    MPopAcc"
  | MNot          -> "    MNot"
  | MNeg          -> "    MNeg"
  | MAnd          -> "    MAnd"
  | MOr           -> "    MOr"
  | MAdd          -> "    MAdd"
  | MSub          -> "    MSub"
  | MMul          -> "    MMul"
  | MDiv          -> "    MDiv"
  | MMod          -> "    MMod"
  | MAlloc    n   -> Printf.sprintf "    MAlloc    %d" n
  | MGet      n   -> Printf.sprintf "    MGet      %d" n
  | MSet      n   -> Printf.sprintf "    MSet      %d" n
  | MBranch(c, l) ->
      Printf.sprintf "    MBranch   %s  %s" (string_of_condition c) l
  | MJump     l   -> Printf.sprintf "    MJump     %s" l
  | MJumpAcc      -> "    MJumpAcc"
  | MCall     l   -> Printf.sprintf "    MCall     %s" l
  | MCallAcc      -> "    MCallAcc"
  | MRet          -> "    MRet"

let asm_of_instr instr =
  let open MPU6809 in
  match instr with
  | MLabel    l -> [ Label l ]
  | MInstr    i -> [ i ]
  | MConst    n -> [ LDD(Imm16 n)]
  | MGetLabel l -> [ LEAX(IA_Label(0, l)); TFR(RegX, RegD) ]
  | MGetLocal n -> [ LDD(a16_index (2*n) IxU) ]
  | MSetLocal n -> [ STD(a_index   (2*n) IxU) ]
  | MPush       -> [ STD(a_decr2   IxU) ]
  | MPopN n     -> [ LEAU(IA_Index(2*n, IxU)) ]
  | MPopAcc     -> [ LDD(a16_incr2 IxU) ]
  | MNot        -> [ COMA; COMB ]
  | MNeg        -> [ COMA; COMB; ADDD(Imm16 1) ]
  | MAnd        -> [ ANDA(a8_incr1 IxU); ANDB(a8_incr1 IxU) ]
  | MOr         -> [ ORA(a8_incr1 IxU); ORB(a8_incr1 IxU) ]
  | MAdd        -> [ ADDD(a16_incr2 IxU) ]
  | MSub        ->
      [ STD(a_decr2 IxS); LDD(a16_incr2 IxU); SUBD(a16_incr2 IxS) ]
  | MMul        ->
      [ STD(a_decr2 IxU); LDB(Imm8 Platform.sys_MUL); SWI2 ]
  | MDiv        ->
      [ STD(a_decr2 IxU); LDB(Imm8 Platform.sys_DIV);
        SWI2; LEAU(IA_Index(2, IxU)) ]
  | MMod        ->
      [ STD(a_decr2 IxU); LDB(Imm8 Platform.sys_DIV);
        SWI2; LDD(a16_incr2 IxU) ]
  | MAlloc n    -> [ LDD(Imm16 (n * 256 + Platform.sys_ALLOC)); SWI2 ]
  | MGet   n    -> [ TFR(RegD, RegX); LDD(a16_index (2*n) IxX) ]
  | MSet   n    -> [ LDX(a16_index 0 IxU); STD(a_index (2*n) IxX) ]
  | MBranch(MC_1, l)  -> [ LBRA(R16Label l) ]
  | MBranch(MC_0, _)  -> []
  | MBranch(MC_Z, l)  -> [ CMPD(Imm16 0); LBEQ(R16Label l) ]
  | MBranch(MC_NZ, l) -> [ CMPD(Imm16 0); LBNE(R16Label l) ]
  | MBranch(MC_P,  l) -> [ CMPD(Imm16 0); LBGT(R16Label l) ]
  | MBranch(MC_NP, l) -> [ CMPD(Imm16 0); LBLE(R16Label l) ]
  | MBranch(MC_N,  l) -> [ CMPD(Imm16 0); LBLT(R16Label l) ]
  | MBranch(MC_NN, l) -> [ CMPD(Imm16 0); LBGE(R16Label l) ]
  | MBranch(MC_EQ, l) -> [ CMPD(a16_incr2 IxU); LBEQ(R16Label l) ]
  | MBranch(MC_NE, l) -> [ CMPD(a16_incr2 IxU); LBNE(R16Label l) ]
  | MBranch(MC_LT, l) -> [ CMPD(a16_incr2 IxU); LBGT(R16Label l) ]
  | MBranch(MC_GE, l) -> [ CMPD(a16_incr2 IxU); LBLE(R16Label l) ]
  | MBranch(MC_LE, l) -> [ CMPD(a16_incr2 IxU); LBGE(R16Label l) ]
  | MBranch(MC_GT, l) -> [ CMPD(a16_incr2 IxU); LBLT(R16Label l) ]
  | MJump l           -> [ LBRA(R16Label l) ]
  | MJumpAcc          -> [ TFR(RegD, RegX); JMP(a_index 0 IxX) ]
  | MCall l           -> [ LBSR(R16Label l) ]
  | MCallAcc          -> [ TFR(RegD, RegX); JSR(a_index 0 IxX) ]
  | MRet              -> [ RTS ]

let asm_of_instrs instrs =
  List.concat_map asm_of_instr instrs
