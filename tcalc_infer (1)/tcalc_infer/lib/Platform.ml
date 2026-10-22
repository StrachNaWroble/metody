let sys_HALT   = 0
let sys_ALLOC  = 1
let sys_MUL    = 2
let sys_DIV    = 3
let sys_PUTC   = 8
let sys_PUTSTR = 9
let sys_PUTNUM = 10
let sys_GETC   = 12
let sys_GETSTR = 13
let sys_GETNUM = 14

let epilog = MPU6809.[
  STD (a_decr2 IxU);
  LDB (Imm8 sys_PUTNUM);
  SWI2;
  LDD (Imm16 (0x0A00 lor sys_PUTC));
  SWI2;
  CLRB;
  SWI2 ]
