type instr =
  | MConst    of int
  | MGetLocal of int
  | MPush
  | MPopN     of int
  | MAdd
  | MSub
  | MMul
  | MDiv

type prog = instr list
