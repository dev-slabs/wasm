module Slabs.Wasm.Binary

imports 
  Universum
  Slabs.Wasm.Structure

defbin 
  NumType
    0x7F -> I32
    0x7E -> I64
    0x7D -> F32
    0x7C -> F64
  
  RefType
    0x70 -> FuncRef
    0x6F -> ExternRef
  
  ValType
    t:NumType -> NumType t
    t:RefType -> RefType t
  
  ResultType
    t:Vec(ValType) -> t
  
  FuncType
    0x60 rt1:ResultType rt2:ResultType -> FuncType rt1 rt2
  
  Limits
    0x00 n:U32 -> Limits n Nothing
    0x01 n:U32 m:U32 -> Limits n (Just m)
  
  MemType
    lim:Limits -> lim
  
  TableType
    et:RefType lim:Limits -> TableType lim et
  
  GlobalType
    t:ValType m:Mut -> GlobalType m t
  
  Mut
    0x00 -> MutConst
    0x01 -> MutVar

  ; 5.4.1
  BlockType
    0x40 -> BlockEmptyType
    t:ValType -> ValType t
    x:U32 -> TypeIdx x
  
  ElseInstrs: [Instr]
    0x0B -> []
    0x05 ins:*(Instr) 0x0B -> ins

  CtlInstr
    0x00              -> Unreachable
    0x01              -> Nop
    0x02 bt:BlockType ins:*(Instr) 0x0B    -> Block bt ins
    0x03 bt:BlockType ins:*(Instr) 0x0B    -> Loop bt ins
    0x04 bt:BlockType in1:*(Instr) in2:ElseInstrs -> If bt in1 in2
    0x0C l:LabelIdx   -> Br l
    0x0D l:LabelIdx   -> BrIf l
    0x0E ls:Vec(LabelIdx) ln:LabelIdx   -> BrTable ls ln
    0x0F  -> Return
    0x10 x:FuncIdx  -> Call x
    0x11 y:TypeIdx x:TableIdx -> CallIndirect x y

  RefInstr
    0xD0 t:RefType  -> RefNull t
    0xD1            -> RefIsNull
    0xD2 x:FuncIdx  -> RefFunc x

  ParamInstr
    0x1A    -> Drop
    0x1B    -> Select []
    0x1C ts:Vec(ValType) -> Select ts
  
  VarInstr
    0x20 x:LocalIdx -> LocalGet x
    0x21 x:LocalIdx -> LocalSet x
    0x22 x:LocalIdx -> LocalTee x
    0x23 x:GlobalIdx -> GlobalGet x
    0x24 x:GlobalIdx -> GlobalSet x

  TableInstr
    0x25 x:TableIdx -> TableGet x
    0x26 x:TableIdx -> TableSet x
  
  TableInstr'
    12:U32 y:ElemIdx x:TableIdx -> TableInit x y
    13:U32 x:ElemIdx -> ElemDrop x
    14:U32 x:TableIdx y:TableIdx -> TableCopy x y
    15:U32 x:TableIdx -> TableGrow x
    16:U32 x:TableIdx -> TableSize x
    17:U32 x:TableIdx -> TableFill x
  
  MemArg
    a:U32 b:U32 -> MemArg a b
  
  MemoryInstr
    0x28 m:MemArg -> I32Load m
    0x29 m:MemArg -> I64Load m
    0x2A m:MemArg -> F32Load m
    0x2B m:MemArg -> F64Load m
    0x2C m:MemArg -> I32Load8S m
    0x2D m:MemArg -> I32Load8U m
    0x2E m:MemArg -> I32Load16S m
    0x2F m:MemArg -> I32Load16U m
    0x30 m:MemArg -> I64Load8S m
    0x31 m:MemArg -> I64Load8U m
    0x32 m:MemArg -> I64Load16S m
    0x33 m:MemArg -> I64Load16U m
    0x34 m:MemArg -> I64Load32S m
    0x35 m:MemArg -> I64Load32U m
    0x36 m:MemArg -> I32Store m
    0x37 m:MemArg -> I64Store m
    0x38 m:MemArg -> F32Store m
    0x39 m:MemArg -> F64Store m
    0x3A m:MemArg -> I32Store8 m
    0x3B m:MemArg -> I32Store16 m
    0x3C m:MemArg -> I64Store8 m
    0x3D m:MemArg -> I64Store16 m
    0x3E m:MemArg -> I64Store32 m
    0x3F 0x00 -> MemorySize
    0x40 0x00 -> MemoryGrow
  
  MemoryInstr'
    8:U32 x:DataIdx 0x00 -> MemoryInit x
    9:U32 x:DataIdx -> DataDrop x
    10:U32 0x00 0x00 -> MemoryCopy
    11:U32 0x00 -> MemoryFill

  ; TODO
  NumInstr
    0x41 n:U32 -> I32Const n
    0x42 n:U64 -> I64Const n


  Instr
    i:MemoryInstr -> MemoryInstr i
    i:CtlInstr -> CtlInstr i
    i:RefInstr -> RefInstr i
    0xFC i:Instr' -> i

  Instr': Instr
    i:NumInstr' -> NumInstr' i
    i:TableInstr' -> TableInstr' i
    i:MemoryInstr' -> MemoryInstr' i
    