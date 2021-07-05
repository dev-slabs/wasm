module Slabs.Wasm.Binary

import Control.Applicative
imports 
  Slabs.Wasm.Structure
  Slabs.Wasm.Internal

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

  MemArg
    a:U32 b:U32 -> MemArg a b
  
  Instr
    ; CtlInstr
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

    ; ParamInstr
    0x1A    -> Drop
    0x1B    -> Select []
    0x1C ts:Vec(ValType) -> Select ts
  
    ; VarInstr
    0x20 x:LocalIdx -> LocalGet x
    0x21 x:LocalIdx -> LocalSet x
    0x22 x:LocalIdx -> LocalTee x
    0x23 x:GlobalIdx -> GlobalGet x
    0x24 x:GlobalIdx -> GlobalSet x

    ; TableInstr
    0x25 x:TableIdx -> TableGet x
    0x26 x:TableIdx -> TableSet x
  
    ; MemoryInstr
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
  
    ; MemoryInstr'
    0xFC 8:U32 x:DataIdx 0x00 -> MemoryInit x
    0xFC 9:U32 x:DataIdx -> DataDrop x
    0xFC 10:U32 0x00 0x00 -> MemoryCopy
    0xFC 11:U32 0x00 -> MemoryFill

    ; NumInstr
    0x41 n:I32 -> I32Const n
    0x42 n:I64 -> I64Const n
    0x43 z:F32 -> F32Const z
    0x44 z:F64 -> F64Const z

    0x45 -> I32Eqz
    0x46 -> I32RelOp IEq
    0x47 -> I32RelOp INe
    0x48 -> I32RelOp ILtS
    0x49 -> I32RelOp ILtU
    0x4A -> I32RelOp IGtS
    0x4B -> I32RelOp IGtU
    0x4C -> I32RelOp ILeS
    0x4D -> I32RelOp ILeU
    0x4E -> I32RelOp IGeS
    0x4F -> I32RelOp IGeU

    0x50 -> I64Eqz
    0x51 -> I64RelOp IEq
    0x52 -> I64RelOp INe
    0x53 -> I64RelOp ILtS
    0x54 -> I64RelOp ILtU
    0x55 -> I64RelOp IGtS
    0x56 -> I64RelOp IGtU
    0x57 -> I64RelOp ILeS
    0x58 -> I64RelOp ILeU
    0x59 -> I64RelOp IGeS
    0x5A -> I64RelOp IGeU

    0x5B -> F32RelOp FEq
    0x5C -> F32RelOp FNe
    0x5D -> F32RelOp FLt
    0x5E -> F32RelOp FGt
    0x5F -> F32RelOp FLe
    0x60 -> F32RelOp FGe

    0x61 -> F64RelOp FEq
    0x62 -> F64RelOp FNe
    0x63 -> F64RelOp FLt
    0x64 -> F64RelOp FGt
    0x65 -> F64RelOp FLe
    0x66 -> F64RelOp FGe

    0x67 -> I32UnOp IClz 
    0x68 -> I32UnOp ICtz 
    0x69 -> I32UnOp IPopcnt 
    0x6A -> I32BinOp IAdd 
    0x6B -> I32BinOp ISub 
    0x6C -> I32BinOp IMul 
    0x6D -> I32BinOp IDivS 
    0x6E -> I32BinOp IDivU 
    0x6F -> I32BinOp IRemS 
    0x70 -> I32BinOp IRemU 
    0x71 -> I32BinOp IAnd 
    0x72 -> I32BinOp IOr 
    0x73 -> I32BinOp IXor 
    0x74 -> I32BinOp IShl 
    0x75 -> I32BinOp IShrS 
    0x76 -> I32BinOp IShrU 
    0x77 -> I32BinOp IRotl 
    0x78 -> I32BinOp IRotr
  
    0x79 -> I64UnOp IClz
    0x7A -> I64UnOp ICtz
    0x7B -> I64UnOp IPopcnt
    0x7C -> I64BinOp IAdd
    0x7D -> I64BinOp ISub
    0x7E -> I64BinOp IMul
    0x7F -> I64BinOp IDivS
    0x80 -> I64BinOp IDivU
    0x81 -> I64BinOp IRemS 
    0x82 -> I64BinOp IRemU 
    0x83 -> I64BinOp IAnd
    0x84 -> I64BinOp IOr
    0x85 -> I64BinOp IXor
    0x86 -> I64BinOp IShl
    0x87 -> I64BinOp IShrS
    0x88 -> I64BinOp IShrU
    0x89 -> I64BinOp IRotl
    0x8A -> I64BinOp IRotr

    0x8B -> F32UnOp FAbs
    0x8C -> F32UnOp FNeg
    0x8D -> F32UnOp FCeil
    0x8E -> F32UnOp FFloor
    0x8F -> F32UnOp FTrunc
    0x90 -> F32UnOp FNearest 
    0x91 -> F32UnOp FSqrt
    0x92 -> F32BinOp FAdd
    0x93 -> F32BinOp FSub
    0x94 -> F32BinOp FMul
    0x95 -> F32BinOp FDiv
    0x96 -> F32BinOp FMin
    0x97 -> F32BinOp FMax
    0x98 -> F32BinOp FCopysign

    0x99 -> F64UnOp FAbs
    0x9A -> F64UnOp FNeg
    0x9B -> F64UnOp FCeil
    0x9C -> F64UnOp FFloor
    0x9D -> F64UnOp FTrunc
    0x9E -> F64UnOp FNearest 
    0x9F -> F64UnOp FSqrt
    0xA0 -> F64BinOp FAdd
    0xA1 -> F64BinOp FSub
    0xA2 -> F64BinOp FMul
    0xA3 -> F64BinOp FDiv
    0xA4 -> F64BinOp FMin
    0xA5 -> F64BinOp FMax
    0xA6 -> F64BinOp FCopysign

    0xA7 -> I32WrapI64
    0xA8 -> I32TruncF32S
    0xA9 -> I32TruncF32U
    0xAA -> I32TruncF64S
    0xAB -> I32TruncF64U
    0xAC -> I64ExtendI32S
    0xAD -> I64ExtendI32U
    0xAE -> I64TruncF32S
    0xAF -> I64TruncF32U
    0xB0 -> I64TruncF64S
    0xB1 -> I64TruncF64U
    0xB2 -> F32ConvertI32S 
    0xB3 -> F32ConvertI32U 
    0xB4 -> F32ConvertI64S 
    0xB5 -> F32ConvertI64U 
    0xB6 -> F32DemoteF64
    0xB7 -> F64ConvertI32S 
    0xB8 -> F64ConvertI32U 
    0xB9 -> F64ConvertI64S 
    0xBA -> F64ConvertI64U 
    0xBB -> F64PromoteF32
    0xBC -> I32ReinterpretF32 
    0xBD -> I64ReinterpretF64 
    0xBE -> F32ReinterpretI32 
    0xBF -> F64ReinterpretI64
    0xC0 -> I32Extend8S 
    0xC1 -> I32Extend16S 
    0xC2 -> I64Extend8S 
    0xC3 -> I64Extend16S 
    0xC4 -> I64Extend32S

    ; RefInstr
    0xD0 t:RefType  -> RefNull t
    0xD1            -> RefIsNull
    0xD2 x:FuncIdx  -> RefFunc x

    ; NumInstr'
    0xFC 0:U32 -> I32TruncSatF32S 
    0xFC 1:U32 -> I32TruncSatF32U 
    0xFC 2:U32 -> I32TruncSatF64S 
    0xFC 3:U32 -> I32TruncSatF64U 
    0xFC 4:U32 -> I64TruncSatF32S 
    0xFC 5:U32 -> I64TruncSatF32U 
    0xFC 6:U32 -> I64TruncSatF64S 
    0xFC 7:U32 -> I64TruncSatF64U

    ; TableInstr'
    0xFC 12:U32 y:ElemIdx x:TableIdx -> TableInit x y
    0xFC 13:U32 x:ElemIdx -> ElemDrop x
    0xFC 14:U32 x:TableIdx y:TableIdx -> TableCopy x y
    0xFC 15:U32 x:TableIdx -> TableGrow x
    0xFC 16:U32 x:TableIdx -> TableSize x
    0xFC 17:U32 x:TableIdx -> TableFill x
  
  Expr
    ins:*(Instr) 0x0B -> Expr ins
  
  ; modules
  TypeIdx
    i:U32 -> i
  
  FuncIdx
    i:U32 -> i
  
  TableIdx
    i:U32 -> i
  
  MemIdx
    i:U32 -> i
  
  GlobalIdx
    i:U32 -> i
  
  ElemIdx
    i:U32 -> i
  
  DataIdx
    i:U32 -> i
  
  LocalIdx
    i:U32 -> i
  
  LabelIdx
    i:U32 -> i

  Magic:()
    0x00 0x61 0x73 0x6D
  
  Version:()
    0x01 0x00 0x00 0x00

  TypeSec: [FuncType]
    types:VecSection(1 FuncType) -> types
  
  ImportSec: [Import]
    imports:VecSection(2 Import) -> imports
  
  Import
    mod:Name nm:Name d:ImportDesc -> Import mod nm d

  ImportDesc
    0x00 x:TypeIdx -> ImportFunc x
    0x01 tt:TableType -> ImportTable tt
    0x02 mt:MemType -> ImportMemory mt
    0x03 gt:GlobalType -> ImportGlobal gt
  
  FuncSec: [TypeIdx]
    x:VecSection(3 TypeIdx) -> x
  
  TableSec: [Table]
    tabs:VecSection(4 Table) -> tabs
  
  Table
    tt:TableType -> Table tt
  
  MemSec: [Mem]
    mems:VecSection(5 Mem) -> mems

  Mem
    mt:MemType -> Mem mt

  GlobalSec: [Global]
    globs:VecSection(6 Global) -> globs
  
  Global
    gt:GlobalType e:Expr -> Global gt e
  
  ExportSec: [Export]
    exs:VecSection(7 Export) -> exs
  
  Export
    nm:Name d:ExportDesc -> Export nm d

  ExportDesc
    0x00 x:FuncIdx -> ExportFunc x
    0x01 x:TableIdx -> ExportTable x
    0x02 x:MemIdx -> ExportMem x
    0x03 x:GlobalIdx -> ExportGlobal x
  
  StartSec: Start
    8 x:Section(FuncIdx) -> StartAt x
    -> NoStart

  ElemSec: [Elem]
    segs:VecSection(9 Elem) -> segs
  
  CodeSec: [Code]
    codes:VecSection(10 Embed(Code)) -> codes
  
  Code
    ts:Vec(Locals) e:Expr -> Code ts e
  
  Locals
    vts:Vec(ValType) -> vts
  
  DataSec: [Data]
    datas:VecSection(11 Data) -> datas
  
  Data
    0x00 e:Expr bs:Bytes -> Data bs (DActive 0 e)
    0x01 bs:Bytes -> Data bs DPassive
    0x02 x:MemIdx e:Expr bs:Bytes -> Data bs (DActive x e)

  DatacountSec: Datacount
    12 x:Section(U32) -> Datacount x
    -> NoDatacount

  Elem
    0x00 e:Expr ys:Vec(FuncIdx) -> Elem FuncRef (RefFuncs ys) (EActive 0 e)
    0x01 0x00 ys:Vec(FuncIdx) -> Elem FuncRef (RefFuncs ys) EPassive
    0x02 x:TableIdx e:Expr 0x00 ys:Vec(FuncIdx) -> Elem FuncRef (RefFuncs ys) (EActive x e)
    0x03 0x00 ys:Vec(FuncIdx) -> Elem FuncRef (RefFuncs ys) EDeclarative
    0x04 e:Expr els:Vec(Expr) -> Elem FuncRef els (EActive 0 e)
    0x05 et:RefType els:Vec(Expr) -> Elem et els EPassive
    0x06 x:TableIdx e:Expr et:RefType els:Vec(Expr) -> Elem et els (EActive x e)
    0x07 et:RefType els:Vec(Expr) -> Elem et els EDeclarative


  Module
    Magic Version 
      . types:TypeSec imports:ImportSec typeidxs:FuncSec tables:TableSec 
      . mems:MemSec globals:GlobalSec exports:ExportSec start:StartSec
      . elems:ElemSec m:DatacountSec codes:CodeSec datas:DataSec
      . -> Module types typeidxs codes tables mems globals elems m datas start imports exports
