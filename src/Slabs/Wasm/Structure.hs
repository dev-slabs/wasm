{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Slabs.Wasm.Structure  where

import Universum
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Z.Data.Vector (Bytes)
import qualified Z.Data.Text as T

type U32 = Word32
type Name = T.Text

-- 2.3 Types
data NumType = I32 | I64 | F32 | F64
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data RefType = FuncRef | ExternRef
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data ValType = NumType NumType | RefType RefType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

type ResultType = [ValType]

data FuncType = FuncType ResultType ResultType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Limits = Limits U32 (Maybe U32)
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

type MemType = Limits

data TableType = TableType Limits RefType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Mut = MutConst | MutVar
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data GlobalType = GlobalType Mut ValType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data ExternType
    = EFunc FuncType
    | ETable TableType
    | EMem MemType
    | EGlobal GlobalType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4 Instructinos
-- 2.4.1 Numeric Instructions
data NumInstr 
    = I32Const Word32
    | I64Const Word64
    | F32Const Float
    | F64Const Double

    | I32UnOp IUnOp
    | I64UnOp IUnOp
    | F32UnOp FUnOp
    | F64UnOp FUnOp

    | I32BinOp IBinOp
    | I64BinOp IBinOp
    | F32BinOp FBinOp
    | F64BinOp FBinOp

    | I32Eqz
    | I64Eqz

    | I32RelOp IRelOp
    | I64RelOp IRelOp
    | F32RelOp FRelOp
    | F64RelOp FRelOp

    | I32WrapI64
    | I32TruncF32U
    | I32TruncF32S
    | I32TruncF64U
    | I32TruncF64S
    | I64ExtendI32S
    | I64ExtendI32U
    | I64TruncF32U
    | I64TruncF32S
    | I64TruncF64U
    | I64TruncF64S
    | F32ConvertI32S
    | F32ConvertI32U
    | F32ConvertI64S
    | F32ConvertI64U
    | F32DemoteF64
    | F64ConvertI32S
    | F64ConvertI32U
    | F64ConvertI64S
    | F64ConvertI64U
    | F64PromoteF32
    | I32ReinterpretF32
    | I64ReinterpretF64
    | F32ReinterpretI32
    | F64ReinterpretI64
    | I32Extend8S
    | I32Extend16S
    | I64Extend8S
    | I64Extend16S
    | I64Extend32S
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data NumInstr'
    = I32TruncSatF32S
    | I32TruncSatF32U
    | I32TruncSatF64S
    | I32TruncSatF64U
    | I64TruncSatF32S
    | I64TruncSatF32U
    | I64TruncSatF64S
    | I64TruncSatF64U
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data IUnOp
    = IClz
    | ICtz
    | IPopcnt
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data IBinOp
    = IAdd
    | ISub
    | IMul
    | IDivU
    | IDivS
    | IRemU
    | IRemS
    | IAnd
    | IOr
    | IXor
    | IShl
    | IShrU
    | IShrS
    | IRotl
    | IRotr
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data IRelOp
    = IEq
    | INe
    | ILtU
    | ILtS
    | IGtU
    | IGtS
    | ILeU
    | ILeS
    | IGeU
    | IGeS
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data FUnOp
    = FAbs
    | FNeg
    | FCeil
    | FFloor
    | FTrunc
    | FNearest
    | FSqrt
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data FBinOp
    = FAdd
    | FSub
    | FMul
    | FDiv
    | FMin
    | FMax
    | FCopysign
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data FRelOp
    = FEq
    | FNe
    | FLt
    | FGt
    | FLe
    | FGe
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.2 Reference Instructions
data RefInstr 
    = RefNull RefType
    | RefIsNull
    | RefFunc FuncIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.3 Parametric Instructions
data ParamInstr
    = Drop
    | Select [ValType]
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.4 Variable Instructions
data VarInstr 
    = LocalGet LocalIdx
    | LocalSet LocalIdx
    | LocalTee LocalIdx
    | GlobalGet GlobalIdx
    | GlobalSet GlobalIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.5 Table Instructions
data TableInstr 
    = TableGet TableIdx
    | TableSet TableIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data TableInstr'
    = TableSize TableIdx
    | TableGrow TableIdx
    | TableFill TableIdx
    | TableCopy TableIdx TableIdx
    | TableInit TableIdx ElemIdx
    | ElemDrop ElemIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.6 Memory Instructions
data MemArg
    = MemArg { offset :: U32
             , align :: U32
             }
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data MemoryInstr 
    = I32Load MemArg
    | I64Load MemArg
    | F32Load MemArg
    | F64Load MemArg

    | I32Store MemArg
    | I64Store MemArg
    | F32Store MemArg
    | F64Store MemArg

    | I32Load8S MemArg
    | I32Load8U MemArg
    | I32Load16S MemArg
    | I32Load16U MemArg
    | I64Load8S MemArg
    | I64Load8U MemArg
    | I64Load16S MemArg
    | I64Load16U MemArg
    | I64Load32S MemArg
    | I64Load32U MemArg

    | I32Store8 MemArg
    | I32Store16 MemArg
    | I64Store8 MemArg
    | I64Store16 MemArg
    | I64Store32 MemArg
    | MemorySize
    | MemoryGrow
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data MemoryInstr'
    = MemoryFill
    | MemoryCopy
    | MemoryInit DataIdx
    | DataDrop DataIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.4.7 Control Instructions
data BlockType = BlockEmptyType | TypeIdx TypeIdx | ValType ValType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data CtlInstr 
    = Nop
    | Unreachable
    | Block BlockType [Instr]
    | Loop BlockType [Instr]
    | If BlockType [Instr] [Instr]
    | Br LabelIdx
    | BrIf LabelIdx
    | BrTable [LabelIdx] LabelIdx
    | Return
    | Call FuncIdx
    | CallIndirect TableIdx TypeIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

newtype Expr = Expr [Instr]
    deriving (Eq, Generic)
    deriving anyclass (NFData, T.Print)

data Instr 
    = NumInstr NumInstr
    | RefInstr RefInstr 
    | ParamInstr ParamInstr
    | VarInstr VarInstr 
    | TableInstr TableInstr 
    | MemoryInstr MemoryInstr 
    | CtlInstr CtlInstr

    | NumInstr' NumInstr'
    | TableInstr' TableInstr'
    | MemoryInstr' MemoryInstr'
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

refToExpr :: FuncIdx -> Expr
refToExpr idx = Expr [RefInstr $ RefFunc idx]

exprsToRefs :: [Expr] -> Maybe [FuncIdx]
exprsToRefs exprs
 | length exprs == length idxs = Just idxs
 | otherwise = Nothing
 where
    go (Expr [RefInstr (RefFunc idx)]) = Just idx
    go _ = Nothing

    idxs = mapMaybe go exprs

pattern RefFuncs :: [FuncIdx] -> [Expr]
pattern RefFuncs idxs <- (exprsToRefs -> Just idxs) where
    RefFuncs idxs = map refToExpr idxs
 
-- 2.5 Modules
data Module
    = Module { types :: [FuncType]
             , typeidxs :: [TypeIdx]
             , codes :: [Code]
             , tables :: [Table]
             , mems :: [Mem]
             , globals :: [Global]
             , elems :: [Elem]
             , datacount :: Datacount
             , datas :: [Data]
             , start :: Start
             , imports :: [Import]
             , exports :: [Export]
             }
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

-- 2.5.1 Indices
type TypeIdx = U32
type FuncIdx = U32
type TableIdx = U32
type MemIdx = U32
type GlobalIdx = U32
type ElemIdx = U32
type DataIdx = U32
type LocalIdx = U32
type LabelIdx = U32

data Code = Code {locals :: [ValType], body :: Expr}
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

newtype Table = Table {_type :: TableType}
    deriving (Eq, Generic)
    deriving anyclass (NFData, T.Print)

newtype Mem = Mem {_type :: MemType}
    deriving (Eq, Generic)
    deriving anyclass (NFData, T.Print)

data Global = Global {_type :: GlobalType, init :: Expr}
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Elem = Elem { refType :: RefType
                  , init :: [Expr]
                  , mode :: ElemMode
                  }
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Datacount = NoDatacount | Datacount U32
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data ElemMode
  = EPassive
  | EActive {table :: TableIdx, offset :: Expr}
  | EDeclarative
  deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Data = Data { init :: Bytes, mode :: DataMode} 
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data DataMode = DPassive | DActive { memory :: MemIdx, offset :: Expr}
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Start = NoStart | StartAt FuncIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Export
    = Export { name :: Name
             , desc :: ExportDesc
             }
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data ExportDesc
    = ExportFunc FuncIdx
    | ExportTable TableIdx
    | ExportMem MemIdx
    | ExportGlobal GlobalIdx
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data Import
    = Import { _module :: Name
             , name :: Name
             , desc :: ImportDesc
             }
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print

data ImportDesc
    = ImportFunc TypeIdx
    | ImportTable TableType
    | ImportMemory MemType
    | ImportGlobal GlobalType
    deriving (Eq, Generic, NFData)
    deriving anyclass T.Print
