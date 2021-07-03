module Slabs.Wasm.Binary  where

import Universum 
import Slabs.Wasm.Structure 
import Slabs.Wasm.Internal 

import Z.Data.Parser (Parser)
import qualified Z.Data.Parser as P 
import qualified Control.Monad.Combinators as P 
import Z.Data.Builder (Builder)
import qualified Z.Data.Builder as B 

decodeNumType :: Parser NumType
decodeNumType = do P.word8 127
                   pure I32
                <|> do P.word8 126
                       pure I64
                <|> do P.word8 125
                       pure F32
                <|> do P.word8 124
                       pure F64

encodeNumType :: NumType -> Builder ()
encodeNumType I32 = B.word8 127
encodeNumType I64 = B.word8 126
encodeNumType F32 = B.word8 125
encodeNumType F64 = B.word8 124

decodeRefType :: Parser RefType
decodeRefType = do P.word8 112
                   pure FuncRef
                <|> do P.word8 111
                       pure ExternRef

encodeRefType :: RefType -> Builder ()
encodeRefType FuncRef = B.word8 112
encodeRefType ExternRef = B.word8 111

decodeValType :: Parser ValType
decodeValType = do t <- decodeNumType
                   pure (NumType t)
                <|> do t <- decodeRefType
                       pure (RefType t)

encodeValType :: ValType -> Builder ()
encodeValType (NumType t) = encodeNumType t
encodeValType (RefType t) = encodeRefType t

decodeResultType :: Parser ResultType
decodeResultType = decodeVec decodeValType

encodeResultType :: ResultType -> Builder ()
encodeResultType = encodeVec encodeValType

decodeFuncType :: Parser FuncType
decodeFuncType = do P.word8 96
                    rt1 <- decodeResultType
                    rt2 <- decodeResultType
                    pure (FuncType rt1 rt2)

encodeFuncType :: FuncType -> Builder ()
encodeFuncType (FuncType rt1 rt2) = do B.word8 96
                                       encodeResultType rt1
                                       encodeResultType rt2

decodeLimits :: Parser Limits
decodeLimits = do P.word8 0
                  n <- decodeU32
                  pure (Limits n Nothing)
               <|> do P.word8 1
                      n <- decodeU32
                      m <- decodeU32
                      pure (Limits n (Just m))

encodeLimits :: Limits -> Builder ()
encodeLimits (Limits n Nothing) = do B.word8 0
                                     encodeU32 n
encodeLimits (Limits n (Just m)) = do B.word8 1
                                      encodeU32 n
                                      encodeU32 m

decodeMemType :: Parser MemType
decodeMemType = decodeLimits

encodeMemType :: MemType -> Builder ()
encodeMemType = encodeLimits

decodeTableType :: Parser TableType
decodeTableType = do et <- decodeRefType
                     lim <- decodeLimits
                     pure (TableType lim et)

encodeTableType :: TableType -> Builder ()
encodeTableType (TableType lim et) = do encodeRefType et
                                        encodeLimits lim

decodeGlobalType :: Parser GlobalType
decodeGlobalType = do t <- decodeValType
                      m <- decodeMut
                      pure (GlobalType m t)

encodeGlobalType :: GlobalType -> Builder ()
encodeGlobalType (GlobalType m t) = do encodeValType t
                                       encodeMut m

decodeMut :: Parser Mut
decodeMut = do P.word8 0
               pure MutConst
            <|> do P.word8 1
                   pure MutVar

encodeMut :: Mut -> Builder ()
encodeMut MutConst = B.word8 0
encodeMut MutVar = B.word8 1

decodeBlockType :: Parser BlockType
decodeBlockType = do P.word8 64
                     pure BlockEmptyType
                  <|> do t <- decodeValType
                         pure (ValType t)
                  <|> do x <- decodeU32
                         pure (TypeIdx x)

encodeBlockType :: BlockType -> Builder ()
encodeBlockType BlockEmptyType = B.word8 64
encodeBlockType (ValType t) = encodeValType t
encodeBlockType (TypeIdx x) = encodeU32 x

decodeElseInstrs :: Parser [Instr]
decodeElseInstrs = do P.word8 11
                      pure []
                   <|> do P.word8 5
                          ins <- P.many decodeInstr
                          P.word8 11
                          pure ins

encodeElseInstrs :: [Instr] -> Builder ()
encodeElseInstrs [] = B.word8 11
encodeElseInstrs ins = do B.word8 5
                          mapM_ encodeInstr ins
                          B.word8 11

decodeCtlInstr :: Parser CtlInstr
decodeCtlInstr = do P.word8 0
                    pure Unreachable
                 <|> do P.word8 1
                        pure Nop
                 <|> do P.word8 2
                        bt <- decodeBlockType
                        ins <- P.many decodeInstr
                        P.word8 11
                        pure (Block bt ins)
                 <|> do P.word8 3
                        bt <- decodeBlockType
                        ins <- P.many decodeInstr
                        P.word8 11
                        pure (Loop bt ins)
                 <|> do P.word8 4
                        bt <- decodeBlockType
                        in1 <- P.many decodeInstr
                        in2 <- decodeElseInstrs
                        pure (If bt in1 in2)
                 <|> do P.word8 12
                        l <- decodeLabelIdx
                        pure (Br l)
                 <|> do P.word8 13
                        l <- decodeLabelIdx
                        pure (BrIf l)
                 <|> do P.word8 14
                        ls <- decodeVec decodeLabelIdx
                        ln <- decodeLabelIdx
                        pure (BrTable ls ln)
                 <|> do P.word8 15
                        pure Return
                 <|> do P.word8 16
                        x <- decodeFuncIdx
                        pure (Call x)
                 <|> do P.word8 17
                        y <- decodeTypeIdx
                        x <- decodeTableIdx
                        pure (CallIndirect x y)

encodeCtlInstr :: CtlInstr -> Builder ()
encodeCtlInstr Unreachable = B.word8 0
encodeCtlInstr Nop = B.word8 1
encodeCtlInstr (Block bt ins) = do B.word8 2
                                   encodeBlockType bt
                                   mapM_ encodeInstr ins
                                   B.word8 11
encodeCtlInstr (Loop bt ins) = do B.word8 3
                                  encodeBlockType bt
                                  mapM_ encodeInstr ins
                                  B.word8 11
encodeCtlInstr (If bt in1 in2) = do B.word8 4
                                    encodeBlockType bt
                                    mapM_ encodeInstr in1
                                    encodeElseInstrs in2
encodeCtlInstr (Br l) = do B.word8 12
                           encodeLabelIdx l
encodeCtlInstr (BrIf l) = do B.word8 13
                             encodeLabelIdx l
encodeCtlInstr (BrTable ls ln) = do B.word8 14
                                    encodeVec encodeLabelIdx ls
                                    encodeLabelIdx ln
encodeCtlInstr Return = B.word8 15
encodeCtlInstr (Call x) = do B.word8 16
                             encodeFuncIdx x
encodeCtlInstr (CallIndirect x y) = do B.word8 17
                                       encodeTypeIdx y
                                       encodeTableIdx x

decodeRefInstr :: Parser RefInstr
decodeRefInstr = do P.word8 208
                    t <- decodeRefType
                    pure (RefNull t)
                 <|> do P.word8 209
                        pure RefIsNull
                 <|> do P.word8 210
                        x <- decodeFuncIdx
                        pure (RefFunc x)

encodeRefInstr :: RefInstr -> Builder ()
encodeRefInstr (RefNull t) = do B.word8 208
                                encodeRefType t
encodeRefInstr RefIsNull = B.word8 209
encodeRefInstr (RefFunc x) = do B.word8 210
                                encodeFuncIdx x

decodeParamInstr :: Parser ParamInstr
decodeParamInstr = do P.word8 26
                      pure Drop
                   <|> do P.word8 27
                          pure (Select [])
                   <|> do P.word8 28
                          ts <- decodeVec decodeValType
                          pure (Select ts)

encodeParamInstr :: ParamInstr -> Builder ()
encodeParamInstr Drop = B.word8 26
encodeParamInstr (Select []) = B.word8 27
encodeParamInstr (Select ts) = do B.word8 28
                                  encodeVec encodeValType ts

decodeVarInstr :: Parser VarInstr
decodeVarInstr = do P.word8 32
                    x <- decodeLocalIdx
                    pure (LocalGet x)
                 <|> do P.word8 33
                        x <- decodeLocalIdx
                        pure (LocalSet x)
                 <|> do P.word8 34
                        x <- decodeLocalIdx
                        pure (LocalTee x)
                 <|> do P.word8 35
                        x <- decodeGlobalIdx
                        pure (GlobalGet x)
                 <|> do P.word8 36
                        x <- decodeGlobalIdx
                        pure (GlobalSet x)

encodeVarInstr :: VarInstr -> Builder ()
encodeVarInstr (LocalGet x) = do B.word8 32
                                 encodeLocalIdx x
encodeVarInstr (LocalSet x) = do B.word8 33
                                 encodeLocalIdx x
encodeVarInstr (LocalTee x) = do B.word8 34
                                 encodeLocalIdx x
encodeVarInstr (GlobalGet x) = do B.word8 35
                                  encodeGlobalIdx x
encodeVarInstr (GlobalSet x) = do B.word8 36
                                  encodeGlobalIdx x

decodeTableInstr :: Parser TableInstr
decodeTableInstr = do P.word8 37
                      x <- decodeTableIdx
                      pure (TableGet x)
                   <|> do P.word8 38
                          x <- decodeTableIdx
                          pure (TableSet x)

encodeTableInstr :: TableInstr -> Builder ()
encodeTableInstr (TableGet x) = do B.word8 37
                                   encodeTableIdx x
encodeTableInstr (TableSet x) = do B.word8 38
                                   encodeTableIdx x

decodeTableInstr' :: Parser TableInstr'
decodeTableInstr' = do checkEq decodeU32 12
                       y <- decodeElemIdx
                       x <- decodeTableIdx
                       pure (TableInit x y)
                    <|> do checkEq decodeU32 13
                           x <- decodeElemIdx
                           pure (ElemDrop x)
                    <|> do checkEq decodeU32 14
                           x <- decodeTableIdx
                           y <- decodeTableIdx
                           pure (TableCopy x y)
                    <|> do checkEq decodeU32 15
                           x <- decodeTableIdx
                           pure (TableGrow x)
                    <|> do checkEq decodeU32 16
                           x <- decodeTableIdx
                           pure (TableSize x)
                    <|> do checkEq decodeU32 17
                           x <- decodeTableIdx
                           pure (TableFill x)

encodeTableInstr' :: TableInstr' -> Builder ()
encodeTableInstr' (TableInit x y) = do encodeU32 12
                                       encodeElemIdx y
                                       encodeTableIdx x
encodeTableInstr' (ElemDrop x) = do encodeU32 13
                                    encodeElemIdx x
encodeTableInstr' (TableCopy x y) = do encodeU32 14
                                       encodeTableIdx x
                                       encodeTableIdx y
encodeTableInstr' (TableGrow x) = do encodeU32 15
                                     encodeTableIdx x
encodeTableInstr' (TableSize x) = do encodeU32 16
                                     encodeTableIdx x
encodeTableInstr' (TableFill x) = do encodeU32 17
                                     encodeTableIdx x

decodeMemArg :: Parser MemArg
decodeMemArg = do a <- decodeU32
                  b <- decodeU32
                  pure (MemArg a b)

encodeMemArg :: MemArg -> Builder ()
encodeMemArg (MemArg a b) = do encodeU32 a
                               encodeU32 b

decodeMemoryInstr :: Parser MemoryInstr
decodeMemoryInstr = do P.word8 40
                       m <- decodeMemArg
                       pure (I32Load m)
                    <|> do P.word8 41
                           m <- decodeMemArg
                           pure (I64Load m)
                    <|> do P.word8 42
                           m <- decodeMemArg
                           pure (F32Load m)
                    <|> do P.word8 43
                           m <- decodeMemArg
                           pure (F64Load m)
                    <|> do P.word8 44
                           m <- decodeMemArg
                           pure (I32Load8S m)
                    <|> do P.word8 45
                           m <- decodeMemArg
                           pure (I32Load8U m)
                    <|> do P.word8 46
                           m <- decodeMemArg
                           pure (I32Load16S m)
                    <|> do P.word8 47
                           m <- decodeMemArg
                           pure (I32Load16U m)
                    <|> do P.word8 48
                           m <- decodeMemArg
                           pure (I64Load8S m)
                    <|> do P.word8 49
                           m <- decodeMemArg
                           pure (I64Load8U m)
                    <|> do P.word8 50
                           m <- decodeMemArg
                           pure (I64Load16S m)
                    <|> do P.word8 51
                           m <- decodeMemArg
                           pure (I64Load16U m)
                    <|> do P.word8 52
                           m <- decodeMemArg
                           pure (I64Load32S m)
                    <|> do P.word8 53
                           m <- decodeMemArg
                           pure (I64Load32U m)
                    <|> do P.word8 54
                           m <- decodeMemArg
                           pure (I32Store m)
                    <|> do P.word8 55
                           m <- decodeMemArg
                           pure (I64Store m)
                    <|> do P.word8 56
                           m <- decodeMemArg
                           pure (F32Store m)
                    <|> do P.word8 57
                           m <- decodeMemArg
                           pure (F64Store m)
                    <|> do P.word8 58
                           m <- decodeMemArg
                           pure (I32Store8 m)
                    <|> do P.word8 59
                           m <- decodeMemArg
                           pure (I32Store16 m)
                    <|> do P.word8 60
                           m <- decodeMemArg
                           pure (I64Store8 m)
                    <|> do P.word8 61
                           m <- decodeMemArg
                           pure (I64Store16 m)
                    <|> do P.word8 62
                           m <- decodeMemArg
                           pure (I64Store32 m)
                    <|> do P.word8 63
                           P.word8 0
                           pure MemorySize
                    <|> do P.word8 64
                           P.word8 0
                           pure MemoryGrow

encodeMemoryInstr :: MemoryInstr -> Builder ()
encodeMemoryInstr (I32Load m) = do B.word8 40
                                   encodeMemArg m
encodeMemoryInstr (I64Load m) = do B.word8 41
                                   encodeMemArg m
encodeMemoryInstr (F32Load m) = do B.word8 42
                                   encodeMemArg m
encodeMemoryInstr (F64Load m) = do B.word8 43
                                   encodeMemArg m
encodeMemoryInstr (I32Load8S m) = do B.word8 44
                                     encodeMemArg m
encodeMemoryInstr (I32Load8U m) = do B.word8 45
                                     encodeMemArg m
encodeMemoryInstr (I32Load16S m) = do B.word8 46
                                      encodeMemArg m
encodeMemoryInstr (I32Load16U m) = do B.word8 47
                                      encodeMemArg m
encodeMemoryInstr (I64Load8S m) = do B.word8 48
                                     encodeMemArg m
encodeMemoryInstr (I64Load8U m) = do B.word8 49
                                     encodeMemArg m
encodeMemoryInstr (I64Load16S m) = do B.word8 50
                                      encodeMemArg m
encodeMemoryInstr (I64Load16U m) = do B.word8 51
                                      encodeMemArg m
encodeMemoryInstr (I64Load32S m) = do B.word8 52
                                      encodeMemArg m
encodeMemoryInstr (I64Load32U m) = do B.word8 53
                                      encodeMemArg m
encodeMemoryInstr (I32Store m) = do B.word8 54
                                    encodeMemArg m
encodeMemoryInstr (I64Store m) = do B.word8 55
                                    encodeMemArg m
encodeMemoryInstr (F32Store m) = do B.word8 56
                                    encodeMemArg m
encodeMemoryInstr (F64Store m) = do B.word8 57
                                    encodeMemArg m
encodeMemoryInstr (I32Store8 m) = do B.word8 58
                                     encodeMemArg m
encodeMemoryInstr (I32Store16 m) = do B.word8 59
                                      encodeMemArg m
encodeMemoryInstr (I64Store8 m) = do B.word8 60
                                     encodeMemArg m
encodeMemoryInstr (I64Store16 m) = do B.word8 61
                                      encodeMemArg m
encodeMemoryInstr (I64Store32 m) = do B.word8 62
                                      encodeMemArg m
encodeMemoryInstr MemorySize = do B.word8 63
                                  B.word8 0
encodeMemoryInstr MemoryGrow = do B.word8 64
                                  B.word8 0

decodeMemoryInstr' :: Parser MemoryInstr'
decodeMemoryInstr' = do checkEq decodeU32 8
                        x <- decodeDataIdx
                        P.word8 0
                        pure (MemoryInit x)
                     <|> do checkEq decodeU32 9
                            x <- decodeDataIdx
                            pure (DataDrop x)
                     <|> do checkEq decodeU32 10
                            P.word8 0
                            P.word8 0
                            pure MemoryCopy
                     <|> do checkEq decodeU32 11
                            P.word8 0
                            pure MemoryFill

encodeMemoryInstr' :: MemoryInstr' -> Builder ()
encodeMemoryInstr' (MemoryInit x) = do encodeU32 8
                                       encodeDataIdx x
                                       B.word8 0
encodeMemoryInstr' (DataDrop x) = do encodeU32 9
                                     encodeDataIdx x
encodeMemoryInstr' MemoryCopy = do encodeU32 10
                                   B.word8 0
                                   B.word8 0
encodeMemoryInstr' MemoryFill = do encodeU32 11
                                   B.word8 0

decodeNumInstr :: Parser NumInstr
decodeNumInstr = do P.word8 65
                    n <- decodeU32
                    pure (I32Const n)
                 <|> do P.word8 66
                        n <- decodeU64
                        pure (I64Const n)
                 <|> do P.word8 67
                        z <- decodeF32
                        pure (F32Const z)
                 <|> do P.word8 68
                        z <- decodeF64
                        pure (F64Const z)
                 <|> do P.word8 69
                        pure I32Eqz
                 <|> do P.word8 70
                        pure (I32RelOp IEq)
                 <|> do P.word8 71
                        pure (I32RelOp INe)
                 <|> do P.word8 72
                        pure (I32RelOp ILtS)
                 <|> do P.word8 73
                        pure (I32RelOp ILtU)
                 <|> do P.word8 74
                        pure (I32RelOp IGtS)
                 <|> do P.word8 75
                        pure (I32RelOp IGtU)
                 <|> do P.word8 76
                        pure (I32RelOp ILeS)
                 <|> do P.word8 77
                        pure (I32RelOp ILeU)
                 <|> do P.word8 78
                        pure (I32RelOp IGeS)
                 <|> do P.word8 79
                        pure (I32RelOp IGeU)
                 <|> do P.word8 80
                        pure I64Eqz
                 <|> do P.word8 81
                        pure (I64RelOp IEq)
                 <|> do P.word8 82
                        pure (I64RelOp INe)
                 <|> do P.word8 83
                        pure (I64RelOp ILtS)
                 <|> do P.word8 84
                        pure (I64RelOp ILtU)
                 <|> do P.word8 85
                        pure (I64RelOp IGtS)
                 <|> do P.word8 86
                        pure (I64RelOp IGtU)
                 <|> do P.word8 87
                        pure (I64RelOp ILeS)
                 <|> do P.word8 88
                        pure (I64RelOp ILeU)
                 <|> do P.word8 89
                        pure (I64RelOp IGeS)
                 <|> do P.word8 90
                        pure (I64RelOp IGeU)
                 <|> do P.word8 91
                        pure (F32RelOp FEq)
                 <|> do P.word8 92
                        pure (F32RelOp FNe)
                 <|> do P.word8 93
                        pure (F32RelOp FLt)
                 <|> do P.word8 94
                        pure (F32RelOp FGt)
                 <|> do P.word8 95
                        pure (F32RelOp FLe)
                 <|> do P.word8 96
                        pure (F32RelOp FGe)
                 <|> do P.word8 97
                        pure (F64RelOp FEq)
                 <|> do P.word8 98
                        pure (F64RelOp FNe)
                 <|> do P.word8 99
                        pure (F64RelOp FLt)
                 <|> do P.word8 100
                        pure (F64RelOp FGt)
                 <|> do P.word8 101
                        pure (F64RelOp FLe)
                 <|> do P.word8 102
                        pure (F64RelOp FGe)
                 <|> do P.word8 103
                        pure (I32UnOp IClz)
                 <|> do P.word8 104
                        pure (I32UnOp ICtz)
                 <|> do P.word8 105
                        pure (I32UnOp IPopcnt)
                 <|> do P.word8 106
                        pure (I32BinOp IAdd)
                 <|> do P.word8 107
                        pure (I32BinOp ISub)
                 <|> do P.word8 108
                        pure (I32BinOp IMul)
                 <|> do P.word8 109
                        pure (I32BinOp IDivS)
                 <|> do P.word8 110
                        pure (I32BinOp IDivU)
                 <|> do P.word8 111
                        pure (I32BinOp IRemS)
                 <|> do P.word8 112
                        pure (I32BinOp IRemU)
                 <|> do P.word8 113
                        pure (I32BinOp IAnd)
                 <|> do P.word8 114
                        pure (I32BinOp IOr)
                 <|> do P.word8 115
                        pure (I32BinOp IXor)
                 <|> do P.word8 116
                        pure (I32BinOp IShl)
                 <|> do P.word8 117
                        pure (I32BinOp IShrS)
                 <|> do P.word8 118
                        pure (I32BinOp IShrU)
                 <|> do P.word8 119
                        pure (I32BinOp IRotl)
                 <|> do P.word8 120
                        pure (I32BinOp IRotr)
                 <|> do P.word8 121
                        pure (I64UnOp IClz)
                 <|> do P.word8 122
                        pure (I64UnOp ICtz)
                 <|> do P.word8 123
                        pure (I64UnOp IPopcnt)
                 <|> do P.word8 124
                        pure (I64BinOp IAdd)
                 <|> do P.word8 125
                        pure (I64BinOp ISub)
                 <|> do P.word8 126
                        pure (I64BinOp IMul)
                 <|> do P.word8 127
                        pure (I64BinOp IDivS)
                 <|> do P.word8 128
                        pure (I64BinOp IDivU)
                 <|> do P.word8 129
                        pure (I64BinOp IRemS)
                 <|> do P.word8 130
                        pure (I64BinOp IRemU)
                 <|> do P.word8 131
                        pure (I64BinOp IAnd)
                 <|> do P.word8 132
                        pure (I64BinOp IOr)
                 <|> do P.word8 133
                        pure (I64BinOp IXor)
                 <|> do P.word8 134
                        pure (I64BinOp IShl)
                 <|> do P.word8 135
                        pure (I64BinOp IShrS)
                 <|> do P.word8 136
                        pure (I64BinOp IShrU)
                 <|> do P.word8 137
                        pure (I64BinOp IRotl)
                 <|> do P.word8 138
                        pure (I64BinOp IRotr)
                 <|> do P.word8 139
                        pure (F32UnOp FAbs)
                 <|> do P.word8 140
                        pure (F32UnOp FNeg)
                 <|> do P.word8 141
                        pure (F32UnOp FCeil)
                 <|> do P.word8 142
                        pure (F32UnOp FFloor)
                 <|> do P.word8 143
                        pure (F32UnOp FTrunc)
                 <|> do P.word8 144
                        pure (F32UnOp FNearest)
                 <|> do P.word8 145
                        pure (F32UnOp FSqrt)
                 <|> do P.word8 146
                        pure (F32BinOp FAdd)
                 <|> do P.word8 147
                        pure (F32BinOp FSub)
                 <|> do P.word8 148
                        pure (F32BinOp FMul)
                 <|> do P.word8 149
                        pure (F32BinOp FDiv)
                 <|> do P.word8 150
                        pure (F32BinOp FMin)
                 <|> do P.word8 151
                        pure (F32BinOp FMax)
                 <|> do P.word8 152
                        pure (F32BinOp FCopysign)
                 <|> do P.word8 153
                        pure (F64UnOp FAbs)
                 <|> do P.word8 154
                        pure (F64UnOp FNeg)
                 <|> do P.word8 155
                        pure (F64UnOp FCeil)
                 <|> do P.word8 156
                        pure (F64UnOp FFloor)
                 <|> do P.word8 157
                        pure (F64UnOp FTrunc)
                 <|> do P.word8 158
                        pure (F64UnOp FNearest)
                 <|> do P.word8 159
                        pure (F64UnOp FSqrt)
                 <|> do P.word8 160
                        pure (F64BinOp FAdd)
                 <|> do P.word8 161
                        pure (F64BinOp FSub)
                 <|> do P.word8 162
                        pure (F64BinOp FMul)
                 <|> do P.word8 163
                        pure (F64BinOp FDiv)
                 <|> do P.word8 164
                        pure (F64BinOp FMin)
                 <|> do P.word8 165
                        pure (F64BinOp FMax)
                 <|> do P.word8 166
                        pure (F64BinOp FCopysign)
                 <|> do P.word8 167
                        pure I32WrapI64
                 <|> do P.word8 168
                        pure I32TruncF32S
                 <|> do P.word8 169
                        pure I32TruncF32U
                 <|> do P.word8 170
                        pure I32TruncF64S
                 <|> do P.word8 171
                        pure I32TruncF64U
                 <|> do P.word8 172
                        pure I64ExtendI32S
                 <|> do P.word8 173
                        pure I64ExtendI32U
                 <|> do P.word8 174
                        pure I64TruncF32S
                 <|> do P.word8 175
                        pure I64TruncF32U
                 <|> do P.word8 176
                        pure I64TruncF64S
                 <|> do P.word8 177
                        pure I64TruncF64U
                 <|> do P.word8 178
                        pure F32ConvertI32S
                 <|> do P.word8 179
                        pure F32ConvertI32U
                 <|> do P.word8 180
                        pure F32ConvertI64S
                 <|> do P.word8 181
                        pure F32ConvertI64U
                 <|> do P.word8 182
                        pure F32DemoteF64
                 <|> do P.word8 183
                        pure F64ConvertI32S
                 <|> do P.word8 184
                        pure F64ConvertI32U
                 <|> do P.word8 185
                        pure F64ConvertI64S
                 <|> do P.word8 186
                        pure F64ConvertI64U
                 <|> do P.word8 187
                        pure F64PromoteF32
                 <|> do P.word8 188
                        pure I32ReinterpretF32
                 <|> do P.word8 189
                        pure I64ReinterpretF64
                 <|> do P.word8 190
                        pure F32ReinterpretI32
                 <|> do P.word8 191
                        pure F64ReinterpretI64
                 <|> do P.word8 192
                        pure I32Extend8S
                 <|> do P.word8 193
                        pure I32Extend16S
                 <|> do P.word8 194
                        pure I64Extend8S
                 <|> do P.word8 195
                        pure I64Extend16S
                 <|> do P.word8 196
                        pure I64Extend32S

encodeNumInstr :: NumInstr -> Builder ()
encodeNumInstr (I32Const n) = do B.word8 65
                                 encodeU32 n
encodeNumInstr (I64Const n) = do B.word8 66
                                 encodeU64 n
encodeNumInstr (F32Const z) = do B.word8 67
                                 encodeF32 z
encodeNumInstr (F64Const z) = do B.word8 68
                                 encodeF64 z
encodeNumInstr I32Eqz = B.word8 69
encodeNumInstr (I32RelOp IEq) = B.word8 70
encodeNumInstr (I32RelOp INe) = B.word8 71
encodeNumInstr (I32RelOp ILtS) = B.word8 72
encodeNumInstr (I32RelOp ILtU) = B.word8 73
encodeNumInstr (I32RelOp IGtS) = B.word8 74
encodeNumInstr (I32RelOp IGtU) = B.word8 75
encodeNumInstr (I32RelOp ILeS) = B.word8 76
encodeNumInstr (I32RelOp ILeU) = B.word8 77
encodeNumInstr (I32RelOp IGeS) = B.word8 78
encodeNumInstr (I32RelOp IGeU) = B.word8 79
encodeNumInstr I64Eqz = B.word8 80
encodeNumInstr (I64RelOp IEq) = B.word8 81
encodeNumInstr (I64RelOp INe) = B.word8 82
encodeNumInstr (I64RelOp ILtS) = B.word8 83
encodeNumInstr (I64RelOp ILtU) = B.word8 84
encodeNumInstr (I64RelOp IGtS) = B.word8 85
encodeNumInstr (I64RelOp IGtU) = B.word8 86
encodeNumInstr (I64RelOp ILeS) = B.word8 87
encodeNumInstr (I64RelOp ILeU) = B.word8 88
encodeNumInstr (I64RelOp IGeS) = B.word8 89
encodeNumInstr (I64RelOp IGeU) = B.word8 90
encodeNumInstr (F32RelOp FEq) = B.word8 91
encodeNumInstr (F32RelOp FNe) = B.word8 92
encodeNumInstr (F32RelOp FLt) = B.word8 93
encodeNumInstr (F32RelOp FGt) = B.word8 94
encodeNumInstr (F32RelOp FLe) = B.word8 95
encodeNumInstr (F32RelOp FGe) = B.word8 96
encodeNumInstr (F64RelOp FEq) = B.word8 97
encodeNumInstr (F64RelOp FNe) = B.word8 98
encodeNumInstr (F64RelOp FLt) = B.word8 99
encodeNumInstr (F64RelOp FGt) = B.word8 100
encodeNumInstr (F64RelOp FLe) = B.word8 101
encodeNumInstr (F64RelOp FGe) = B.word8 102
encodeNumInstr (I32UnOp IClz) = B.word8 103
encodeNumInstr (I32UnOp ICtz) = B.word8 104
encodeNumInstr (I32UnOp IPopcnt) = B.word8 105
encodeNumInstr (I32BinOp IAdd) = B.word8 106
encodeNumInstr (I32BinOp ISub) = B.word8 107
encodeNumInstr (I32BinOp IMul) = B.word8 108
encodeNumInstr (I32BinOp IDivS) = B.word8 109
encodeNumInstr (I32BinOp IDivU) = B.word8 110
encodeNumInstr (I32BinOp IRemS) = B.word8 111
encodeNumInstr (I32BinOp IRemU) = B.word8 112
encodeNumInstr (I32BinOp IAnd) = B.word8 113
encodeNumInstr (I32BinOp IOr) = B.word8 114
encodeNumInstr (I32BinOp IXor) = B.word8 115
encodeNumInstr (I32BinOp IShl) = B.word8 116
encodeNumInstr (I32BinOp IShrS) = B.word8 117
encodeNumInstr (I32BinOp IShrU) = B.word8 118
encodeNumInstr (I32BinOp IRotl) = B.word8 119
encodeNumInstr (I32BinOp IRotr) = B.word8 120
encodeNumInstr (I64UnOp IClz) = B.word8 121
encodeNumInstr (I64UnOp ICtz) = B.word8 122
encodeNumInstr (I64UnOp IPopcnt) = B.word8 123
encodeNumInstr (I64BinOp IAdd) = B.word8 124
encodeNumInstr (I64BinOp ISub) = B.word8 125
encodeNumInstr (I64BinOp IMul) = B.word8 126
encodeNumInstr (I64BinOp IDivS) = B.word8 127
encodeNumInstr (I64BinOp IDivU) = B.word8 128
encodeNumInstr (I64BinOp IRemS) = B.word8 129
encodeNumInstr (I64BinOp IRemU) = B.word8 130
encodeNumInstr (I64BinOp IAnd) = B.word8 131
encodeNumInstr (I64BinOp IOr) = B.word8 132
encodeNumInstr (I64BinOp IXor) = B.word8 133
encodeNumInstr (I64BinOp IShl) = B.word8 134
encodeNumInstr (I64BinOp IShrS) = B.word8 135
encodeNumInstr (I64BinOp IShrU) = B.word8 136
encodeNumInstr (I64BinOp IRotl) = B.word8 137
encodeNumInstr (I64BinOp IRotr) = B.word8 138
encodeNumInstr (F32UnOp FAbs) = B.word8 139
encodeNumInstr (F32UnOp FNeg) = B.word8 140
encodeNumInstr (F32UnOp FCeil) = B.word8 141
encodeNumInstr (F32UnOp FFloor) = B.word8 142
encodeNumInstr (F32UnOp FTrunc) = B.word8 143
encodeNumInstr (F32UnOp FNearest) = B.word8 144
encodeNumInstr (F32UnOp FSqrt) = B.word8 145
encodeNumInstr (F32BinOp FAdd) = B.word8 146
encodeNumInstr (F32BinOp FSub) = B.word8 147
encodeNumInstr (F32BinOp FMul) = B.word8 148
encodeNumInstr (F32BinOp FDiv) = B.word8 149
encodeNumInstr (F32BinOp FMin) = B.word8 150
encodeNumInstr (F32BinOp FMax) = B.word8 151
encodeNumInstr (F32BinOp FCopysign) = B.word8 152
encodeNumInstr (F64UnOp FAbs) = B.word8 153
encodeNumInstr (F64UnOp FNeg) = B.word8 154
encodeNumInstr (F64UnOp FCeil) = B.word8 155
encodeNumInstr (F64UnOp FFloor) = B.word8 156
encodeNumInstr (F64UnOp FTrunc) = B.word8 157
encodeNumInstr (F64UnOp FNearest) = B.word8 158
encodeNumInstr (F64UnOp FSqrt) = B.word8 159
encodeNumInstr (F64BinOp FAdd) = B.word8 160
encodeNumInstr (F64BinOp FSub) = B.word8 161
encodeNumInstr (F64BinOp FMul) = B.word8 162
encodeNumInstr (F64BinOp FDiv) = B.word8 163
encodeNumInstr (F64BinOp FMin) = B.word8 164
encodeNumInstr (F64BinOp FMax) = B.word8 165
encodeNumInstr (F64BinOp FCopysign) = B.word8 166
encodeNumInstr I32WrapI64 = B.word8 167
encodeNumInstr I32TruncF32S = B.word8 168
encodeNumInstr I32TruncF32U = B.word8 169
encodeNumInstr I32TruncF64S = B.word8 170
encodeNumInstr I32TruncF64U = B.word8 171
encodeNumInstr I64ExtendI32S = B.word8 172
encodeNumInstr I64ExtendI32U = B.word8 173
encodeNumInstr I64TruncF32S = B.word8 174
encodeNumInstr I64TruncF32U = B.word8 175
encodeNumInstr I64TruncF64S = B.word8 176
encodeNumInstr I64TruncF64U = B.word8 177
encodeNumInstr F32ConvertI32S = B.word8 178
encodeNumInstr F32ConvertI32U = B.word8 179
encodeNumInstr F32ConvertI64S = B.word8 180
encodeNumInstr F32ConvertI64U = B.word8 181
encodeNumInstr F32DemoteF64 = B.word8 182
encodeNumInstr F64ConvertI32S = B.word8 183
encodeNumInstr F64ConvertI32U = B.word8 184
encodeNumInstr F64ConvertI64S = B.word8 185
encodeNumInstr F64ConvertI64U = B.word8 186
encodeNumInstr F64PromoteF32 = B.word8 187
encodeNumInstr I32ReinterpretF32 = B.word8 188
encodeNumInstr I64ReinterpretF64 = B.word8 189
encodeNumInstr F32ReinterpretI32 = B.word8 190
encodeNumInstr F64ReinterpretI64 = B.word8 191
encodeNumInstr I32Extend8S = B.word8 192
encodeNumInstr I32Extend16S = B.word8 193
encodeNumInstr I64Extend8S = B.word8 194
encodeNumInstr I64Extend16S = B.word8 195
encodeNumInstr I64Extend32S = B.word8 196

decodeNumInstr' :: Parser NumInstr'
decodeNumInstr' = do checkEq decodeU32 0
                     pure I32TruncSatF32S
                  <|> do checkEq decodeU32 1
                         pure I32TruncSatF32U
                  <|> do checkEq decodeU32 2
                         pure I32TruncSatF64S
                  <|> do checkEq decodeU32 3
                         pure I32TruncSatF64U
                  <|> do checkEq decodeU32 4
                         pure I64TruncSatF32S
                  <|> do checkEq decodeU32 5
                         pure I64TruncSatF32U
                  <|> do checkEq decodeU32 6
                         pure I64TruncSatF64S
                  <|> do checkEq decodeU32 7
                         pure I64TruncSatF64U

encodeNumInstr' :: NumInstr' -> Builder ()
encodeNumInstr' I32TruncSatF32S = encodeU32 0
encodeNumInstr' I32TruncSatF32U = encodeU32 1
encodeNumInstr' I32TruncSatF64S = encodeU32 2
encodeNumInstr' I32TruncSatF64U = encodeU32 3
encodeNumInstr' I64TruncSatF32S = encodeU32 4
encodeNumInstr' I64TruncSatF32U = encodeU32 5
encodeNumInstr' I64TruncSatF64S = encodeU32 6
encodeNumInstr' I64TruncSatF64U = encodeU32 7

decodeExpr :: Parser Expr
decodeExpr = do ins <- P.many decodeInstr
                P.word8 11
                pure (Expr ins)

encodeExpr :: Expr -> Builder ()
encodeExpr (Expr ins) = do mapM_ encodeInstr ins
                           B.word8 11

decodeTypeIdx :: Parser TypeIdx
decodeTypeIdx = decodeU32

encodeTypeIdx :: TypeIdx -> Builder ()
encodeTypeIdx = encodeU32

decodeFuncIdx :: Parser FuncIdx
decodeFuncIdx = decodeU32

encodeFuncIdx :: FuncIdx -> Builder ()
encodeFuncIdx = encodeU32

decodeTableIdx :: Parser TableIdx
decodeTableIdx = decodeU32

encodeTableIdx :: TableIdx -> Builder ()
encodeTableIdx = encodeU32

decodeMemIdx :: Parser MemIdx
decodeMemIdx = decodeU32

encodeMemIdx :: MemIdx -> Builder ()
encodeMemIdx = encodeU32

decodeGlobalIdx :: Parser GlobalIdx
decodeGlobalIdx = decodeU32

encodeGlobalIdx :: GlobalIdx -> Builder ()
encodeGlobalIdx = encodeU32

decodeElemIdx :: Parser ElemIdx
decodeElemIdx = decodeU32

encodeElemIdx :: ElemIdx -> Builder ()
encodeElemIdx = encodeU32

decodeDataIdx :: Parser DataIdx
decodeDataIdx = decodeU32

encodeDataIdx :: DataIdx -> Builder ()
encodeDataIdx = encodeU32

decodeLocalIdx :: Parser LocalIdx
decodeLocalIdx = decodeU32

encodeLocalIdx :: LocalIdx -> Builder ()
encodeLocalIdx = encodeU32

decodeLabelIdx :: Parser LabelIdx
decodeLabelIdx = decodeU32

encodeLabelIdx :: LabelIdx -> Builder ()
encodeLabelIdx = encodeU32

decodeMagic :: Parser ()
decodeMagic = do P.word8 0
                 P.word8 97
                 P.word8 115
                 P.word8 109
                 pure ()

encodeMagic :: Builder ()
encodeMagic = do B.word8 0
                 B.word8 97
                 B.word8 115
                 B.word8 109

decodeVersion :: Parser ()
decodeVersion = do P.word8 16
                   P.word8 0
                   P.word8 0
                   P.word8 0
                   pure ()

encodeVersion :: Builder ()
encodeVersion = do B.word8 16
                   B.word8 0
                   B.word8 0
                   B.word8 0

decodeTypeSec :: Parser [FuncType]
decodeTypeSec = do P.word8 1
                   types <- decodeSection (decodeVec decodeFuncType)
                   pure types
                <|> pure []

encodeTypeSec :: [FuncType] -> Builder ()
encodeTypeSec [] = pure ()
encodeTypeSec types = do B.word8 1
                         encodeSection (encodeVec encodeFuncType) types

decodeImportSec :: Parser [Import]
decodeImportSec = do P.word8 2
                     imports <- decodeSection (decodeVec decodeImport)
                     pure imports
                  <|> pure []

encodeImportSec :: [Import] -> Builder ()
encodeImportSec [] = pure ()
encodeImportSec imports = do B.word8 2
                             encodeSection (encodeVec encodeImport) imports

decodeImport :: Parser Import
decodeImport = do mod <- decodeName
                  nm <- decodeName
                  d <- decodeImportDesc
                  pure (Import mod nm d)

encodeImport :: Import -> Builder ()
encodeImport (Import mod nm d) = do encodeName mod
                                    encodeName nm
                                    encodeImportDesc d

decodeImportDesc :: Parser ImportDesc
decodeImportDesc = do P.word8 0
                      x <- decodeTypeIdx
                      pure (ImportFunc x)
                   <|> do P.word8 1
                          tt <- decodeTableType
                          pure (ImportTable tt)
                   <|> do P.word8 2
                          mt <- decodeMemType
                          pure (ImportMemory mt)
                   <|> do P.word8 3
                          gt <- decodeGlobalType
                          pure (ImportGlobal gt)

encodeImportDesc :: ImportDesc -> Builder ()
encodeImportDesc (ImportFunc x) = do B.word8 0
                                     encodeTypeIdx x
encodeImportDesc (ImportTable tt) = do B.word8 1
                                       encodeTableType tt
encodeImportDesc (ImportMemory mt) = do B.word8 2
                                        encodeMemType mt
encodeImportDesc (ImportGlobal gt) = do B.word8 3
                                        encodeGlobalType gt

decodeFuncSec :: Parser [TypeIdx]
decodeFuncSec = do P.word8 3
                   x <- decodeSection (decodeVec decodeTypeIdx)
                   pure x
                <|> pure []

encodeFuncSec :: [TypeIdx] -> Builder ()
encodeFuncSec [] = pure ()
encodeFuncSec x = do B.word8 3
                     encodeSection (encodeVec encodeTypeIdx) x

decodeTableSec :: Parser [Table]
decodeTableSec = do P.word8 4
                    tabs <- decodeSection (decodeVec decodeTable)
                    pure tabs
                 <|> pure []

encodeTableSec :: [Table] -> Builder ()
encodeTableSec [] = pure ()
encodeTableSec tabs = do B.word8 4
                         encodeSection (encodeVec encodeTable) tabs

decodeTable :: Parser Table
decodeTable = do tt <- decodeTableType
                 pure (Table tt)

encodeTable :: Table -> Builder ()
encodeTable (Table tt) = encodeTableType tt

decodeMemSec :: Parser [Mem]
decodeMemSec = do P.word8 5
                  mems <- decodeSection (decodeVec decodeMem)
                  pure mems
               <|> pure []

encodeMemSec :: [Mem] -> Builder ()
encodeMemSec [] = pure ()
encodeMemSec mems = do B.word8 5
                       encodeSection (encodeVec encodeMem) mems

decodeMem :: Parser Mem
decodeMem = do mt <- decodeMemType
               pure (Mem mt)

encodeMem :: Mem -> Builder ()
encodeMem (Mem mt) = encodeMemType mt

decodeGlobalSec :: Parser [Global]
decodeGlobalSec = do P.word8 6
                     globs <- decodeSection (decodeVec decodeGlobal)
                     pure globs
                  <|> pure []

encodeGlobalSec :: [Global] -> Builder ()
encodeGlobalSec [] = pure ()
encodeGlobalSec globs = do B.word8 6
                           encodeSection (encodeVec encodeGlobal) globs

decodeGlobal :: Parser Global
decodeGlobal = do gt <- decodeGlobalType
                  e <- decodeExpr
                  pure (Global gt e)

encodeGlobal :: Global -> Builder ()
encodeGlobal (Global gt e) = do encodeGlobalType gt
                                encodeExpr e

decodeExportSec :: Parser [Export]
decodeExportSec = do P.word8 7
                     exs <- decodeSection (decodeVec decodeExport)
                     pure exs
                  <|> pure []

encodeExportSec :: [Export] -> Builder ()
encodeExportSec [] = pure ()
encodeExportSec exs = do B.word8 7
                         encodeSection (encodeVec encodeExport) exs

decodeExport :: Parser Export
decodeExport = do nm <- decodeName
                  d <- decodeExportDesc
                  pure (Export nm d)

encodeExport :: Export -> Builder ()
encodeExport (Export nm d) = do encodeName nm
                                encodeExportDesc d

decodeExportDesc :: Parser ExportDesc
decodeExportDesc = do P.word8 0
                      x <- decodeFuncIdx
                      pure (ExportFunc x)
                   <|> do P.word8 1
                          x <- decodeTableIdx
                          pure (ExportTable x)
                   <|> do P.word8 2
                          x <- decodeMemIdx
                          pure (ExportMem x)
                   <|> do P.word8 3
                          x <- decodeGlobalIdx
                          pure (ExportGlobal x)

encodeExportDesc :: ExportDesc -> Builder ()
encodeExportDesc (ExportFunc x) = do B.word8 0
                                     encodeFuncIdx x
encodeExportDesc (ExportTable x) = do B.word8 1
                                      encodeTableIdx x
encodeExportDesc (ExportMem x) = do B.word8 2
                                    encodeMemIdx x
encodeExportDesc (ExportGlobal x) = do B.word8 3
                                       encodeGlobalIdx x

decodeStartSec :: Parser Start
decodeStartSec = do P.word8 8
                    x <- decodeSection decodeFuncIdx
                    pure (StartAt x)
                 <|> pure NoStart

encodeStartSec :: Start -> Builder ()
encodeStartSec NoStart = pure ()
encodeStartSec (StartAt x) = do B.word8 8
                                encodeSection encodeFuncIdx x

decodeElemSec :: Parser [Elem]
decodeElemSec = do P.word8 9
                   segs <- decodeSection (decodeVec decodeElem)
                   pure segs
                <|> pure []

encodeElemSec :: [Elem] -> Builder ()
encodeElemSec [] = pure ()
encodeElemSec segs = do B.word8 9
                        encodeSection (encodeVec encodeElem) segs

decodeCodeSec :: Parser [Code]
decodeCodeSec = do P.word8 10
                   codes <- decodeSection (decodeVec decodeCode)
                   pure codes
                <|> pure []

encodeCodeSec :: [Code] -> Builder ()
encodeCodeSec [] = pure ()
encodeCodeSec codes = do B.word8 10
                         encodeSection (encodeVec encodeCode) codes

decodeCode :: Parser Code
decodeCode = do ts <- decodeVec decodeValType
                e <- decodeExpr
                pure (Code ts e)

encodeCode :: Code -> Builder ()
encodeCode (Code ts e) = do encodeVec encodeValType ts
                            encodeExpr e

decodeLocals :: Parser [ValType]
decodeLocals = decodeVec decodeValType

encodeLocals :: [ValType] -> Builder ()
encodeLocals = encodeVec encodeValType

decodeDataSec :: Parser [Data]
decodeDataSec = do P.word8 11
                   datas <- decodeSection (decodeVec decodeData)
                   pure datas
                <|> pure []

encodeDataSec :: [Data] -> Builder ()
encodeDataSec [] = pure ()
encodeDataSec datas = do B.word8 11
                         encodeSection (encodeVec encodeData) datas

decodeData :: Parser Data
decodeData = do P.word8 0
                e <- decodeExpr
                bs <- decodeBytes
                pure (Data bs (DActive 0 e))
             <|> do P.word8 1
                    bs <- decodeBytes
                    pure (Data bs DPassive)
             <|> do P.word8 2
                    x <- decodeMemIdx
                    e <- decodeExpr
                    bs <- decodeBytes
                    pure (Data bs (DActive x e))

encodeData :: Data -> Builder ()
encodeData (Data bs (DActive 0 e)) = do B.word8 0
                                        encodeExpr e
                                        encodeBytes bs
encodeData (Data bs DPassive) = do B.word8 1
                                   encodeBytes bs
encodeData (Data bs (DActive x e)) = do B.word8 2
                                        encodeMemIdx x
                                        encodeExpr e
                                        encodeBytes bs

decodeDatacountSec :: Parser U32
decodeDatacountSec = do P.word8 12
                        x <- decodeSection decodeU32
                        pure x

encodeDatacountSec :: U32 -> Builder ()
encodeDatacountSec x = do B.word8 12
                          encodeSection encodeU32 x

decodeElem :: Parser Elem
decodeElem = do P.word8 0
                e <- decodeExpr
                ys <- decodeVec decodeFuncIdx
                pure (Elem FuncRef (RefFuncs ys) (EActive 0 e))
             <|> do P.word8 1
                    P.word8 0
                    ys <- decodeVec decodeFuncIdx
                    pure (Elem FuncRef (RefFuncs ys) EPassive)
             <|> do P.word8 2
                    x <- decodeTableIdx
                    e <- decodeExpr
                    P.word8 0
                    ys <- decodeVec decodeFuncIdx
                    pure (Elem FuncRef (RefFuncs ys) (EActive x e))
             <|> do P.word8 3
                    P.word8 0
                    ys <- decodeVec decodeFuncIdx
                    pure (Elem FuncRef (RefFuncs ys) EDeclarative)
             <|> do P.word8 4
                    e <- decodeExpr
                    els <- decodeVec decodeExpr
                    pure (Elem FuncRef els (EActive 0 e))
             <|> do P.word8 5
                    et <- decodeRefType
                    els <- decodeVec decodeExpr
                    pure (Elem et els EPassive)
             <|> do P.word8 6
                    x <- decodeTableIdx
                    e <- decodeExpr
                    et <- decodeRefType
                    els <- decodeVec decodeExpr
                    pure (Elem et els (EActive x e))
             <|> do P.word8 7
                    et <- decodeRefType
                    els <- decodeVec decodeExpr
                    pure (Elem et els EDeclarative)

encodeElem :: Elem -> Builder ()
encodeElem (Elem FuncRef (RefFuncs ys) (EActive 0 e)) = do B.word8 0
                                                           encodeExpr e
                                                           encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) EPassive) = do B.word8 1
                                                      B.word8 0
                                                      encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) (EActive x e)) = do B.word8 2
                                                           encodeTableIdx x
                                                           encodeExpr e
                                                           B.word8 0
                                                           encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) EDeclarative) = do B.word8 3
                                                          B.word8 0
                                                          encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef els (EActive 0 e)) = do B.word8 4
                                                 encodeExpr e
                                                 encodeVec encodeExpr els
encodeElem (Elem et els EPassive) = do B.word8 5
                                       encodeRefType et
                                       encodeVec encodeExpr els
encodeElem (Elem et els (EActive x e)) = do B.word8 6
                                            encodeTableIdx x
                                            encodeExpr e
                                            encodeRefType et
                                            encodeVec encodeExpr els
encodeElem (Elem et els EDeclarative) = do B.word8 7
                                           encodeRefType et
                                           encodeVec encodeExpr els

decodeModule :: Parser Module
decodeModule = do decodeMagic
                  decodeVersion
                  types <- decodeTypeSec
                  imports <- decodeImportSec
                  typeidxs <- decodeFuncSec
                  tables <- decodeTableSec
                  mems <- decodeMemSec
                  globals <- decodeGlobalSec
                  exports <- decodeExportSec
                  start <- decodeStartSec
                  elems <- decodeElemSec
                  m <- decodeDatacountSec
                  codes <- decodeCodeSec
                  datas <- decodeDataSec
                  pure (Module types typeidxs codes tables mems globals elems m datas start imports exports)

encodeModule :: Module -> Builder ()
encodeModule (Module types typeidxs codes tables mems globals elems m datas start imports exports) = do encodeMagic
                                                                                                        encodeVersion
                                                                                                        encodeTypeSec types
                                                                                                        encodeImportSec imports
                                                                                                        encodeFuncSec typeidxs
                                                                                                        encodeTableSec tables
                                                                                                        encodeMemSec mems
                                                                                                        encodeGlobalSec globals
                                                                                                        encodeExportSec exports
                                                                                                        encodeStartSec start
                                                                                                        encodeElemSec elems
                                                                                                        encodeDatacountSec m
                                                                                                        encodeCodeSec codes
                                                                                                        encodeDataSec datas

-- 解析Instr
decodeInstr :: Parser Instr
decodeInstr = NumInstr <$> decodeNumInstr
    <|> RefInstr <$> decodeRefInstr
    <|> ParamInstr <$> decodeParamInstr
    <|> VarInstr <$> decodeVarInstr
    <|> TableInstr <$> decodeTableInstr
    <|> MemoryInstr <$> decodeMemoryInstr
    <|> CtlInstr <$> decodeCtlInstr
    <|> do
        P.word8 0xFC
        NumInstr' <$> decodeNumInstr'
            <|> TableInstr' <$> decodeTableInstr'
            <|> MemoryInstr' <$> decodeMemoryInstr'

encodeInstr :: Instr -> Builder ()
encodeInstr = go
    where
        go (NumInstr i) = encodeNumInstr i
        go (RefInstr i) = encodeRefInstr i
        go (ParamInstr i) = encodeParamInstr i
        go (VarInstr i) = encodeVarInstr i
        go (TableInstr i) = encodeTableInstr i
        go (MemoryInstr i) = encodeMemoryInstr i
        go (CtlInstr i) = encodeCtlInstr i
        go (NumInstr' i) = B.word8 0xFC >> encodeNumInstr' i
        go (TableInstr' i) = B.word8 0xFC >> encodeTableInstr' i
        go (MemoryInstr' i) = B.word8 0xFC >> encodeMemoryInstr' i
