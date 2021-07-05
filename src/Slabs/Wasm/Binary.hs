module Slabs.Wasm.Binary  where

import Control.Applicative 
import Slabs.Wasm.Structure 
import Slabs.Wasm.Internal 

import Z.Data.Parser (Parser, (<?>))
import qualified Z.Data.Parser as P 
import qualified Control.Monad.Combinators as P 
import Z.Data.Builder (Builder)
import qualified Z.Data.Builder as B 

decodeNumType :: Parser NumType
decodeNumType
  = "NumType" <?> do P.word8 127
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
decodeRefType
  = "RefType" <?> do P.word8 112
                     pure FuncRef
                  <|> do P.word8 111
                         pure ExternRef

encodeRefType :: RefType -> Builder ()
encodeRefType FuncRef = B.word8 112
encodeRefType ExternRef = B.word8 111

decodeValType :: Parser ValType
decodeValType
  = "ValType" <?> do t <- decodeNumType
                     pure (NumType t)
                  <|> do t <- decodeRefType
                         pure (RefType t)

encodeValType :: ValType -> Builder ()
encodeValType (NumType t) = encodeNumType t
encodeValType (RefType t) = encodeRefType t

decodeResultType :: Parser ResultType
decodeResultType = "ResultType" <?> decodeVec decodeValType

encodeResultType :: ResultType -> Builder ()
encodeResultType = encodeVec encodeValType

decodeFuncType :: Parser FuncType
decodeFuncType
  = "FuncType" <?> do P.word8 96
                      rt1 <- decodeResultType
                      rt2 <- decodeResultType
                      pure (FuncType rt1 rt2)

encodeFuncType :: FuncType -> Builder ()
encodeFuncType (FuncType rt1 rt2)
  = do B.word8 96
       encodeResultType rt1
       encodeResultType rt2

decodeLimits :: Parser Limits
decodeLimits
  = "Limits" <?> do P.word8 0
                    n <- decodeU32
                    pure (Limits n Nothing)
                 <|> do P.word8 1
                        n <- decodeU32
                        m <- decodeU32
                        pure (Limits n (Just m))

encodeLimits :: Limits -> Builder ()
encodeLimits (Limits n Nothing)
  = do B.word8 0
       encodeU32 n
encodeLimits (Limits n (Just m))
  = do B.word8 1
       encodeU32 n
       encodeU32 m

decodeMemType :: Parser MemType
decodeMemType = "MemType" <?> decodeLimits

encodeMemType :: MemType -> Builder ()
encodeMemType = encodeLimits

decodeTableType :: Parser TableType
decodeTableType
  = "TableType" <?> do et <- decodeRefType
                       lim <- decodeLimits
                       pure (TableType lim et)

encodeTableType :: TableType -> Builder ()
encodeTableType (TableType lim et)
  = do encodeRefType et
       encodeLimits lim

decodeGlobalType :: Parser GlobalType
decodeGlobalType
  = "GlobalType" <?> do t <- decodeValType
                        m <- decodeMut
                        pure (GlobalType m t)

encodeGlobalType :: GlobalType -> Builder ()
encodeGlobalType (GlobalType m t)
  = do encodeValType t
       encodeMut m

decodeMut :: Parser Mut
decodeMut
  = "Mut" <?> do P.word8 0
                 pure MutConst
              <|> do P.word8 1
                     pure MutVar

encodeMut :: Mut -> Builder ()
encodeMut MutConst = B.word8 0
encodeMut MutVar = B.word8 1

decodeBlockType :: Parser BlockType
decodeBlockType
  = "BlockType" <?> do P.word8 64
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
decodeElseInstrs
  = "ElseInstrs" <?> do P.word8 11
                        pure []
                     <|> do P.word8 5
                            ins <- P.many decodeInstr
                            P.word8 11
                            pure ins

encodeElseInstrs :: [Instr] -> Builder ()
encodeElseInstrs [] = B.word8 11
encodeElseInstrs ins
  = do B.word8 5
       mapM_ encodeInstr ins
       B.word8 11

decodeMemArg :: Parser MemArg
decodeMemArg
  = "MemArg" <?> do a <- decodeU32
                    b <- decodeU32
                    pure (MemArg a b)

encodeMemArg :: MemArg -> Builder ()
encodeMemArg (MemArg a b)
  = do encodeU32 a
       encodeU32 b

decodeInstr :: Parser Instr
decodeInstr
  = "Instr" <?> do P.word8 0
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
                <|> do P.word8 26
                       pure Drop
                <|> do P.word8 27
                       pure (Select [])
                <|> do P.word8 28
                       ts <- decodeVec decodeValType
                       pure (Select ts)
                <|> do P.word8 32
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
                <|> do P.word8 37
                       x <- decodeTableIdx
                       pure (TableGet x)
                <|> do P.word8 38
                       x <- decodeTableIdx
                       pure (TableSet x)
                <|> do P.word8 40
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
                <|> do P.word8 252
                       decodeU32 >>= \case
                           8 -> do x <- decodeDataIdx
                                   P.word8 0
                                   pure (MemoryInit x)
                           9 -> do x <- decodeDataIdx
                                   pure (DataDrop x)
                           10 -> do P.word8 0
                                    P.word8 0
                                    pure MemoryCopy
                           11 -> do P.word8 0
                                    pure MemoryFill
                           0 -> pure I32TruncSatF32S
                           1 -> pure I32TruncSatF32U
                           2 -> pure I32TruncSatF64S
                           3 -> pure I32TruncSatF64U
                           4 -> pure I64TruncSatF32S
                           5 -> pure I64TruncSatF32U
                           6 -> pure I64TruncSatF64S
                           7 -> pure I64TruncSatF64U
                           12 -> do y <- decodeElemIdx
                                    x <- decodeTableIdx
                                    pure (TableInit x y)
                           13 -> do x <- decodeElemIdx
                                    pure (ElemDrop x)
                           14 -> do x <- decodeTableIdx
                                    y <- decodeTableIdx
                                    pure (TableCopy x y)
                           15 -> do x <- decodeTableIdx
                                    pure (TableGrow x)
                           16 -> do x <- decodeTableIdx
                                    pure (TableSize x)
                           17 -> do x <- decodeTableIdx
                                    pure (TableFill x)
                           _ -> fail "mismatch"
                <|> do P.word8 65
                       n <- decodeI32
                       pure (I32Const n)
                <|> do P.word8 66
                       n <- decodeI64
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
                <|> do P.word8 208
                       t <- decodeRefType
                       pure (RefNull t)
                <|> do P.word8 209
                       pure RefIsNull
                <|> do P.word8 210
                       x <- decodeFuncIdx
                       pure (RefFunc x)

encodeInstr :: Instr -> Builder ()
encodeInstr Unreachable = B.word8 0
encodeInstr Nop = B.word8 1
encodeInstr (Block bt ins)
  = do B.word8 2
       encodeBlockType bt
       mapM_ encodeInstr ins
       B.word8 11
encodeInstr (Loop bt ins)
  = do B.word8 3
       encodeBlockType bt
       mapM_ encodeInstr ins
       B.word8 11
encodeInstr (If bt in1 in2)
  = do B.word8 4
       encodeBlockType bt
       mapM_ encodeInstr in1
       encodeElseInstrs in2
encodeInstr (Br l)
  = do B.word8 12
       encodeLabelIdx l
encodeInstr (BrIf l)
  = do B.word8 13
       encodeLabelIdx l
encodeInstr (BrTable ls ln)
  = do B.word8 14
       encodeVec encodeLabelIdx ls
       encodeLabelIdx ln
encodeInstr Return = B.word8 15
encodeInstr (Call x)
  = do B.word8 16
       encodeFuncIdx x
encodeInstr (CallIndirect x y)
  = do B.word8 17
       encodeTypeIdx y
       encodeTableIdx x
encodeInstr Drop = B.word8 26
encodeInstr (Select []) = B.word8 27
encodeInstr (Select ts)
  = do B.word8 28
       encodeVec encodeValType ts
encodeInstr (LocalGet x)
  = do B.word8 32
       encodeLocalIdx x
encodeInstr (LocalSet x)
  = do B.word8 33
       encodeLocalIdx x
encodeInstr (LocalTee x)
  = do B.word8 34
       encodeLocalIdx x
encodeInstr (GlobalGet x)
  = do B.word8 35
       encodeGlobalIdx x
encodeInstr (GlobalSet x)
  = do B.word8 36
       encodeGlobalIdx x
encodeInstr (TableGet x)
  = do B.word8 37
       encodeTableIdx x
encodeInstr (TableSet x)
  = do B.word8 38
       encodeTableIdx x
encodeInstr (I32Load m)
  = do B.word8 40
       encodeMemArg m
encodeInstr (I64Load m)
  = do B.word8 41
       encodeMemArg m
encodeInstr (F32Load m)
  = do B.word8 42
       encodeMemArg m
encodeInstr (F64Load m)
  = do B.word8 43
       encodeMemArg m
encodeInstr (I32Load8S m)
  = do B.word8 44
       encodeMemArg m
encodeInstr (I32Load8U m)
  = do B.word8 45
       encodeMemArg m
encodeInstr (I32Load16S m)
  = do B.word8 46
       encodeMemArg m
encodeInstr (I32Load16U m)
  = do B.word8 47
       encodeMemArg m
encodeInstr (I64Load8S m)
  = do B.word8 48
       encodeMemArg m
encodeInstr (I64Load8U m)
  = do B.word8 49
       encodeMemArg m
encodeInstr (I64Load16S m)
  = do B.word8 50
       encodeMemArg m
encodeInstr (I64Load16U m)
  = do B.word8 51
       encodeMemArg m
encodeInstr (I64Load32S m)
  = do B.word8 52
       encodeMemArg m
encodeInstr (I64Load32U m)
  = do B.word8 53
       encodeMemArg m
encodeInstr (I32Store m)
  = do B.word8 54
       encodeMemArg m
encodeInstr (I64Store m)
  = do B.word8 55
       encodeMemArg m
encodeInstr (F32Store m)
  = do B.word8 56
       encodeMemArg m
encodeInstr (F64Store m)
  = do B.word8 57
       encodeMemArg m
encodeInstr (I32Store8 m)
  = do B.word8 58
       encodeMemArg m
encodeInstr (I32Store16 m)
  = do B.word8 59
       encodeMemArg m
encodeInstr (I64Store8 m)
  = do B.word8 60
       encodeMemArg m
encodeInstr (I64Store16 m)
  = do B.word8 61
       encodeMemArg m
encodeInstr (I64Store32 m)
  = do B.word8 62
       encodeMemArg m
encodeInstr MemorySize
  = do B.word8 63
       B.word8 0
encodeInstr MemoryGrow
  = do B.word8 64
       B.word8 0
encodeInstr (MemoryInit x)
  = do B.word8 252
       encodeU32 8
       encodeDataIdx x
       B.word8 0
encodeInstr (DataDrop x)
  = do B.word8 252
       encodeU32 9
       encodeDataIdx x
encodeInstr MemoryCopy
  = do B.word8 252
       encodeU32 10
       B.word8 0
       B.word8 0
encodeInstr MemoryFill
  = do B.word8 252
       encodeU32 11
       B.word8 0
encodeInstr (I32Const n)
  = do B.word8 65
       encodeI32 n
encodeInstr (I64Const n)
  = do B.word8 66
       encodeI64 n
encodeInstr (F32Const z)
  = do B.word8 67
       encodeF32 z
encodeInstr (F64Const z)
  = do B.word8 68
       encodeF64 z
encodeInstr I32Eqz = B.word8 69
encodeInstr (I32RelOp IEq) = B.word8 70
encodeInstr (I32RelOp INe) = B.word8 71
encodeInstr (I32RelOp ILtS) = B.word8 72
encodeInstr (I32RelOp ILtU) = B.word8 73
encodeInstr (I32RelOp IGtS) = B.word8 74
encodeInstr (I32RelOp IGtU) = B.word8 75
encodeInstr (I32RelOp ILeS) = B.word8 76
encodeInstr (I32RelOp ILeU) = B.word8 77
encodeInstr (I32RelOp IGeS) = B.word8 78
encodeInstr (I32RelOp IGeU) = B.word8 79
encodeInstr I64Eqz = B.word8 80
encodeInstr (I64RelOp IEq) = B.word8 81
encodeInstr (I64RelOp INe) = B.word8 82
encodeInstr (I64RelOp ILtS) = B.word8 83
encodeInstr (I64RelOp ILtU) = B.word8 84
encodeInstr (I64RelOp IGtS) = B.word8 85
encodeInstr (I64RelOp IGtU) = B.word8 86
encodeInstr (I64RelOp ILeS) = B.word8 87
encodeInstr (I64RelOp ILeU) = B.word8 88
encodeInstr (I64RelOp IGeS) = B.word8 89
encodeInstr (I64RelOp IGeU) = B.word8 90
encodeInstr (F32RelOp FEq) = B.word8 91
encodeInstr (F32RelOp FNe) = B.word8 92
encodeInstr (F32RelOp FLt) = B.word8 93
encodeInstr (F32RelOp FGt) = B.word8 94
encodeInstr (F32RelOp FLe) = B.word8 95
encodeInstr (F32RelOp FGe) = B.word8 96
encodeInstr (F64RelOp FEq) = B.word8 97
encodeInstr (F64RelOp FNe) = B.word8 98
encodeInstr (F64RelOp FLt) = B.word8 99
encodeInstr (F64RelOp FGt) = B.word8 100
encodeInstr (F64RelOp FLe) = B.word8 101
encodeInstr (F64RelOp FGe) = B.word8 102
encodeInstr (I32UnOp IClz) = B.word8 103
encodeInstr (I32UnOp ICtz) = B.word8 104
encodeInstr (I32UnOp IPopcnt) = B.word8 105
encodeInstr (I32BinOp IAdd) = B.word8 106
encodeInstr (I32BinOp ISub) = B.word8 107
encodeInstr (I32BinOp IMul) = B.word8 108
encodeInstr (I32BinOp IDivS) = B.word8 109
encodeInstr (I32BinOp IDivU) = B.word8 110
encodeInstr (I32BinOp IRemS) = B.word8 111
encodeInstr (I32BinOp IRemU) = B.word8 112
encodeInstr (I32BinOp IAnd) = B.word8 113
encodeInstr (I32BinOp IOr) = B.word8 114
encodeInstr (I32BinOp IXor) = B.word8 115
encodeInstr (I32BinOp IShl) = B.word8 116
encodeInstr (I32BinOp IShrS) = B.word8 117
encodeInstr (I32BinOp IShrU) = B.word8 118
encodeInstr (I32BinOp IRotl) = B.word8 119
encodeInstr (I32BinOp IRotr) = B.word8 120
encodeInstr (I64UnOp IClz) = B.word8 121
encodeInstr (I64UnOp ICtz) = B.word8 122
encodeInstr (I64UnOp IPopcnt) = B.word8 123
encodeInstr (I64BinOp IAdd) = B.word8 124
encodeInstr (I64BinOp ISub) = B.word8 125
encodeInstr (I64BinOp IMul) = B.word8 126
encodeInstr (I64BinOp IDivS) = B.word8 127
encodeInstr (I64BinOp IDivU) = B.word8 128
encodeInstr (I64BinOp IRemS) = B.word8 129
encodeInstr (I64BinOp IRemU) = B.word8 130
encodeInstr (I64BinOp IAnd) = B.word8 131
encodeInstr (I64BinOp IOr) = B.word8 132
encodeInstr (I64BinOp IXor) = B.word8 133
encodeInstr (I64BinOp IShl) = B.word8 134
encodeInstr (I64BinOp IShrS) = B.word8 135
encodeInstr (I64BinOp IShrU) = B.word8 136
encodeInstr (I64BinOp IRotl) = B.word8 137
encodeInstr (I64BinOp IRotr) = B.word8 138
encodeInstr (F32UnOp FAbs) = B.word8 139
encodeInstr (F32UnOp FNeg) = B.word8 140
encodeInstr (F32UnOp FCeil) = B.word8 141
encodeInstr (F32UnOp FFloor) = B.word8 142
encodeInstr (F32UnOp FTrunc) = B.word8 143
encodeInstr (F32UnOp FNearest) = B.word8 144
encodeInstr (F32UnOp FSqrt) = B.word8 145
encodeInstr (F32BinOp FAdd) = B.word8 146
encodeInstr (F32BinOp FSub) = B.word8 147
encodeInstr (F32BinOp FMul) = B.word8 148
encodeInstr (F32BinOp FDiv) = B.word8 149
encodeInstr (F32BinOp FMin) = B.word8 150
encodeInstr (F32BinOp FMax) = B.word8 151
encodeInstr (F32BinOp FCopysign) = B.word8 152
encodeInstr (F64UnOp FAbs) = B.word8 153
encodeInstr (F64UnOp FNeg) = B.word8 154
encodeInstr (F64UnOp FCeil) = B.word8 155
encodeInstr (F64UnOp FFloor) = B.word8 156
encodeInstr (F64UnOp FTrunc) = B.word8 157
encodeInstr (F64UnOp FNearest) = B.word8 158
encodeInstr (F64UnOp FSqrt) = B.word8 159
encodeInstr (F64BinOp FAdd) = B.word8 160
encodeInstr (F64BinOp FSub) = B.word8 161
encodeInstr (F64BinOp FMul) = B.word8 162
encodeInstr (F64BinOp FDiv) = B.word8 163
encodeInstr (F64BinOp FMin) = B.word8 164
encodeInstr (F64BinOp FMax) = B.word8 165
encodeInstr (F64BinOp FCopysign) = B.word8 166
encodeInstr I32WrapI64 = B.word8 167
encodeInstr I32TruncF32S = B.word8 168
encodeInstr I32TruncF32U = B.word8 169
encodeInstr I32TruncF64S = B.word8 170
encodeInstr I32TruncF64U = B.word8 171
encodeInstr I64ExtendI32S = B.word8 172
encodeInstr I64ExtendI32U = B.word8 173
encodeInstr I64TruncF32S = B.word8 174
encodeInstr I64TruncF32U = B.word8 175
encodeInstr I64TruncF64S = B.word8 176
encodeInstr I64TruncF64U = B.word8 177
encodeInstr F32ConvertI32S = B.word8 178
encodeInstr F32ConvertI32U = B.word8 179
encodeInstr F32ConvertI64S = B.word8 180
encodeInstr F32ConvertI64U = B.word8 181
encodeInstr F32DemoteF64 = B.word8 182
encodeInstr F64ConvertI32S = B.word8 183
encodeInstr F64ConvertI32U = B.word8 184
encodeInstr F64ConvertI64S = B.word8 185
encodeInstr F64ConvertI64U = B.word8 186
encodeInstr F64PromoteF32 = B.word8 187
encodeInstr I32ReinterpretF32 = B.word8 188
encodeInstr I64ReinterpretF64 = B.word8 189
encodeInstr F32ReinterpretI32 = B.word8 190
encodeInstr F64ReinterpretI64 = B.word8 191
encodeInstr I32Extend8S = B.word8 192
encodeInstr I32Extend16S = B.word8 193
encodeInstr I64Extend8S = B.word8 194
encodeInstr I64Extend16S = B.word8 195
encodeInstr I64Extend32S = B.word8 196
encodeInstr (RefNull t)
  = do B.word8 208
       encodeRefType t
encodeInstr RefIsNull = B.word8 209
encodeInstr (RefFunc x)
  = do B.word8 210
       encodeFuncIdx x
encodeInstr I32TruncSatF32S
  = do B.word8 252
       encodeU32 0
encodeInstr I32TruncSatF32U
  = do B.word8 252
       encodeU32 1
encodeInstr I32TruncSatF64S
  = do B.word8 252
       encodeU32 2
encodeInstr I32TruncSatF64U
  = do B.word8 252
       encodeU32 3
encodeInstr I64TruncSatF32S
  = do B.word8 252
       encodeU32 4
encodeInstr I64TruncSatF32U
  = do B.word8 252
       encodeU32 5
encodeInstr I64TruncSatF64S
  = do B.word8 252
       encodeU32 6
encodeInstr I64TruncSatF64U
  = do B.word8 252
       encodeU32 7
encodeInstr (TableInit x y)
  = do B.word8 252
       encodeU32 12
       encodeElemIdx y
       encodeTableIdx x
encodeInstr (ElemDrop x)
  = do B.word8 252
       encodeU32 13
       encodeElemIdx x
encodeInstr (TableCopy x y)
  = do B.word8 252
       encodeU32 14
       encodeTableIdx x
       encodeTableIdx y
encodeInstr (TableGrow x)
  = do B.word8 252
       encodeU32 15
       encodeTableIdx x
encodeInstr (TableSize x)
  = do B.word8 252
       encodeU32 16
       encodeTableIdx x
encodeInstr (TableFill x)
  = do B.word8 252
       encodeU32 17
       encodeTableIdx x

decodeExpr :: Parser Expr
decodeExpr
  = "Expr" <?> do ins <- P.many decodeInstr
                  P.word8 11
                  pure (Expr ins)

encodeExpr :: Expr -> Builder ()
encodeExpr (Expr ins)
  = do mapM_ encodeInstr ins
       B.word8 11

decodeTypeIdx :: Parser TypeIdx
decodeTypeIdx = "TypeIdx" <?> decodeU32

encodeTypeIdx :: TypeIdx -> Builder ()
encodeTypeIdx = encodeU32

decodeFuncIdx :: Parser FuncIdx
decodeFuncIdx = "FuncIdx" <?> decodeU32

encodeFuncIdx :: FuncIdx -> Builder ()
encodeFuncIdx = encodeU32

decodeTableIdx :: Parser TableIdx
decodeTableIdx = "TableIdx" <?> decodeU32

encodeTableIdx :: TableIdx -> Builder ()
encodeTableIdx = encodeU32

decodeMemIdx :: Parser MemIdx
decodeMemIdx = "MemIdx" <?> decodeU32

encodeMemIdx :: MemIdx -> Builder ()
encodeMemIdx = encodeU32

decodeGlobalIdx :: Parser GlobalIdx
decodeGlobalIdx = "GlobalIdx" <?> decodeU32

encodeGlobalIdx :: GlobalIdx -> Builder ()
encodeGlobalIdx = encodeU32

decodeElemIdx :: Parser ElemIdx
decodeElemIdx = "ElemIdx" <?> decodeU32

encodeElemIdx :: ElemIdx -> Builder ()
encodeElemIdx = encodeU32

decodeDataIdx :: Parser DataIdx
decodeDataIdx = "DataIdx" <?> decodeU32

encodeDataIdx :: DataIdx -> Builder ()
encodeDataIdx = encodeU32

decodeLocalIdx :: Parser LocalIdx
decodeLocalIdx = "LocalIdx" <?> decodeU32

encodeLocalIdx :: LocalIdx -> Builder ()
encodeLocalIdx = encodeU32

decodeLabelIdx :: Parser LabelIdx
decodeLabelIdx = "LabelIdx" <?> decodeU32

encodeLabelIdx :: LabelIdx -> Builder ()
encodeLabelIdx = encodeU32

decodeMagic :: Parser ()
decodeMagic
  = "Magic" <?> do P.word8 0
                   P.word8 97
                   P.word8 115
                   P.word8 109
                   pure ()

encodeMagic :: Builder ()
encodeMagic
  = do B.word8 0
       B.word8 97
       B.word8 115
       B.word8 109

decodeVersion :: Parser ()
decodeVersion
  = "Version" <?> do P.word8 1
                     P.word8 0
                     P.word8 0
                     P.word8 0
                     pure ()

encodeVersion :: Builder ()
encodeVersion
  = do B.word8 1
       B.word8 0
       B.word8 0
       B.word8 0

decodeTypeSec :: Parser [FuncType]
decodeTypeSec = "TypeSec" <?> decodeVecSection 1 decodeFuncType

encodeTypeSec :: [FuncType] -> Builder ()
encodeTypeSec = encodeVecSection 1 encodeFuncType

decodeImportSec :: Parser [Import]
decodeImportSec = "ImportSec" <?> decodeVecSection 2 decodeImport

encodeImportSec :: [Import] -> Builder ()
encodeImportSec = encodeVecSection 2 encodeImport

decodeImport :: Parser Import
decodeImport
  = "Import" <?> do mod <- decodeName
                    nm <- decodeName
                    d <- decodeImportDesc
                    pure (Import mod nm d)

encodeImport :: Import -> Builder ()
encodeImport (Import mod nm d)
  = do encodeName mod
       encodeName nm
       encodeImportDesc d

decodeImportDesc :: Parser ImportDesc
decodeImportDesc
  = "ImportDesc" <?> do P.word8 0
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
encodeImportDesc (ImportFunc x)
  = do B.word8 0
       encodeTypeIdx x
encodeImportDesc (ImportTable tt)
  = do B.word8 1
       encodeTableType tt
encodeImportDesc (ImportMemory mt)
  = do B.word8 2
       encodeMemType mt
encodeImportDesc (ImportGlobal gt)
  = do B.word8 3
       encodeGlobalType gt

decodeFuncSec :: Parser [TypeIdx]
decodeFuncSec = "FuncSec" <?> decodeVecSection 3 decodeTypeIdx

encodeFuncSec :: [TypeIdx] -> Builder ()
encodeFuncSec = encodeVecSection 3 encodeTypeIdx

decodeTableSec :: Parser [Table]
decodeTableSec = "TableSec" <?> decodeVecSection 4 decodeTable

encodeTableSec :: [Table] -> Builder ()
encodeTableSec = encodeVecSection 4 encodeTable

decodeTable :: Parser Table
decodeTable
  = "Table" <?> do tt <- decodeTableType
                   pure (Table tt)

encodeTable :: Table -> Builder ()
encodeTable (Table tt) = encodeTableType tt

decodeMemSec :: Parser [Mem]
decodeMemSec = "MemSec" <?> decodeVecSection 5 decodeMem

encodeMemSec :: [Mem] -> Builder ()
encodeMemSec = encodeVecSection 5 encodeMem

decodeMem :: Parser Mem
decodeMem
  = "Mem" <?> do mt <- decodeMemType
                 pure (Mem mt)

encodeMem :: Mem -> Builder ()
encodeMem (Mem mt) = encodeMemType mt

decodeGlobalSec :: Parser [Global]
decodeGlobalSec = "GlobalSec" <?> decodeVecSection 6 decodeGlobal

encodeGlobalSec :: [Global] -> Builder ()
encodeGlobalSec = encodeVecSection 6 encodeGlobal

decodeGlobal :: Parser Global
decodeGlobal
  = "Global" <?> do gt <- decodeGlobalType
                    e <- decodeExpr
                    pure (Global gt e)

encodeGlobal :: Global -> Builder ()
encodeGlobal (Global gt e)
  = do encodeGlobalType gt
       encodeExpr e

decodeExportSec :: Parser [Export]
decodeExportSec = "ExportSec" <?> decodeVecSection 7 decodeExport

encodeExportSec :: [Export] -> Builder ()
encodeExportSec = encodeVecSection 7 encodeExport

decodeExport :: Parser Export
decodeExport
  = "Export" <?> do nm <- decodeName
                    d <- decodeExportDesc
                    pure (Export nm d)

encodeExport :: Export -> Builder ()
encodeExport (Export nm d)
  = do encodeName nm
       encodeExportDesc d

decodeExportDesc :: Parser ExportDesc
decodeExportDesc
  = "ExportDesc" <?> do P.word8 0
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
encodeExportDesc (ExportFunc x)
  = do B.word8 0
       encodeFuncIdx x
encodeExportDesc (ExportTable x)
  = do B.word8 1
       encodeTableIdx x
encodeExportDesc (ExportMem x)
  = do B.word8 2
       encodeMemIdx x
encodeExportDesc (ExportGlobal x)
  = do B.word8 3
       encodeGlobalIdx x

decodeStartSec :: Parser Start
decodeStartSec
  = "StartSec" <?> do P.word8 8
                      x <- decodeSection decodeFuncIdx
                      pure (StartAt x)
                   <|> pure NoStart

encodeStartSec :: Start -> Builder ()
encodeStartSec NoStart = pure ()
encodeStartSec (StartAt x)
  = do B.word8 8
       encodeSection encodeFuncIdx x

decodeElemSec :: Parser [Elem]
decodeElemSec = "ElemSec" <?> decodeVecSection 9 decodeElem

encodeElemSec :: [Elem] -> Builder ()
encodeElemSec = encodeVecSection 9 encodeElem

decodeCodeSec :: Parser [Code]
decodeCodeSec = "CodeSec" <?> decodeVecSection 10 (decodeEmbed decodeCode)

encodeCodeSec :: [Code] -> Builder ()
encodeCodeSec = encodeVecSection 10 (encodeEmbed encodeCode)

decodeCode :: Parser Code
decodeCode
  = "Code" <?> do ts <- decodeVec decodeLocals
                  e <- decodeExpr
                  pure (Code ts e)

encodeCode :: Code -> Builder ()
encodeCode (Code ts e)
  = do encodeVec encodeLocals ts
       encodeExpr e

decodeLocals :: Parser Locals
decodeLocals = "Locals" <?> decodeVec decodeValType

encodeLocals :: Locals -> Builder ()
encodeLocals = encodeVec encodeValType

decodeDataSec :: Parser [Data]
decodeDataSec = "DataSec" <?> decodeVecSection 11 decodeData

encodeDataSec :: [Data] -> Builder ()
encodeDataSec = encodeVecSection 11 encodeData

decodeData :: Parser Data
decodeData
  = "Data" <?> do P.word8 0
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
encodeData (Data bs (DActive 0 e))
  = do B.word8 0
       encodeExpr e
       encodeBytes bs
encodeData (Data bs DPassive)
  = do B.word8 1
       encodeBytes bs
encodeData (Data bs (DActive x e))
  = do B.word8 2
       encodeMemIdx x
       encodeExpr e
       encodeBytes bs

decodeDatacountSec :: Parser Datacount
decodeDatacountSec
  = "DatacountSec" <?> do P.word8 12
                          x <- decodeSection decodeU32
                          pure (Datacount x)
                       <|> pure NoDatacount

encodeDatacountSec :: Datacount -> Builder ()
encodeDatacountSec NoDatacount = pure ()
encodeDatacountSec (Datacount x)
  = do B.word8 12
       encodeSection encodeU32 x

decodeElem :: Parser Elem
decodeElem
  = "Elem" <?> do P.word8 0
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
encodeElem (Elem FuncRef (RefFuncs ys) (EActive 0 e))
  = do B.word8 0
       encodeExpr e
       encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) EPassive)
  = do B.word8 1
       B.word8 0
       encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) (EActive x e))
  = do B.word8 2
       encodeTableIdx x
       encodeExpr e
       B.word8 0
       encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef (RefFuncs ys) EDeclarative)
  = do B.word8 3
       B.word8 0
       encodeVec encodeFuncIdx ys
encodeElem (Elem FuncRef els (EActive 0 e))
  = do B.word8 4
       encodeExpr e
       encodeVec encodeExpr els
encodeElem (Elem et els EPassive)
  = do B.word8 5
       encodeRefType et
       encodeVec encodeExpr els
encodeElem (Elem et els (EActive x e))
  = do B.word8 6
       encodeTableIdx x
       encodeExpr e
       encodeRefType et
       encodeVec encodeExpr els
encodeElem (Elem et els EDeclarative)
  = do B.word8 7
       encodeRefType et
       encodeVec encodeExpr els

decodeModule :: Parser Module
decodeModule
  = "Module" <?> do decodeMagic
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
encodeModule (Module types typeidxs codes tables mems globals elems m datas start imports exports)
  = do encodeMagic
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
