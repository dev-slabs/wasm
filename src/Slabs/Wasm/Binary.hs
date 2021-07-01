module Slabs.Wasm.Binary  where

import Universum 
import Slabs.Wasm.Structure 
import Slabs.Wasm.Internal 

import Z.Data.Parser (Parser)
import qualified Z.Data.Parser as P 
import qualified Control.Monad.Combinators as P 
import Z.Data.Builder (Builder)
import qualified Z.Data.Builder as B 

parseNumType :: Parser NumType
parseNumType = do P.word8 127
                  pure I32
               <|> do P.word8 126
                      pure I64
               <|> do P.word8 125
                      pure F32
               <|> do P.word8 124
                      pure F64

buildNumType :: NumType -> Builder ()
buildNumType I32 = B.word8 127
buildNumType I64 = B.word8 126
buildNumType F32 = B.word8 125
buildNumType F64 = B.word8 124

parseRefType :: Parser RefType
parseRefType = do P.word8 112
                  pure FuncRef
               <|> do P.word8 111
                      pure ExternRef

buildRefType :: RefType -> Builder ()
buildRefType FuncRef = B.word8 112
buildRefType ExternRef = B.word8 111

parseValType :: Parser ValType
parseValType = do t <- parseNumType
                  pure (NumType t)
               <|> do t <- parseRefType
                      pure (RefType t)

buildValType :: ValType -> Builder ()
buildValType (NumType t) = buildNumType t
buildValType (RefType t) = buildRefType t

parseResultType :: Parser ResultType
parseResultType = do t <- (parseVec parseValType)
                     pure t

buildResultType :: ResultType -> Builder ()
buildResultType t = buildVec buildValType t

parseFuncType :: Parser FuncType
parseFuncType = do P.word8 96
                   rt1 <- parseResultType
                   rt2 <- parseResultType
                   pure (FuncType rt1 rt2)

buildFuncType :: FuncType -> Builder ()
buildFuncType (FuncType rt1 rt2) = do B.word8 96
                                      buildResultType rt1
                                      buildResultType rt2

parseLimits :: Parser Limits
parseLimits = do P.word8 0
                 n <- parseU32
                 pure (Limits n Nothing)
              <|> do P.word8 1
                     n <- parseU32
                     m <- parseU32
                     pure (Limits n (Just m))

buildLimits :: Limits -> Builder ()
buildLimits (Limits n Nothing) = do B.word8 0
                                    buildU32 n
buildLimits (Limits n (Just m)) = do B.word8 1
                                     buildU32 n
                                     buildU32 m

parseMemType :: Parser MemType
parseMemType = parseLimits

buildMemType :: MemType -> Builder ()
buildMemType = buildLimits

parseTableType :: Parser TableType
parseTableType = do et <- parseRefType
                    lim <- parseLimits
                    pure (TableType lim et)

buildTableType :: TableType -> Builder ()
buildTableType (TableType lim et) = do buildRefType et
                                       buildLimits lim

parseGlobalType :: Parser GlobalType
parseGlobalType = do t <- parseValType
                     m <- parseMut
                     pure (GlobalType m t)

buildGlobalType :: GlobalType -> Builder ()
buildGlobalType (GlobalType m t) = do buildValType t
                                      buildMut m

parseMut :: Parser Mut
parseMut = do P.word8 0
              pure MutConst
           <|> do P.word8 1
                  pure MutVar

buildMut :: Mut -> Builder ()
buildMut MutConst = B.word8 0
buildMut MutVar = B.word8 1

parseBlockType :: Parser BlockType
parseBlockType = do P.word8 64
                    pure BlockEmptyType
                 <|> do t <- parseValType
                        pure (ValType t)
                 <|> do x <- parseU32
                        pure (TypeIdx x)

buildBlockType :: BlockType -> Builder ()
buildBlockType BlockEmptyType = B.word8 64
buildBlockType (ValType t) = buildValType t
buildBlockType (TypeIdx x) = buildU32 x

parseElseInstrs :: Parser [Instr]
parseElseInstrs = do P.word8 11
                     pure []
                  <|> do P.word8 5
                         ins <- (P.many parseInstr)
                         P.word8 11
                         pure ins

buildElseInstrs :: [Instr] -> Builder ()
buildElseInstrs [] = B.word8 11
buildElseInstrs ins = do B.word8 5
                         mapM_ buildInstr ins
                         B.word8 11

parseCtlInstr :: Parser CtlInstr
parseCtlInstr = do P.word8 0
                   pure Unreachable
                <|> do P.word8 1
                       pure Nop
                <|> do P.word8 2
                       bt <- parseBlockType
                       ins <- (P.many parseInstr)
                       P.word8 11
                       pure (Block bt ins)
                <|> do P.word8 3
                       bt <- parseBlockType
                       ins <- (P.many parseInstr)
                       P.word8 11
                       pure (Loop bt ins)
                <|> do P.word8 4
                       bt <- parseBlockType
                       in1 <- (P.many parseInstr)
                       in2 <- parseElseInstrs
                       pure (If bt in1 in2)
                <|> do P.word8 12
                       l <- parseLabelIdx
                       pure (Br l)
                <|> do P.word8 13
                       l <- parseLabelIdx
                       pure (BrIf l)
                <|> do P.word8 14
                       ls <- (parseVec parseLabelIdx)
                       ln <- parseLabelIdx
                       pure (BrTable ls ln)
                <|> do P.word8 15
                       pure Return
                <|> do P.word8 16
                       x <- parseFuncIdx
                       pure (Call x)
                <|> do P.word8 17
                       y <- parseTypeIdx
                       x <- parseTableIdx
                       pure (CallIndirect x y)

buildCtlInstr :: CtlInstr -> Builder ()
buildCtlInstr Unreachable = B.word8 0
buildCtlInstr Nop = B.word8 1
buildCtlInstr (Block bt ins) = do B.word8 2
                                  buildBlockType bt
                                  mapM_ buildInstr ins
                                  B.word8 11
buildCtlInstr (Loop bt ins) = do B.word8 3
                                 buildBlockType bt
                                 mapM_ buildInstr ins
                                 B.word8 11
buildCtlInstr (If bt in1 in2) = do B.word8 4
                                   buildBlockType bt
                                   mapM_ buildInstr in1
                                   buildElseInstrs in2
buildCtlInstr (Br l) = do B.word8 12
                          buildLabelIdx l
buildCtlInstr (BrIf l) = do B.word8 13
                            buildLabelIdx l
buildCtlInstr (BrTable ls ln) = do B.word8 14
                                   buildVec buildLabelIdx ls
                                   buildLabelIdx ln
buildCtlInstr Return = B.word8 15
buildCtlInstr (Call x) = do B.word8 16
                            buildFuncIdx x
buildCtlInstr (CallIndirect x y) = do B.word8 17
                                      buildTypeIdx y
                                      buildTableIdx x

parseRefInstr :: Parser RefInstr
parseRefInstr = do P.word8 208
                   t <- parseRefType
                   pure (RefNull t)
                <|> do P.word8 209
                       pure RefIsNull
                <|> do P.word8 210
                       x <- parseFuncIdx
                       pure (RefFunc x)

buildRefInstr :: RefInstr -> Builder ()
buildRefInstr (RefNull t) = do B.word8 208
                               buildRefType t
buildRefInstr RefIsNull = B.word8 209
buildRefInstr (RefFunc x) = do B.word8 210
                               buildFuncIdx x

parseParamInstr :: Parser ParamInstr
parseParamInstr = do P.word8 26
                     pure Drop
                  <|> do P.word8 27
                         pure (Select [])
                  <|> do P.word8 28
                         ts <- (parseVec parseValType)
                         pure (Select ts)

buildParamInstr :: ParamInstr -> Builder ()
buildParamInstr Drop = B.word8 26
buildParamInstr (Select []) = B.word8 27
buildParamInstr (Select ts) = do B.word8 28
                                 buildVec buildValType ts

parseVarInstr :: Parser VarInstr
parseVarInstr = do P.word8 32
                   x <- parseLocalIdx
                   pure (LocalGet x)
                <|> do P.word8 33
                       x <- parseLocalIdx
                       pure (LocalSet x)
                <|> do P.word8 34
                       x <- parseLocalIdx
                       pure (LocalTee x)
                <|> do P.word8 35
                       x <- parseGlobalIdx
                       pure (GlobalGet x)
                <|> do P.word8 36
                       x <- parseGlobalIdx
                       pure (GlobalSet x)

buildVarInstr :: VarInstr -> Builder ()
buildVarInstr (LocalGet x) = do B.word8 32
                                buildLocalIdx x
buildVarInstr (LocalSet x) = do B.word8 33
                                buildLocalIdx x
buildVarInstr (LocalTee x) = do B.word8 34
                                buildLocalIdx x
buildVarInstr (GlobalGet x) = do B.word8 35
                                 buildGlobalIdx x
buildVarInstr (GlobalSet x) = do B.word8 36
                                 buildGlobalIdx x

parseTableInstr :: Parser TableInstr
parseTableInstr = do P.word8 37
                     x <- parseTableIdx
                     pure (TableGet x)
                  <|> do P.word8 38
                         x <- parseTableIdx
                         pure (TableSet x)

buildTableInstr :: TableInstr -> Builder ()
buildTableInstr (TableGet x) = do B.word8 37
                                  buildTableIdx x
buildTableInstr (TableSet x) = do B.word8 38
                                  buildTableIdx x

parseTableInstr' :: Parser TableInstr'
parseTableInstr' = do checkEq parseU32 12
                      y <- parseElemIdx
                      x <- parseTableIdx
                      pure (TableInit x y)
                   <|> do checkEq parseU32 13
                          x <- parseElemIdx
                          pure (ElemDrop x)
                   <|> do checkEq parseU32 14
                          x <- parseTableIdx
                          y <- parseTableIdx
                          pure (TableCopy x y)
                   <|> do checkEq parseU32 15
                          x <- parseTableIdx
                          pure (TableGrow x)
                   <|> do checkEq parseU32 16
                          x <- parseTableIdx
                          pure (TableSize x)
                   <|> do checkEq parseU32 17
                          x <- parseTableIdx
                          pure (TableFill x)

buildTableInstr' :: TableInstr' -> Builder ()
buildTableInstr' (TableInit x y) = do buildU32 12
                                      buildElemIdx y
                                      buildTableIdx x
buildTableInstr' (ElemDrop x) = do buildU32 13
                                   buildElemIdx x
buildTableInstr' (TableCopy x y) = do buildU32 14
                                      buildTableIdx x
                                      buildTableIdx y
buildTableInstr' (TableGrow x) = do buildU32 15
                                    buildTableIdx x
buildTableInstr' (TableSize x) = do buildU32 16
                                    buildTableIdx x
buildTableInstr' (TableFill x) = do buildU32 17
                                    buildTableIdx x

parseMemArg :: Parser MemArg
parseMemArg = do a <- parseU32
                 b <- parseU32
                 pure (MemArg a b)

buildMemArg :: MemArg -> Builder ()
buildMemArg (MemArg a b) = do buildU32 a
                              buildU32 b

parseMemoryInstr :: Parser MemoryInstr
parseMemoryInstr = do P.word8 40
                      m <- parseMemArg
                      pure (I32Load m)
                   <|> do P.word8 41
                          m <- parseMemArg
                          pure (I64Load m)
                   <|> do P.word8 42
                          m <- parseMemArg
                          pure (F32Load m)
                   <|> do P.word8 43
                          m <- parseMemArg
                          pure (F64Load m)
                   <|> do P.word8 44
                          m <- parseMemArg
                          pure (I32Load8S m)
                   <|> do P.word8 45
                          m <- parseMemArg
                          pure (I32Load8U m)
                   <|> do P.word8 46
                          m <- parseMemArg
                          pure (I32Load16S m)
                   <|> do P.word8 47
                          m <- parseMemArg
                          pure (I32Load16U m)
                   <|> do P.word8 48
                          m <- parseMemArg
                          pure (I64Load8S m)
                   <|> do P.word8 49
                          m <- parseMemArg
                          pure (I64Load8U m)
                   <|> do P.word8 50
                          m <- parseMemArg
                          pure (I64Load16S m)
                   <|> do P.word8 51
                          m <- parseMemArg
                          pure (I64Load16U m)
                   <|> do P.word8 52
                          m <- parseMemArg
                          pure (I64Load32S m)
                   <|> do P.word8 53
                          m <- parseMemArg
                          pure (I64Load32U m)
                   <|> do P.word8 54
                          m <- parseMemArg
                          pure (I32Store m)
                   <|> do P.word8 55
                          m <- parseMemArg
                          pure (I64Store m)
                   <|> do P.word8 56
                          m <- parseMemArg
                          pure (F32Store m)
                   <|> do P.word8 57
                          m <- parseMemArg
                          pure (F64Store m)
                   <|> do P.word8 58
                          m <- parseMemArg
                          pure (I32Store8 m)
                   <|> do P.word8 59
                          m <- parseMemArg
                          pure (I32Store16 m)
                   <|> do P.word8 60
                          m <- parseMemArg
                          pure (I64Store8 m)
                   <|> do P.word8 61
                          m <- parseMemArg
                          pure (I64Store16 m)
                   <|> do P.word8 62
                          m <- parseMemArg
                          pure (I64Store32 m)
                   <|> do P.word8 63
                          P.word8 0
                          pure MemorySize
                   <|> do P.word8 64
                          P.word8 0
                          pure MemoryGrow

buildMemoryInstr :: MemoryInstr -> Builder ()
buildMemoryInstr (I32Load m) = do B.word8 40
                                  buildMemArg m
buildMemoryInstr (I64Load m) = do B.word8 41
                                  buildMemArg m
buildMemoryInstr (F32Load m) = do B.word8 42
                                  buildMemArg m
buildMemoryInstr (F64Load m) = do B.word8 43
                                  buildMemArg m
buildMemoryInstr (I32Load8S m) = do B.word8 44
                                    buildMemArg m
buildMemoryInstr (I32Load8U m) = do B.word8 45
                                    buildMemArg m
buildMemoryInstr (I32Load16S m) = do B.word8 46
                                     buildMemArg m
buildMemoryInstr (I32Load16U m) = do B.word8 47
                                     buildMemArg m
buildMemoryInstr (I64Load8S m) = do B.word8 48
                                    buildMemArg m
buildMemoryInstr (I64Load8U m) = do B.word8 49
                                    buildMemArg m
buildMemoryInstr (I64Load16S m) = do B.word8 50
                                     buildMemArg m
buildMemoryInstr (I64Load16U m) = do B.word8 51
                                     buildMemArg m
buildMemoryInstr (I64Load32S m) = do B.word8 52
                                     buildMemArg m
buildMemoryInstr (I64Load32U m) = do B.word8 53
                                     buildMemArg m
buildMemoryInstr (I32Store m) = do B.word8 54
                                   buildMemArg m
buildMemoryInstr (I64Store m) = do B.word8 55
                                   buildMemArg m
buildMemoryInstr (F32Store m) = do B.word8 56
                                   buildMemArg m
buildMemoryInstr (F64Store m) = do B.word8 57
                                   buildMemArg m
buildMemoryInstr (I32Store8 m) = do B.word8 58
                                    buildMemArg m
buildMemoryInstr (I32Store16 m) = do B.word8 59
                                     buildMemArg m
buildMemoryInstr (I64Store8 m) = do B.word8 60
                                    buildMemArg m
buildMemoryInstr (I64Store16 m) = do B.word8 61
                                     buildMemArg m
buildMemoryInstr (I64Store32 m) = do B.word8 62
                                     buildMemArg m
buildMemoryInstr MemorySize = do B.word8 63
                                 B.word8 0
buildMemoryInstr MemoryGrow = do B.word8 64
                                 B.word8 0

parseMemoryInstr' :: Parser MemoryInstr'
parseMemoryInstr' = do checkEq parseU32 8
                       x <- parseDataIdx
                       P.word8 0
                       pure (MemoryInit x)
                    <|> do checkEq parseU32 9
                           x <- parseDataIdx
                           pure (DataDrop x)
                    <|> do checkEq parseU32 10
                           P.word8 0
                           P.word8 0
                           pure MemoryCopy
                    <|> do checkEq parseU32 11
                           P.word8 0
                           pure MemoryFill

buildMemoryInstr' :: MemoryInstr' -> Builder ()
buildMemoryInstr' (MemoryInit x) = do buildU32 8
                                      buildDataIdx x
                                      B.word8 0
buildMemoryInstr' (DataDrop x) = do buildU32 9
                                    buildDataIdx x
buildMemoryInstr' MemoryCopy = do buildU32 10
                                  B.word8 0
                                  B.word8 0
buildMemoryInstr' MemoryFill = do buildU32 11
                                  B.word8 0

parseNumInstr :: Parser NumInstr
parseNumInstr = do P.word8 65
                   n <- parseU32
                   pure (I32Const n)
                <|> do P.word8 66
                       n <- parseU64
                       pure (I64Const n)
                <|> do P.word8 67
                       z <- parseF32
                       pure (F32Const z)
                <|> do P.word8 68
                       z <- parseF64
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

buildNumInstr :: NumInstr -> Builder ()
buildNumInstr (I32Const n) = do B.word8 65
                                buildU32 n
buildNumInstr (I64Const n) = do B.word8 66
                                buildU64 n
buildNumInstr (F32Const z) = do B.word8 67
                                buildF32 z
buildNumInstr (F64Const z) = do B.word8 68
                                buildF64 z
buildNumInstr I32Eqz = B.word8 69
buildNumInstr (I32RelOp IEq) = B.word8 70
buildNumInstr (I32RelOp INe) = B.word8 71
buildNumInstr (I32RelOp ILtS) = B.word8 72
buildNumInstr (I32RelOp ILtU) = B.word8 73
buildNumInstr (I32RelOp IGtS) = B.word8 74
buildNumInstr (I32RelOp IGtU) = B.word8 75
buildNumInstr (I32RelOp ILeS) = B.word8 76
buildNumInstr (I32RelOp ILeU) = B.word8 77
buildNumInstr (I32RelOp IGeS) = B.word8 78
buildNumInstr (I32RelOp IGeU) = B.word8 79
buildNumInstr I64Eqz = B.word8 80
buildNumInstr (I64RelOp IEq) = B.word8 81
buildNumInstr (I64RelOp INe) = B.word8 82
buildNumInstr (I64RelOp ILtS) = B.word8 83
buildNumInstr (I64RelOp ILtU) = B.word8 84
buildNumInstr (I64RelOp IGtS) = B.word8 85
buildNumInstr (I64RelOp IGtU) = B.word8 86
buildNumInstr (I64RelOp ILeS) = B.word8 87
buildNumInstr (I64RelOp ILeU) = B.word8 88
buildNumInstr (I64RelOp IGeS) = B.word8 89
buildNumInstr (I64RelOp IGeU) = B.word8 90
buildNumInstr (F32RelOp FEq) = B.word8 91
buildNumInstr (F32RelOp FNe) = B.word8 92
buildNumInstr (F32RelOp FLt) = B.word8 93
buildNumInstr (F32RelOp FGt) = B.word8 94
buildNumInstr (F32RelOp FLe) = B.word8 95
buildNumInstr (F32RelOp FGe) = B.word8 96
buildNumInstr (F64RelOp FEq) = B.word8 97
buildNumInstr (F64RelOp FNe) = B.word8 98
buildNumInstr (F64RelOp FLt) = B.word8 99
buildNumInstr (F64RelOp FGt) = B.word8 100
buildNumInstr (F64RelOp FLe) = B.word8 101
buildNumInstr (F64RelOp FGe) = B.word8 102
buildNumInstr (I32UnOp IClz) = B.word8 103
buildNumInstr (I32UnOp ICtz) = B.word8 104
buildNumInstr (I32UnOp IPopcnt) = B.word8 105
buildNumInstr (I32BinOp IAdd) = B.word8 106
buildNumInstr (I32BinOp ISub) = B.word8 107
buildNumInstr (I32BinOp IMul) = B.word8 108
buildNumInstr (I32BinOp IDivS) = B.word8 109
buildNumInstr (I32BinOp IDivU) = B.word8 110
buildNumInstr (I32BinOp IRemS) = B.word8 111
buildNumInstr (I32BinOp IRemU) = B.word8 112
buildNumInstr (I32BinOp IAnd) = B.word8 113
buildNumInstr (I32BinOp IOr) = B.word8 114
buildNumInstr (I32BinOp IXor) = B.word8 115
buildNumInstr (I32BinOp IShl) = B.word8 116
buildNumInstr (I32BinOp IShrS) = B.word8 117
buildNumInstr (I32BinOp IShrU) = B.word8 118
buildNumInstr (I32BinOp IRotl) = B.word8 119
buildNumInstr (I32BinOp IRotr) = B.word8 120
buildNumInstr (I64UnOp IClz) = B.word8 121
buildNumInstr (I64UnOp ICtz) = B.word8 122
buildNumInstr (I64UnOp IPopcnt) = B.word8 123
buildNumInstr (I64BinOp IAdd) = B.word8 124
buildNumInstr (I64BinOp ISub) = B.word8 125
buildNumInstr (I64BinOp IMul) = B.word8 126
buildNumInstr (I64BinOp IDivS) = B.word8 127
buildNumInstr (I64BinOp IDivU) = B.word8 128
buildNumInstr (I64BinOp IRemS) = B.word8 129
buildNumInstr (I64BinOp IRemU) = B.word8 130
buildNumInstr (I64BinOp IAnd) = B.word8 131
buildNumInstr (I64BinOp IOr) = B.word8 132
buildNumInstr (I64BinOp IXor) = B.word8 133
buildNumInstr (I64BinOp IShl) = B.word8 134
buildNumInstr (I64BinOp IShrS) = B.word8 135
buildNumInstr (I64BinOp IShrU) = B.word8 136
buildNumInstr (I64BinOp IRotl) = B.word8 137
buildNumInstr (I64BinOp IRotr) = B.word8 138
buildNumInstr (F32UnOp FAbs) = B.word8 139
buildNumInstr (F32UnOp FNeg) = B.word8 140
buildNumInstr (F32UnOp FCeil) = B.word8 141
buildNumInstr (F32UnOp FFloor) = B.word8 142
buildNumInstr (F32UnOp FTrunc) = B.word8 143
buildNumInstr (F32UnOp FNearest) = B.word8 144
buildNumInstr (F32UnOp FSqrt) = B.word8 145
buildNumInstr (F32BinOp FAdd) = B.word8 146
buildNumInstr (F32BinOp FSub) = B.word8 147
buildNumInstr (F32BinOp FMul) = B.word8 148
buildNumInstr (F32BinOp FDiv) = B.word8 149
buildNumInstr (F32BinOp FMin) = B.word8 150
buildNumInstr (F32BinOp FMax) = B.word8 151
buildNumInstr (F32BinOp FCopysign) = B.word8 152
buildNumInstr (F64UnOp FAbs) = B.word8 153
buildNumInstr (F64UnOp FNeg) = B.word8 154
buildNumInstr (F64UnOp FCeil) = B.word8 155
buildNumInstr (F64UnOp FFloor) = B.word8 156
buildNumInstr (F64UnOp FTrunc) = B.word8 157
buildNumInstr (F64UnOp FNearest) = B.word8 158
buildNumInstr (F64UnOp FSqrt) = B.word8 159
buildNumInstr (F64BinOp FAdd) = B.word8 160
buildNumInstr (F64BinOp FSub) = B.word8 161
buildNumInstr (F64BinOp FMul) = B.word8 162
buildNumInstr (F64BinOp FDiv) = B.word8 163
buildNumInstr (F64BinOp FMin) = B.word8 164
buildNumInstr (F64BinOp FMax) = B.word8 165
buildNumInstr (F64BinOp FCopysign) = B.word8 166
buildNumInstr I32WrapI64 = B.word8 167
buildNumInstr I32TruncF32S = B.word8 168
buildNumInstr I32TruncF32U = B.word8 169
buildNumInstr I32TruncF64S = B.word8 170
buildNumInstr I32TruncF64U = B.word8 171
buildNumInstr I64ExtendI32S = B.word8 172
buildNumInstr I64ExtendI32U = B.word8 173
buildNumInstr I64TruncF32S = B.word8 174
buildNumInstr I64TruncF32U = B.word8 175
buildNumInstr I64TruncF64S = B.word8 176
buildNumInstr I64TruncF64U = B.word8 177
buildNumInstr F32ConvertI32S = B.word8 178
buildNumInstr F32ConvertI32U = B.word8 179
buildNumInstr F32ConvertI64S = B.word8 180
buildNumInstr F32ConvertI64U = B.word8 181
buildNumInstr F32DemoteF64 = B.word8 182
buildNumInstr F64ConvertI32S = B.word8 183
buildNumInstr F64ConvertI32U = B.word8 184
buildNumInstr F64ConvertI64S = B.word8 185
buildNumInstr F64ConvertI64U = B.word8 186
buildNumInstr F64PromoteF32 = B.word8 187
buildNumInstr I32ReinterpretF32 = B.word8 188
buildNumInstr I64ReinterpretF64 = B.word8 189
buildNumInstr F32ReinterpretI32 = B.word8 190
buildNumInstr F64ReinterpretI64 = B.word8 191
buildNumInstr I32Extend8S = B.word8 192
buildNumInstr I32Extend16S = B.word8 193
buildNumInstr I64Extend8S = B.word8 194
buildNumInstr I64Extend16S = B.word8 195
buildNumInstr I64Extend32S = B.word8 196

parseNumInstr' :: Parser NumInstr'
parseNumInstr' = do checkEq parseU32 0
                    pure I32TruncSatF32S
                 <|> do checkEq parseU32 1
                        pure I32TruncSatF32U
                 <|> do checkEq parseU32 2
                        pure I32TruncSatF64S
                 <|> do checkEq parseU32 3
                        pure I32TruncSatF64U
                 <|> do checkEq parseU32 4
                        pure I64TruncSatF32S
                 <|> do checkEq parseU32 5
                        pure I64TruncSatF32U
                 <|> do checkEq parseU32 6
                        pure I64TruncSatF64S
                 <|> do checkEq parseU32 7
                        pure I64TruncSatF64U

buildNumInstr' :: NumInstr' -> Builder ()
buildNumInstr' I32TruncSatF32S = buildU32 0
buildNumInstr' I32TruncSatF32U = buildU32 1
buildNumInstr' I32TruncSatF64S = buildU32 2
buildNumInstr' I32TruncSatF64U = buildU32 3
buildNumInstr' I64TruncSatF32S = buildU32 4
buildNumInstr' I64TruncSatF32U = buildU32 5
buildNumInstr' I64TruncSatF64S = buildU32 6
buildNumInstr' I64TruncSatF64U = buildU32 7

parseTypeIdx :: Parser TypeIdx
parseTypeIdx = parseU32

buildTypeIdx :: TypeIdx -> Builder ()
buildTypeIdx = buildU32

parseFuncIdx :: Parser FuncIdx
parseFuncIdx = parseU32

buildFuncIdx :: FuncIdx -> Builder ()
buildFuncIdx = buildU32

parseTableIdx :: Parser TableIdx
parseTableIdx = parseU32

buildTableIdx :: TableIdx -> Builder ()
buildTableIdx = buildU32

parseMemIdx :: Parser MemIdx
parseMemIdx = parseU32

buildMemIdx :: MemIdx -> Builder ()
buildMemIdx = buildU32

parseGlobalIdx :: Parser GlobalIdx
parseGlobalIdx = parseU32

buildGlobalIdx :: GlobalIdx -> Builder ()
buildGlobalIdx = buildU32

parseElemIdx :: Parser ElemIdx
parseElemIdx = parseU32

buildElemIdx :: ElemIdx -> Builder ()
buildElemIdx = buildU32

parseDataIdx :: Parser DataIdx
parseDataIdx = parseU32

buildDataIdx :: DataIdx -> Builder ()
buildDataIdx = buildU32

parseLocalIdx :: Parser LocalIdx
parseLocalIdx = parseU32

buildLocalIdx :: LocalIdx -> Builder ()
buildLocalIdx = buildU32

parseLabelIdx :: Parser LabelIdx
parseLabelIdx = parseU32

buildLabelIdx :: LabelIdx -> Builder ()
buildLabelIdx = buildU32

parseMagic :: Parser ()
parseMagic = do P.word8 0
                P.word8 97
                P.word8 115
                P.word8 109
                pure ()

buildMagic :: Builder ()
buildMagic = do B.word8 0
                B.word8 97
                B.word8 115
                B.word8 109

parseVersion :: Parser ()
parseVersion = do P.word8 16
                  P.word8 0
                  P.word8 0
                  P.word8 0
                  pure ()

buildVersion :: Builder ()
buildVersion = do B.word8 16
                  B.word8 0
                  B.word8 0
                  B.word8 0

parseModule :: Parser Module
parseModule = do parseMagic
                 parseVersion
                 _ <- (P.many parseCustom)
                 pure (Module [] [] [] [] [] [] [] Nothing [] [])

buildModule :: Module -> Builder ()
buildModule (Module [] [] [] [] [] [] [] Nothing [] []) = do buildMagic
                                                             buildVersion
                                                             mapM_ buildCustom _

-- 解析Instr
parseInstr :: Parser Instr
parseInstr = NumInstr <$> parseNumInstr
    <|> RefInstr <$> parseRefInstr
    <|> ParamInstr <$> parseParamInstr
    <|> VarInstr <$> parseVarInstr
    <|> TableInstr <$> parseTableInstr
    <|> MemoryInstr <$> parseMemoryInstr
    <|> CtlInstr <$> parseCtlInstr
    <|> do
        P.word8 0xFC
        NumInstr' <$> parseNumInstr'
            <|> TableInstr' <$> parseTableInstr'
            <|> MemoryInstr' <$> parseMemoryInstr'

buildInstr :: Instr -> Builder ()
buildInstr = go
    where
        go (NumInstr i) = buildNumInstr i
        go (RefInstr i) = buildRefInstr i
        go (ParamInstr i) = buildParamInstr i
        go (VarInstr i) = buildVarInstr i
        go (TableInstr i) = buildTableInstr i
        go (MemoryInstr i) = buildMemoryInstr i
        go (CtlInstr i) = buildCtlInstr i
        go (NumInstr' i) = B.word8 0xFC >> buildNumInstr' i
        go (TableInstr' i) = B.word8 0xFC >> buildTableInstr' i
        go (MemoryInstr' i) = B.word8 0xFC >> buildMemoryInstr' i
