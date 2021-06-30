module Slabs.Wasm.Binary  where

import Universum 
import Slabs.Wasm.Structure 

import Z.Data.Parser (Parser)
import qualified Z.Data.Parser as P 
import qualified Control.Monad.Combinators as P 
import Z.Data.Builder (Builder)
import qualified Z.Data.Builder as B 

parseNumType :: Parser NumType
parseNumType = P.choice [do P.word8 127
                            pure I32, do P.word8 126
                                         pure I64, do P.word8 125
                                                      pure F32, do P.word8 124
                                                                   pure F64]
buildNumType :: NumType -> (Builder ())
buildNumType I32 = B.word8 127
buildNumType I64 = B.word8 126
buildNumType F32 = B.word8 125
buildNumType F64 = B.word8 124
parseRefType :: Parser RefType
parseRefType = P.choice [do P.word8 112
                            pure FuncRef, do P.word8 111
                                             pure ExternRef]
buildRefType :: RefType -> (Builder ())
buildRefType FuncRef = B.word8 112
buildRefType ExternRef = B.word8 111
parseValType :: Parser ValType
parseValType = P.choice [do t <- parseNumType
                            pure (NumType t), do t <- parseRefType
                                                 pure (RefType t)]
buildValType :: ValType -> (Builder ())
buildValType (NumType t) = buildNumType t
buildValType (RefType t) = buildRefType t
parseResultType :: Parser ResultType
parseResultType = do t <- (parseVec parseValType)
                     pure t
buildResultType :: ResultType -> (Builder ())
buildResultType t = buildVec buildValType t
parseFuncType :: Parser FuncType
parseFuncType = do P.word8 96
                   rt1 <- parseResultType
                   rt2 <- parseResultType
                   pure (FuncType rt1 rt2)
buildFuncType :: FuncType -> (Builder ())
buildFuncType (FuncType rt1 rt2) = do B.word8 96
                                      buildResultType rt1
                                      buildResultType rt2
parseLimits :: Parser Limits
parseLimits = P.choice [do P.word8 0
                           n <- parseU32
                           pure (Limits n Nothing), do P.word8 1
                                                       n <- parseU32
                                                       m <- parseU32
                                                       pure (Limits n (Just m))]
buildLimits :: Limits -> (Builder ())
buildLimits (Limits n Nothing) = do B.word8 0
                                    buildU32 n
buildLimits (Limits n (Just m)) = do B.word8 1
                                     buildU32 n
                                     buildU32 m
parseMemType :: Parser MemType
parseMemType = do lim <- parseLimits
                  pure lim
buildMemType :: MemType -> (Builder ())
buildMemType lim = buildLimits lim
parseTableType :: Parser TableType
parseTableType = do et <- parseRefType
                    lim <- parseLimits
                    pure (TableType lim et)
buildTableType :: TableType -> (Builder ())
buildTableType (TableType lim et) = do buildRefType et
                                       buildLimits lim
parseGlobalType :: Parser GlobalType
parseGlobalType = do t <- parseValType
                     m <- parseMut
                     pure (GlobalType m t)
buildGlobalType :: GlobalType -> (Builder ())
buildGlobalType (GlobalType m t) = do buildValType t
                                      buildMut m
parseMut :: Parser Mut
parseMut = P.choice [do P.word8 0
                        pure MutConst, do P.word8 1
                                          pure MutVar]
buildMut :: Mut -> (Builder ())
buildMut MutConst = B.word8 0
buildMut MutVar = B.word8 1
parseBlockType :: Parser BlockType
parseBlockType = P.choice [do P.word8 64
                              pure BlockEmptyType, do t <- parseValType
                                                      pure (ValType t), do x <- parseU32
                                                                           pure (TypeIdx x)]
buildBlockType :: BlockType -> (Builder ())
buildBlockType BlockEmptyType = B.word8 64
buildBlockType (ValType t) = buildValType t
buildBlockType (TypeIdx x) = buildU32 x
parseElseInstrs :: Parser [Instr]
parseElseInstrs = P.choice [do P.word8 11
                               pure [], do P.word8 5
                                           ins <- (P.many parseInstr)
                                           P.word8 11
                                           pure ins]
buildElseInstrs :: [Instr] -> (Builder ())
buildElseInstrs [] = B.word8 11
buildElseInstrs ins = do B.word8 5
                         mapM_ buildInstr ins
                         B.word8 11
parseCtlInstr :: Parser CtlInstr
parseCtlInstr = P.choice [do P.word8 0
                             pure Unreachable, do P.word8 1
                                                  pure Nop, do P.word8 2
                                                               bt <- parseBlockType
                                                               ins <- (P.many parseInstr)
                                                               P.word8 11
                                                               pure (Block bt ins), do P.word8 3
                                                                                       bt <- parseBlockType
                                                                                       ins <- (P.many parseInstr)
                                                                                       P.word8 11
                                                                                       pure (Loop bt ins), do P.word8 4
                                                                                                              bt <- parseBlockType
                                                                                                              in1 <- (P.many parseInstr)
                                                                                                              in2 <- parseElseInstrs
                                                                                                              pure (If bt in1 in2), do P.word8 12
                                                                                                                                       l <- parseLabelIdx
                                                                                                                                       pure (Br l), do P.word8 13
                                                                                                                                                       l <- parseLabelIdx
                                                                                                                                                       pure (BrIf l), do P.word8 14
                                                                                                                                                                         ls <- (parseVec parseLabelIdx)
                                                                                                                                                                         ln <- parseLabelIdx
                                                                                                                                                                         pure (BrTable ls ln), do P.word8 15
                                                                                                                                                                                                  pure Return, do P.word8 16
                                                                                                                                                                                                                  x <- parseFuncIdx
                                                                                                                                                                                                                  pure (Call x), do P.word8 17
                                                                                                                                                                                                                                    y <- parseTypeIdx
                                                                                                                                                                                                                                    x <- parseTableIdx
                                                                                                                                                                                                                                    pure (CallIndirect x y)]
buildCtlInstr :: CtlInstr -> (Builder ())
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
parseRefInstr = P.choice [do P.word8 208
                             t <- parseRefType
                             pure (RefNull t), do P.word8 209
                                                  pure RefIsNull, do P.word8 210
                                                                     x <- parseFuncIdx
                                                                     pure (RefFunc x)]
buildRefInstr :: RefInstr -> (Builder ())
buildRefInstr (RefNull t) = do B.word8 208
                               buildRefType t
buildRefInstr RefIsNull = B.word8 209
buildRefInstr (RefFunc x) = do B.word8 210
                               buildFuncIdx x
parseParamInstr :: Parser ParamInstr
parseParamInstr = P.choice [do P.word8 26
                               pure Drop, do P.word8 27
                                             pure (Select []), do P.word8 28
                                                                  ts <- (parseVec parseValType)
                                                                  pure (Select ts)]
buildParamInstr :: ParamInstr -> (Builder ())
buildParamInstr Drop = B.word8 26
buildParamInstr (Select []) = B.word8 27
buildParamInstr (Select ts) = do B.word8 28
                                 buildVec buildValType ts
parseVarInstr :: Parser VarInstr
parseVarInstr = P.choice [do P.word8 32
                             x <- parseLocalIdx
                             pure (LocalGet x), do P.word8 33
                                                   x <- parseLocalIdx
                                                   pure (LocalSet x), do P.word8 34
                                                                         x <- parseLocalIdx
                                                                         pure (LocalTee x), do P.word8 35
                                                                                               x <- parseGlobalIdx
                                                                                               pure (GlobalGet x), do P.word8 36
                                                                                                                      x <- parseGlobalIdx
                                                                                                                      pure (GlobalSet x)]
buildVarInstr :: VarInstr -> (Builder ())
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
parseTableInstr = P.choice [do P.word8 37
                               x <- parseTableIdx
                               pure (TableGet x), do P.word8 38
                                                     x <- parseTableIdx
                                                     pure (TableSet x)]
buildTableInstr :: TableInstr -> (Builder ())
buildTableInstr (TableGet x) = do B.word8 37
                                  buildTableIdx x
buildTableInstr (TableSet x) = do B.word8 38
                                  buildTableIdx x
parseTableInstr' :: Parser TableInstr'
parseTableInstr' = P.choice [do parseEq parseU32 12
                                y <- parseElemIdx
                                x <- parseTableIdx
                                pure (TableInit x y), do parseEq parseU32 13
                                                         x <- parseElemIdx
                                                         pure (ElemDrop x), do parseEq parseU32 14
                                                                               x <- parseTableIdx
                                                                               y <- parseTableIdx
                                                                               pure (TableCopy x y), do parseEq parseU32 15
                                                                                                        x <- parseTableIdx
                                                                                                        pure (TableGrow x), do parseEq parseU32 16
                                                                                                                               x <- parseTableIdx
                                                                                                                               pure (TableSize x), do parseEq parseU32 17
                                                                                                                                                      x <- parseTableIdx
                                                                                                                                                      pure (TableFill x)]
buildTableInstr' :: TableInstr' -> (Builder ())
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
buildMemArg :: MemArg -> (Builder ())
buildMemArg (MemArg a b) = do buildU32 a
                              buildU32 b
parseMemoryInstr :: Parser MemoryInstr
parseMemoryInstr = P.choice [do P.word8 40
                                m <- parseMemArg
                                pure (I32Load m), do P.word8 41
                                                     m <- parseMemArg
                                                     pure (I64Load m), do P.word8 42
                                                                          m <- parseMemArg
                                                                          pure (F32Load m), do P.word8 43
                                                                                               m <- parseMemArg
                                                                                               pure (F64Load m), do P.word8 44
                                                                                                                    m <- parseMemArg
                                                                                                                    pure (I32Load8S m), do P.word8 45
                                                                                                                                           m <- parseMemArg
                                                                                                                                           pure (I32Load8U m), do P.word8 46
                                                                                                                                                                  m <- parseMemArg
                                                                                                                                                                  pure (I32Load16S m), do P.word8 47
                                                                                                                                                                                          m <- parseMemArg
                                                                                                                                                                                          pure (I32Load16U m), do P.word8 48
                                                                                                                                                                                                                  m <- parseMemArg
                                                                                                                                                                                                                  pure (I64Load8S m), do P.word8 49
                                                                                                                                                                                                                                         m <- parseMemArg
                                                                                                                                                                                                                                         pure (I64Load8U m), do P.word8 50
                                                                                                                                                                                                                                                                m <- parseMemArg
                                                                                                                                                                                                                                                                pure (I64Load16S m), do P.word8 51
                                                                                                                                                                                                                                                                                        m <- parseMemArg
                                                                                                                                                                                                                                                                                        pure (I64Load16U m), do P.word8 52
                                                                                                                                                                                                                                                                                                                m <- parseMemArg
                                                                                                                                                                                                                                                                                                                pure (I64Load32S m), do P.word8 53
                                                                                                                                                                                                                                                                                                                                        m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                        pure (I64Load32U m), do P.word8 54
                                                                                                                                                                                                                                                                                                                                                                m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                pure (I32Store m), do P.word8 55
                                                                                                                                                                                                                                                                                                                                                                                      m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                      pure (I64Store m), do P.word8 56
                                                                                                                                                                                                                                                                                                                                                                                                            m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                            pure (F32Store m), do P.word8 57
                                                                                                                                                                                                                                                                                                                                                                                                                                  m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                  pure (F64Store m), do P.word8 58
                                                                                                                                                                                                                                                                                                                                                                                                                                                        m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                                        pure (I32Store8 m), do P.word8 59
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               pure (I32Store16 m), do P.word8 60
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       pure (I64Store8 m), do P.word8 61
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              pure (I64Store16 m), do P.word8 62
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      m <- parseMemArg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      pure (I64Store32 m), do P.word8 63
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              P.word8 0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              pure MemorySize, do P.word8 64
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  P.word8 0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  pure MemoryGrow]
buildMemoryInstr :: MemoryInstr -> (Builder ())
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
parseMemoryInstr' = P.choice [do parseEq parseU32 8
                                 x <- parseDataIdx
                                 P.word8 0
                                 pure (MemoryInit x), do parseEq parseU32 9
                                                         x <- parseDataIdx
                                                         pure (DataDrop x), do parseEq parseU32 10
                                                                               P.word8 0
                                                                               P.word8 0
                                                                               pure MemoryCopy, do parseEq parseU32 11
                                                                                                   P.word8 0
                                                                                                   pure MemoryFill]
buildMemoryInstr' :: MemoryInstr' -> (Builder ())
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
parseNumInstr = P.choice [do P.word8 65
                             n <- parseU32
                             pure (I32Const n), do P.word8 66
                                                   n <- parseU64
                                                   pure (I64Const n)]
buildNumInstr :: NumInstr -> (Builder ())
buildNumInstr (I32Const n) = do B.word8 65
                                buildU32 n
buildNumInstr (I64Const n) = do B.word8 66
                                buildU64 n
parseInstr :: Parser Instr
parseInstr = P.choice [do i <- parseMemoryInstr
                          pure (MemoryInstr i), do i <- parseCtlInstr
                                                   pure (CtlInstr i), do i <- parseRefInstr
                                                                         pure (RefInstr i), do P.word8 252
                                                                                               i <- parseInstr'
                                                                                               pure i]
buildInstr :: Instr -> (Builder ())
buildInstr (MemoryInstr i) = buildMemoryInstr i
buildInstr (CtlInstr i) = buildCtlInstr i
buildInstr (RefInstr i) = buildRefInstr i
buildInstr i = do B.word8 252
                  buildInstr' i
parseInstr' :: Parser Instr
parseInstr' = P.choice [do i <- parseNumInstr'
                           pure (NumInstr' i), do i <- parseTableInstr'
                                                  pure (TableInstr' i), do i <- parseMemoryInstr'
                                                                           pure (MemoryInstr' i)]
buildInstr' :: Instr -> (Builder ())
buildInstr' (NumInstr' i) = buildNumInstr' i
buildInstr' (TableInstr' i) = buildTableInstr' i
buildInstr' (MemoryInstr' i) = buildMemoryInstr' i