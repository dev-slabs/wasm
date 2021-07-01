module Slabs.Wasm.Internal  where

import Universum
import Control.Monad.Combinators
import Slabs.Data.Codec 
import Slabs.Wasm.Structure

parseU32 :: Parser Word32
parseU32 = anyWord32leb128

buildU32 :: U32 -> Builder ()
buildU32 = word32LEB128

parseU64 :: Parser Word64
parseU64 = anyWord64leb128

buildU64 :: Word64 -> Builder ()
buildU64 = word64LEB128

parseF32 :: Parser Float
parseF32 = anyWord32leb128

buildF32 :: Float -> Builder ()
buildF32 = word32LEB128

parseF64 :: Parser Double
parseF64 = anyWord64leb128

buildF64 :: Double -> Builder ()
buildF64 = word64LEB128

parseVec :: Parser a -> Parser [a]
parseVec p = do
  n <- parseU32
  count (fromIntegral n) p

buildVec :: (a -> Builder ()) -> [a] -> Builder ()
buildVec b vals = do
  buildU32 (fromIntegral $ length vals)
  mapM_ b vals


checkEq :: (Show a, Eq a) => Parser a -> a -> Parser a
checkEq p a = do
    v <- p
    if a == v
        then return v
        else fail $ "not equal: " <> show a
