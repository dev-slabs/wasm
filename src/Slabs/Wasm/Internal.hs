module Slabs.Wasm.Internal  where

import Universum
import Data.Default
import Slabs.Data.Codec 
import Slabs.Wasm.Structure

import Z.Data.Parser (Parser)
import qualified Z.Data.Parser as P 
import qualified Control.Monad.Combinators as P 
import Z.Data.Builder (Builder)
import qualified Z.Data.Builder as B 
import qualified Z.Data.Vector as V

parseU32 :: Parser Word32
parseU32 = anyWord32leb128

buildU32 :: U32 -> Builder ()
buildU32 = word32LEB128

parseU64 :: Parser Word64
parseU64 = anyWord64leb128

buildU64 :: Word64 -> Builder ()
buildU64 = word64LEB128

parseF32 :: Parser Float
parseF32 = pure 1  -- TODO

buildF32 :: Float -> Builder ()
buildF32 = word32LEB128

parseF64 :: Parser Double
parseF64 = anyWord64leb128

buildF64 :: Double -> Builder ()
buildF64 = word64LEB128

parseVec :: Parser a -> Parser [a]
parseVec p = do
  n <- parseU32
  P.count (fromIntegral n) p

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

parseBytes :: Parser Bytes
parseBytes = undefined

buildBytes :: Bytes -> Builder ()
buildBytes bs = undefined

parseName :: Parser Text
parseName = undefined

buildName :: Text -> Builder ()
buildName = undefined

parseSection :: Parser a -> Parser a
parseSection p = do
  size <- parseU32
  bs <- P.take $ fromIntegral size
  case P.parse' (p <* P.endOfInput) bs of
    Left err -> P.fail' $ mconcat err
    Right a -> return a

buildSection :: (a -> Builder ()) -> a -> Builder ()
buildSection b a = do
  let bs = B.build $ b a
      size = fromIntegral $ V.length bs
  buildU32 size
  B.bytes bs
