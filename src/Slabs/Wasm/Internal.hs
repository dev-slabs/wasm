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
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V

decodeU32 :: Parser Word32
decodeU32 = anyWord32leb128

encodeU32 :: U32 -> Builder ()
encodeU32 = word32LEB128

decodeU64 :: Parser Word64
decodeU64 = anyWord64leb128

encodeU64 :: Word64 -> Builder ()
encodeU64 = word64LEB128

decodeF32 :: Parser Float
decodeF32 = P.decodePrimLE

encodeF32 :: Float -> Builder ()
encodeF32 = B.encodePrimLE

decodeF64 :: Parser Double
decodeF64 = P.decodePrimLE

encodeF64 :: Double -> Builder ()
encodeF64 = B.encodePrimLE

decodeVec :: Parser a -> Parser [a]
decodeVec p = do
  n <- decodeU32
  P.count (fromIntegral n) p

encodeVec :: (a -> Builder ()) -> [a] -> Builder ()
encodeVec b vals = do
  encodeU32 (fromIntegral $ length vals)
  mapM_ b vals


checkEq :: (Show a, Eq a) => Parser a -> a -> Parser a
checkEq p a = do
    v <- p
    if a == v
        then return v
        else fail $ "not equal: " <> show a

decodeBytes :: Parser Bytes
decodeBytes = do
  size <- decodeU32
  P.take (fromIntegral size)

encodeBytes :: Bytes -> Builder ()
encodeBytes bs = encodeU32 size >> encodeBytes bs
  where
    size = fromIntegral $ V.length bs

decodeName :: Parser Name
decodeName = T.validate <$> decodeBytes

encodeName :: Name -> Builder ()
encodeName = encodeBytes . T.getUTF8Bytes

decodeSection :: Parser a -> Parser a
decodeSection p = do
  size <- decodeU32
  bs <- P.take $ fromIntegral size
  case P.parse' (p <* P.endOfInput) bs of
    Left err -> P.fail' $ mconcat err
    Right a -> return a

encodeSection :: (a -> Builder ()) -> a -> Builder ()
encodeSection b a = do
  let bs = B.build $ b a
      size = fromIntegral $ V.length bs
  encodeU32 size
  B.bytes bs
