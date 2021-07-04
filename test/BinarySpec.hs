module BinarySpec where

import Prelude
import System.Directory
import Test.Hspec
import Slabs.Wasm.Structure
import Slabs.Wasm.Binary

import Z.IO.Logger
import qualified Z.Data.Text as T
import qualified Z.Data.Parser as P
import qualified Z.Data.Builder as B
import qualified Z.IO as ZIO
import qualified Z.IO.FileSystem as ZIO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decode" do
    it "decode / print / encode" $ withDefaultLogger $ do
      getCurrentDirectory >>= putStrLn
      bs <- ZIO.readFile "test/test.wasm"
      let m = P.parse' decodeModule bs
      case m of
        Left errs -> fatal $ B.text $ mconcat errs
        Right module_ -> do
          info $ B.text . T.toText $ module_
          let bs' = B.build $ encodeModule module_
          info $ B.bytes bs'
          bs' `shouldBe` bs
