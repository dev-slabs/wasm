module BinarySpec where

-- import System.Directory
import Test.Hspec
import Slabs.Wasm.Structure
import Slabs.Wasm.Binary

import Z.IO
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
    it "decode / print / encode" $ do
      -- getCurrentDirectory >>= putStrLn
      bs <- ZIO.readFile "test/test.wasm"
      let m = P.parse' decodeModule bs
      case m of
        Left errs -> fatal $ B.text $ mconcat errs
        Right module_ -> do
          printStdLn module_
          let bs' = B.build $ encodeModule module_
          -- putStdLn $ B.bytes bs'
          bs' `shouldBe` bs
