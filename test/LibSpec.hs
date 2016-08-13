module LibSpec
    ( spec
    ) where

import Test.Hspec
import Data.Text (pack)
import Lib

spec :: Spec
spec = do
        describe "parseGuid" $ do
            it "parses '{21E81DE2-8048-43EA-BE6C-B17541493312}'" $
                parseGuid (pack "{21E81DE2-8048-43EA-BE6C-B17541493312}") `shouldBe`
                    (Right $ GUID (pack "21E81DE2") (pack "8048") (pack "43EA") (pack "BE6CB17541493312"))
            it "fails to parses '{}'" $
                parseGuid (pack "{}") `shouldBe` Left "Failed reading: satisfy"
        describe "toCppAggr" $ do
            it "takes GUID and convert to C++ aggregate initialization form" $
                toCppAggr (GUID (pack "21E81DE2") (pack "8048") (pack "43EA") (pack "BE6CB17541493312")) `shouldBe`
                    pack "GUID{0x21E81DE2u,0x8048u,0x43EAu,{0xBEu,0x6Cu,0xB1u,0x75u,0x41u,0x49u,0x33u,0x12u}}"
