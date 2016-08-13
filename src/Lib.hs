{-# LANGUAGE OverloadedStrings #-}
-- echo -n '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' | stack exec guid-exe

module Lib
    ( GUID(GUID)
    , run
    , parseGuid
    , toCppAggr
    ) where

import Data.Char (isHexDigit)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack, append)
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

data GUID = GUID
          { data1 :: Text
          , data2 :: Text
          , data3 :: Text
          , data4 :: Text
          }
          deriving (Show, Eq, Ord)

begin :: Parser Char
begin = char '{'
end :: Parser Char
end = char '}'
sep :: Parser Char
sep = char '-'
hexDigit :: Parser Char
hexDigit = satisfy isHexDigit
h :: Int -> Parser Text
h n = do
        s <- count n hexDigit
        return . pack $ s

parseData1 :: Parser Text
parseData1 = h 8
parseData2 :: Parser Text
parseData2 = h 4
parseData3 :: Parser Text
parseData3 = h 4
parseData4 :: Parser Text
parseData4 = do
        headPart <- h 4
        sep
        tailPart <- h 12
        return $ headPart `append` tailPart

parseInner :: Parser GUID
parseInner = do
        d1 <- h 8
        sep
        d2 <- h 4
        sep
        d3 <- h 4
        sep
        d41 <- h 4
        sep
        d42 <- h 12
        return $ GUID d1 d2 d3 (d41 `append` d42)

guidParser :: Parser GUID
guidParser = do
        begin
        x <- parseInner
        end
        return x

parseGuid :: Text -> Either String GUID
parseGuid s = parseOnly guidParser s

toHexLit :: String -> String
toHexLit s = "0x" ++ s ++ "u"

toCharList :: String -> [String]
toCharList [] = []
toCharList (c1:c2:cs) = [c1, c2] : toCharList cs

toInitializerList :: [String] -> String
toInitializerList bs = "{" ++ (intercalate "," bs) ++ "}"

toCppAggr :: GUID -> Text
toCppAggr guid =
        "GUID{" `append`
        (pack . toHexLit . unpack $ data1 guid) `append` "," `append`
        (pack . toHexLit . unpack $ data2 guid) `append` "," `append`
        (pack . toHexLit . unpack $ data3 guid) `append` "," `append`
        (pack . toInitializerList . map toHexLit . toCharList . unpack $ data4 guid) `append`
        "}"

makeOutput :: Either String GUID -> Text
makeOutput (Left s) = pack s
makeOutput (Right result) = toCppAggr result

run :: IO ()
run = do
        str <- getLine
        T.putStrLn . pack $ str
        T.putStrLn . makeOutput . parseGuid . pack $ str

