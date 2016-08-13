{-# LANGUAGE OverloadedStrings #-}
-- echo -n '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' | stack exec guid-exe

module Lib
    ( GUID(GUID)
    , run
    , parseGuid
    , toCppAggr
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Char (isHexDigit)
import Data.List (foldl', intercalate)
import qualified Data.Text as T (Text, pack, unpack, append, empty, intercalate)
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

data GUID = GUID
          { data1 :: T.Text
          , data2 :: T.Text
          , data3 :: T.Text
          , data4 :: T.Text
          }
          deriving (Show, Eq, Ord)

begin :: Parser Char
begin = char '{' <?> "begin brace"
end :: Parser Char
end = char '}' <?> "end brace"
sep :: Parser Char
sep = char '-' <?> "digit separator"
hexDigit :: Parser Char
hexDigit = satisfy isHexDigit <?> "isHexDigit"
h :: Int -> Parser T.Text
h n = T.pack <$> count n hexDigit
        <?> "n times digits"

parseData1 :: Parser T.Text
parseData1 = h 8 <?> "Data1"
parseData2 :: Parser T.Text
parseData2 = h 4 <?> "Data2"
parseData3 :: Parser T.Text
parseData3 = h 4 <?> "Data3"
parseData4 :: Parser T.Text
parseData4 = T.append <$> h 4 <* sep <*> h 12
        <?> "Data4"

parseInner :: Parser GUID
parseInner = GUID
        <$> parseData1 <* sep
        <*> parseData2 <* sep
        <*> parseData3 <* sep
        <*> parseData4
        <?> "inner part"

guidWithBraces :: Parser GUID
guidWithBraces = begin *> parseInner <* end
        <?> "GUID with braces"

guidWithoutBraces :: Parser GUID
guidWithoutBraces = parseInner <?> "GUID without braces"

guidParser :: Parser GUID
guidParser = choice [guidWithBraces, guidWithoutBraces]

parseGuid :: T.Text -> Either String GUID
parseGuid s = parseOnly guidParser s

toHexLit :: String -> String
toHexLit s = "0x" ++ s ++ "u"

toCharList :: String -> [String]
toCharList [] = []
toCharList (c1:c2:cs) = [c1, c2] : toCharList cs

toInitializerList :: [String] -> String
toInitializerList bs = "{" ++ (intercalate "," bs) ++ "}"

toCppAggr :: GUID -> T.Text
toCppAggr guid = foldl' T.append T.empty d
    where
        d = [hd, ds, tl]
        hd = T.pack "GUID{"
        ds = T.intercalate (T.pack ",") [d1, d2, d3, d4]
        d1 = toHex $ data1 guid
        d2 = toHex $ data2 guid
        d3 = toHex $ data3 guid
        d4 = toArr $ data4 guid
        toHex = T.pack . toHexLit . T.unpack
        toArr = T.pack . toInitializerList . map toHexLit . toCharList . T.unpack
        tl = T.pack "}"

makeOutput :: Either String GUID -> T.Text
makeOutput (Left s) = T.pack s
makeOutput (Right result) = toCppAggr result

run :: IO ()
run = do
        str <- getLine
        T.putStrLn . T.pack $ str
        T.putStrLn . makeOutput . parseGuid . T.pack $ str

