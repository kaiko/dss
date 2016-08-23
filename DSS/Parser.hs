{-# LANGUAGE OverloadedStrings #-}

module DSS.Parser (parseDSS) where

import Control.Applicative ((<$>), (<*>), (*>))
import Control.Monad (liftM)
import Control.Exception hiding (try)
-- import Text.ParserCombinators.Parsec hiding (spaces, State)
-- import Text.ParserCombinators.Parsec.Error
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Text.Parsec.Char
import Data.Maybe (isNothing)
import Data.List (isPrefixOf, break)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.UTF8 as UTF
import Data.Ratio
-- import qualified Data.Text as T
-- import Text.Parsec.Text -- NOTE: Text makes it much slower (parsec 3.1), try ByteString
import qualified Data.Text as T

import DSS.LangTypes

parseDSS x = parse (many block) "" x

block = Block <$> selector `sepBy` (spc >> comma >> spc) <*> between (spc >> char '{' >> spc) (spc >> char '}' >> spc) (many rule)

selector :: Parser Selector
selector = inSpace sPlain
-- selector = inSpace (try (Combined <$> (sPlain `sepBy` spc)) <|> sPlain) -- TODO: take one and if more, use Combined

sPlain :: Parser Selector
sPlain  = try sCombined <|> try sAll <|> try sTarget <|> try sPoint <|> try sHash <|> try sMedia <|> try sPseudo <|> try sExists <|> try sEquals <|> try sBegins <|> try sEnds <|> try sContains <|> try sDescendant <|> try sSibling <|> try sChild
sPlainX = try sAll <|> try sTarget <|> try sPoint <|> try sHash <|> try sMedia <|> try sPseudo <|> try sExists <|> try sEquals <|> try sBegins <|> try sEnds <|> try sContains

sAll, sTarget, sPoint, sHash, sMedia, sPseudo, sExists, sBegins, sEnds, sContains, sDescendant, sSibling, sChild :: Parser Selector
sAll    = char '*' >> return All
sTarget =             Target <$> target
sPoint  = char '.' >> Point  <$> target
sHash   = char '#' >> Hash   <$> target
sMedia  = char '@' >> Media  <$> target

sCombined = Combined <$> many1 sPlainX
sPseudo   = char ':' >> Pseudo <$> ident "-" <*> return Nothing
sExists   = Exists <$> between (char '[') (char ']') (ident "")
sEquals   = attr Equals   "="
sBegins   = attr Begins   "^="
sEnds     = attr Ends     "$="
sContains = attr Contains "*="

sDescendant = Descendant <$> followedBy spc               sPlainX <*> inSpace sPlainX
sSibling    = Sibling    <$> followedBy (inSpace $ char '+') sPlainX <*> inSpace sPlainX
sChild      = Child      <$> followedBy (inSpace $ char '>') sPlainX <*> inSpace sPlainX

-- sMediaA = char '@' >> (MediaA <$> ident "" <*> between (char '(') (char ')') (sepBy (spc >> char ',' >> spc) (ident "")))

attr :: (T.Text -> T.Text -> Selector) -> String -> Parser Selector
attr constr eq = between (char '[') (char ']') (constr <$> ident "-" <*> (T.pack <$> between (string eq >> char '"') (char '"') (many $ noneOf "\"")))

rule = between spc ruleEnd $ (Rule . T.pack <$> manyTill (alphaNum <|> oneOf "_-") (try (inSpace $ char ':')) <*> ruleValue)

ruleValue = ruleSql <|> ruleConst <|> ruleString <|> ruleNum
ruleEnd = spc >> ((char ';' >> spc) <|> (lookAhead (char '}' >> return ())))

ruleString = ValueStr   . T.pack <$> between (char '"') (char '"') ( many $ noneOf "\"" ) -- (:) <$> letter <*> many (alphaNum <|> char '_' <|> char '-') )
ruleConst  = ValueConst . T.pack <$> ((:) <$> letter <*> many (alphaNum <|> oneOf "_-"))
ruleNum    = ValueNum <$> nr
  where
    nr =  do
        a <- integer
        c <- optionMaybe (char '.' >> num)
        case c of
            Nothing -> return $ let Just (i, _) = C.readInt (C.pack a) in fromIntegral i
            Just ac -> return $ let Just (i, _) = C.readInteger (C.pack a)           -- TODO: better ways?
                                    Just (c, _) = C.readInteger (C.tail $ C.pack ac)
                                in fromRational (i % c)

num, plus, minus, integer :: Parser String
num = many1 digit
plus   =         char '+'  *> num
minus  = (:) <$> char '-' <*> num

-- https://www.fpcomplete.com/school/pick-of-the-week/parsing-floats-with-parsec
integer = plus <|> minus <|> num


ruleSql = string "sql" >> spc >> (ValueSql <$> between (char '(' >> spc) (spc >> char ')') ((ruleString <|> ruleConst) `sepBy` spc))

------------------------------
  
target = ident ".-"

ident :: String -> Parser T.Text
ident allowed = do
  a <- letter <|> char '_'
  b <- many (alphaNum <|> char '_' <|> oneOf allowed)
  return $ T.pack (a:b)

comma :: Parser Char
comma = char ','

spc = spaces >> optional (string "/*" >> manyTill anyChar (try (string "*/")) >> spaces)

narrower = narrowOnSql

narrowOnSql = char ':' >> (Pseudo <$> ident "-" <*> optionMaybe (between (char '(') (char ')') ruleValue))

followedBy close p = do { x <- p; close; return x }
inSpace = between spc spc

someVal = do
  a <- ident ""
  (char '.' >> (ValueField a <$> ident "")) <|> return (ValueConst a)

