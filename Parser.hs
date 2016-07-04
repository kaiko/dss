{-# LANGUAGE OverloadedStrings #-}

module Parser (parseDSS) where

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
import           Types
import qualified Data.Text as T

parseExprEither :: T.Text -> Either ParseError [Block]
parseExprEither s = parse blocks ("Error in expression: " `T.concat` s) s

{-
-- TODO: make shorter, all functions have almost same content
parseExpr:: B.ByteString -> ZValue
parseExpr expr =
  case parseExprEither expr
  of Left err -> throw $ ExprParseError err "Invalid zazler expression " -- TODO: more exact error should be given as an argument, I guess
     Right ex -> ex
-}

comma :: Parser Char
comma = char ','

multiSelector = sepBy comma selector

ident :: Parser T.Text
ident = do
  a <- letter <|> char '_'
  b <- many (alphaNum <|> char '_')
  return $ T.pack (a:b)

narrower = narrowOnSql <|> narrowOnSel

narrowOnSql = do
  string ":on-input-row"
  between (char '(') (char ')') (many1 sqlPart) >>= return . OnInputSql

sqlPart = (between (many spaces >> char '"') (char '"') anyChar >>= return . Raw . T.pack) <|> (many1 spaces >> someVal)
  
narrowOnSel = do
  string ":on-input-row"

white = skipMany spaces
  
blocks = many (white >> block)
block = do
  sel <- multiSelector 
  char '{'
  skipMany spaces
  rules <- many rule
  char '}'
  return $ Block sel rules

rule = do
  key <- ident
  white
  char ':'
  white
  val <- ruleValue
  return $ Rule key val

ruleValue = ruleSql <|> ruleString
ruleString = do
  a <- letter
  b <- many (alphaNum <|> char '_' <|> char '-')
  ruleEnd
  return $ RuleString $ T.pack (a:b)

ruleEnd = skipMany spaces >> char ';'

ruleSql = do
  string "sql"
  skipMany spaces
  sql <- between (char '(') (char ')') (many1 sqlPart)
  ruleEnd
  return $ RuleSql $ Sql sql

someVal = do
  a <- optionMaybe $ do
      i <- ident
      char '.'
      return i
  b <- ident
  return $ case a of
    Just aa -> MetaVal aa b
    Nothing -> Var b


