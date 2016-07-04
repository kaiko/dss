module Types
( Block(..)
, Sql(..)
, Subject
, Selector(..)
, SqlPart(..)
, Narrow(..)
, Rule(..)
, RuleValue(..)
) where

import Data.Scientific
import Data.Text as T

data Block = Block [Selector] [Rule] deriving (Show)

type Subject = T.Text
data Selector = Selector T.Text [Narrow] deriving (Show)

data Sql = Sql [SqlPart] deriving (Show)
data SqlPart = Raw T.Text | Let T.Text | MetaVal T.Text T.Text | Var T.Text deriving (Show)

data Narrow
  = At
  | Hash
  | OnInputSql Sql
  | OnInputSel [Narrow]
  | Exists T.Text     -- [id]
  | Begins   T.Text   -- [id^="blahh"]
  | Ends     T.Text   -- [id$="blahh"]
  | Contains T.Text   -- [id*="blahh"]
  deriving (Show)

data RuleValue
  = RuleString T.Text 
  | RuleNum Scientific
  | RuleSql Sql
  | RuleAuto
  -- | RuleValue T.Text  -- special stuff like "always"
  deriving (Show)

data Rule = Rule T.Text RuleValue deriving (Show)


