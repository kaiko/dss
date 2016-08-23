module DSS.LangTypes
( Block    (..)
, Selector (..)
, Rule     (..)
, MediaArg (..)
, Value    (..)
) where

import Data.Scientific
import Data.Text as T


data Rule = Rule T.Text Value deriving (Show)
data Block = Block [Selector] [Rule] deriving (Show)

data Value = ValueNum Scientific | ValueStr T.Text | ValueConst T.Text | ValueField T.Text T.Text | ValueSql [Value] deriving (Show)

data Selector
  = All               -- *
  | Target    T.Text  -- foo
  | Point     T.Text  -- .foo
  | Hash      T.Text  -- #foo
  | Pseudo    T.Text  (Maybe Value) -- foo:on
  | Exists    T.Text  -- [id]
  | Equals    T.Text T.Text -- [id="blahh"]
  | Begins    T.Text T.Text -- [id^="blahh"]
  | Ends      T.Text T.Text -- [id$="blahh"]
  | Contains  T.Text T.Text -- [id*="blahh"]
  | Combined  [Selector]    -- a, b
  | Descendant Selector Selector -- a b
  | Sibling    Selector Selector -- a + b
  | Child      Selector Selector -- a > b

  | Media  T.Text
  | MediaA T.Text [MediaArg]
  deriving (Show)

-- data SqlPart = Raw T.Text | Let T.Text | MetaVal T.Text T.Text | Var T.Text deriving (Show)

data MediaArg = ArgNum Scientific | ArgName T.Text deriving (Show)

