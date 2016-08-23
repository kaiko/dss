{-# LANGUAGE OverloadedStrings #-}

module DSS.Writer (writeDSS) where

import qualified Data.Text as T
import DSS.LangTypes


writeDSS :: [Block] -> T.Text
writeDSS b = T.concat (map writeBlock b)

writeBlock (Block [] _) = error "DSS is missing rule"
writeBlock (Block s []) = T.concat [ T.intercalate ", " (map writeSel s), T.pack " { }\n" ]
writeBlock (Block s (x:[])) = T.concat [ T.intercalate ", " (map writeSel s), T.pack " { ", writeRule x, " }\n" ]
writeBlock (Block x r) = T.concat [ T.intercalate ", " (map writeSel x), s " { \n\t", T.intercalate (s "\n\t") $ map writeRule r, s "\n}\n" ]

writeSel All = T.singleton '*'
writeSel (Target x) = x
writeSel (Point x)  = c '.' ... x
writeSel (Hash  x)  = c '#' ... x
writeSel (Pseudo x Nothing)  = c ':' ... x
writeSel (Pseudo x (Just y))  = c ':' ... x ... (inC '(' ')' $ writeVal y)
writeSel (Exists x  ) = inC '[' ']' x
writeSel (Equals x y) = inC '[' ']' $ x ... c '='  ... y
writeSel (Begins x y) = inC '[' ']' $ x ... s "^=\"" ... y ... c '"'
writeSel (Ends   x y) = inC '[' ']' $ x ... s "$=\"" ... y ... c '"'
writeSel (Contains x y) = inC '[' ']' $ x ... s "*=\"" ... y ... c '"'
writeSel (Combined xs) = T.concat $ map writeSel xs
writeSel (Descendant a b) = writeSel a ... c ' ' ... writeSel b
writeSel (Sibling    a b) = writeSel a ... s " + " ... writeSel b
writeSel (Child       a b) = writeSel a ... s " > " ... writeSel b
writeSel (Media       a  ) = (c '@') ... a
-- writeSel (MediaA      a  ) = (c '@') ... a

eq = T.singleton 
inC a b x = T.singleton a ... x ... T.singleton b

(...) = T.append
c = T.singleton
s = T.pack

writeRule (Rule x val) = x ... ": " ... writeVal val ... ";"

writeVal (ValueNum s) = T.pack $ show s
writeVal (ValueStr s) = inC '"' '"' s
writeVal (ValueConst s) = s
writeVal (ValueField t f) = t ... c '.' ... f
writeVal (ValueSql xs) = s "sql(" ... T.intercalate (c ' ') (map writeVal xs) ... c ')'

