module Eval where

import Types

eval :: Value -> Value
eval b@(Bool _) = b
eval n@(Number _) = n
eval s@(String _) = s
eval Nil = Nil
eval (Pair (Ident "quote") (Pair cdr Nil)) = cdr
eval (Ident _) = undefined
