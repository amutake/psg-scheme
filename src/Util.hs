module Util where

import Types.Syntax

nil :: List a
nil = ProperList []

cons :: a -> List a -> List a
cons x (ProperList xs) = ProperList (x:xs)
cons x (DottedList xs x') = DottedList (x:xs) x'
