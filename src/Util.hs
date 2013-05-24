module Util where

import Types.Util

cons :: a -> List a -> List a
cons x (ProperList xs) = ProperList (x:xs)
cons x (DottedList xs x') = DottedList (x:xs) x'

consArgs :: Ident -> Args -> Args
consArgs var (Args list) = Args $ cons var list
