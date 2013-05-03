module Types where

data Value
    = Bool Bool
    | Number Integer
    | String String
    | Nil
    | Pair Value Value
    | Ident Ident
    deriving (Eq)

instance Show Value where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (String s) = show s
    show Nil = "()"
    show (Pair car cdr) = "(" ++ showCar car ++ showCdr cdr
      where
        showCar = show
        showCdr (Pair ca Nil) = " " ++ showCar ca ++ ")"
        showCdr (Pair ca cd) = " " ++ showCar ca ++ showCdr cd
        showCdr v = " . " ++ show v ++ ")"
    show (Ident i) = i

type Ident = String

isProperList :: Value -> Bool
isProperList (Pair _ Nil) = True
isProperList (Pair _ cdr@(Pair _ _)) = isProperList cdr
isProperList (Pair _ _) = False
isProperList _ = False

isDottedList :: Value -> Bool
isDottedList (Pair _ Nil) = False
isDottedList (Pair _ cdr@(Pair _ _)) = isDottedList cdr
isDottedList (Pair _ _) = True
isDottedList _ = False
