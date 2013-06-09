{-# LANGUAGE FlexibleContexts, Rank2Types, ConstraintKinds #-}

module Primitives where

import Control.Monad (foldM)
import Control.Monad.Error (MonadError (..))
import Data.Monoid (mempty)
import Text.Trifecta (parseString, Result (..))

import Parser (parseNumber)
import Types.Core
import Types.Exception
import Types.Syntax
import Util

type PrimFunc = MonadScheme m => [Expr] -> SchemeT m Expr

applyPrim :: Prim -> PrimFunc
applyPrim Add = primAdd
applyPrim Sub = primSub
applyPrim Mul = primMul
applyPrim Div = primDiv
applyPrim Equal = primEqual
applyPrim NLT = primLT
applyPrim NGT = primGT
applyPrim Eqv = primEqv
applyPrim Car = primCar
applyPrim Cdr = primCdr
applyPrim Cons = primCons
applyPrim Pair = primPair
applyPrim NumberP = primNumberP
applyPrim SymbolP = primSymbolP
applyPrim BooleanP = primBooleanP
applyPrim StringP = primStringP
applyPrim StringAppend = primStringAppend
applyPrim SymbolString = primSymbolString
applyPrim StringSymbol = primStringSymbol
applyPrim NumberString = primNumberString
applyPrim StringNumber = primStringNumber
applyPrim ProcP = primProcP
applyPrim EqualP = primEqualP
applyPrim EqP = primEqP

primAdd :: PrimFunc
primAdd = foldM add (Const $ Number 0)
  where
    add (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n + m
    add _ _ = throwError $ TypeMismatch "Number"

primSub :: PrimFunc
primSub [] = throwError $ NumArgs "(-): 1 <= args"
primSub (x:xs) = foldM sub x xs
  where
    sub (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n - m
    sub _ _ = throwError $ TypeMismatch "Number"

primMul :: PrimFunc
primMul = foldM mul (Const (Number 1))
  where
    mul (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n * m
    mul _ _ = throwError $ TypeMismatch "Number"

primDiv :: PrimFunc
primDiv [] = throwError $ NumArgs "(/): 1 <= args"
primDiv ((Const (Number n)):[]) = return $ Const $ Number $ 1 `div` n
primDiv (_:[]) = throwError $ TypeMismatch "Number"
primDiv (x:xs) = foldM div' x xs
  where
    div' (Const (Number n)) (Const (Number m)) =
        return $ Const $ Number $ n `div` m
    div' _ _ = throwError $ TypeMismatch "Number"

primEqual :: PrimFunc
primEqual [] = throwError $ NumArgs "(=): 2 <= args"
primEqual (_:[]) = throwError $ NumArgs "(=): 2 <= args"
primEqual (x:xs)
    | all isNumber (x:xs) = bool $ all (== x) xs
    | otherwise = throwError $ TypeMismatch "Number"

isNumber :: Expr -> Bool
isNumber (Const (Number _)) = True
isNumber _ = False

primLT :: PrimFunc
primLT ((Const (Number n)):(Const (Number m)):ns)
    | ns == [] = bool $ n < m
    | otherwise = do
        (Const (Bool b)) <- primLT $ (Const $ Number m) : ns
        bool $ n < m && b
primLT _ = throwError $ NumArgs "(<): 2 <= args"

primGT :: PrimFunc
primGT ((Const (Number n)):(Const (Number m)):ns)
    | ns == [] = bool $ n > m
    | otherwise = do
        (Const (Bool b)) <- primGT $ (Const $ Number m) : ns
        bool $ n > m && b
primGT _ = throwError $ NumArgs "(>): 2 <= args"

primEqv :: PrimFunc
primEqv xs
    | length xs == 2 = bool $ xs !! 0 == xs !! 1
    | otherwise = throwError $ NumArgs "eqv?: args == 2"

primCar :: PrimFunc
primCar [List (ProperList (x : _))] = return x
primCar [List (DottedList (x : _) _)] = return $ x
primCar [_] = throwError $ TypeMismatch "pair"
primCar _ = throwError $ NumArgs "car: args == 1"

primCdr :: PrimFunc
primCdr [List (ProperList [])] = return $ List nil
primCdr [List (ProperList (_ : xs))] = return $ List $ ProperList xs
primCdr [List (DottedList [_] x)] = return x
primCdr [List (DottedList (_ : xs) x)] = return $ List $ DottedList xs x
primCdr [_] = throwError $ TypeMismatch "Pair"
primCdr _ = throwError $ NumArgs "cdr: args == 1"

primCons :: PrimFunc
primCons [x, List (ProperList [])] = return $ List $ cons x nil
primCons [x, List (ProperList ys)] = return $ List $ cons x $ ProperList ys
primCons [x, List (DottedList ys y)] = return $ List $ DottedList (x : ys) y
primCons [x, y] = return $ List $ DottedList [x] y
primCons _ = throwError $ NumArgs "cons: args == 2"

primPair :: PrimFunc
primPair [List _] = bool True
primPair [_] = bool False
primPair _ = throwError $ NumArgs "pair?: args == 1"

primNumberP :: PrimFunc
primNumberP [n] = bool $ isNumber n
primNumberP _ = throwError $ NumArgs "number?: args == 1"

primSymbolP :: PrimFunc
primSymbolP [Ident _] = bool True
primSymbolP [Normalized (Prim _)] = bool True
primSymbolP [_] = bool False
primSymbolP _ = throwError $ NumArgs "symbol?: args == 1"

primBooleanP :: PrimFunc
primBooleanP [Const (Bool _)] = bool True
primBooleanP [_] = bool False
primBooleanP _ = throwError $ NumArgs "boolean?: args == 1"

primStringP :: PrimFunc
primStringP [Const (String _)] = bool True
primStringP [_] = bool False
primStringP _ = throwError $ NumArgs "string?: args == 1"

primStringAppend :: PrimFunc
primStringAppend [Const (String s), Const (String t)] = return $ Const $ String $ s ++ t
primStringAppend [_, _] = throwError $ TypeMismatch "String"
primStringAppend _ = throwError $ NumArgs "string-append: args == 2"

primProcP :: PrimFunc
primProcP [Evaled (Func _ _ _)] = bool True
primProcP [_] = bool False
primProcP _ = throwError $ NumArgs "procedure?: args == 1"

primSymbolString :: PrimFunc
primSymbolString [Ident i] = return $ Const $ String i
primSymbolString [Normalized (Prim p)] = return $ Const $ String $ show p
primSymbolString [_] = throwError $ TypeMismatch "Symbol"
primSymbolString _ = throwError $ NumArgs "symbol->string: args == 1"

primStringSymbol :: PrimFunc
primStringSymbol [Const (String s)] = return $ prim s
primStringSymbol [_] = throwError $ TypeMismatch "String"
primStringSymbol _ = throwError $ NumArgs "string->symbol: args == 1"

primNumberString :: PrimFunc
primNumberString [Const (Number n)] = return $ Const $ String $ show n
primNumberString [_] = throwError $ TypeMismatch "Number"
primNumberString _ = throwError $ NumArgs "number->string: args == 1"

primStringNumber :: PrimFunc
primStringNumber [Const (String s)] = case parseString parseNumber mempty s of
    Success c -> return $ Const c
    Failure _ -> bool False
primStringNumber [_] = throwError $ TypeMismatch "String"
primStringNumber _ = throwError $ NumArgs "string->number: args == 1"

primEqualP :: PrimFunc
primEqualP [e1, e2] = bool $ show e1 == show e2
primEqualP _ = throwError $ NumArgs "equal?: args == 2"

primEqP :: PrimFunc
primEqP [Const (Bool p), Const (Bool q)] = bool $ p == q
primEqP [Const (Number n), Const (Number m)] = bool $ n == m
primEqP [Const Undefined, Const Undefined] = bool True
primEqP [Ident i, Ident k] = bool $ i == k
primEqP [List (ProperList []), List (ProperList [])] = bool True
primEqP [Normalized (Prim p), Normalized (Prim q)] = bool $ p == q
primEqP [_, _] = bool False
primEqP _ = throwError $ NumArgs "eq?: args == 2"

bool :: MonadScheme m => Bool -> SchemeT m Expr
bool = return . Const . Bool
