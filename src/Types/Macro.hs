module Types.Macro where

import Data.Map (Map)

import Types.Util
import Types.Syntax.After

type Macro = Map Ident MacroBody

data MacroBody = MacroBody Args Expr deriving (Show)