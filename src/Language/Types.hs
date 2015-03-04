module Language.Types where

import qualified Data.Text as T

type Text = T.Text
type Variable = Text

data Expr =  Var Variable
           | App Expr Expr
           | Lam Variable Expr deriving (Eq, Show)
