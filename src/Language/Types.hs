module Language.Types where

import qualified Data.Text as T

type Text = T.Text
type Variable = Text

data Expr =  Var Variable
           | App Expr Expr
           | Lam Variable Expr deriving (Eq)

instance Show Expr where
    show (Var v)     = T.unpack v
    show (App e1 e2) = show e1 ++ " " ++ show e2
    show (Lam v (Var x)) = "(\\" ++ T.unpack v ++ " -> " ++ T.unpack x ++ ")"
    show (Lam v e)   = "\\" ++ T.unpack v ++ " -> \n"
                         ++ (unlines . map ("  " ++) . lines) (show e)
