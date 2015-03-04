module Language.Lambduh
  (
    reduce
  , interpret
  , Expr(..)
  ) where

import qualified Data.Text      as T
import           Language.Parse (interpret)
import           Language.Types

reduce :: Expr -> Expr
reduce e@(App e1 e2) = let e' = betaReduce e1 e2
                       in if e == e' then e else reduce e'
reduce e             = e

betaReduce :: Expr -> Expr -> Expr
betaReduce (Lam v body) arg = substitute v arg body
betaReduce e1           e2  = App e1 e2

substitute :: Variable -> -- Bound variable of a Lambda
              Expr     -> -- Expression to replace that variable
              Expr     -> -- Expression in which to replace that variable
              Expr        -- Returned expression
substitute x s (Var y)     = if x == y then s else Var y
substitute x s (App e1 e2) = App (substitute x s e1) (substitute x s e2)
substitute x s (Lam v body)| v == x               = Lam v body
                           | not (occursFree x s) = Lam v (substitute x s body)
                           | otherwise            = substitute x s $ alpha v body x s


occursFree :: Variable -> Expr -> Bool
occursFree x (Var v)      = x == v
occursFree x (App e1 e2)  = occursFree x e1 || occursFree x e2
occursFree x (Lam v body) = x /= v && occursFree x body

--
--   alpha x e1 v e2 tries to create a lambda expression (Lam z e)
--   that is alpha-equivalent to (Lam x e1) such that z is not v
--   and does not occur free in either e1 or e2.
--
alpha ::   Variable -> Expr -> Variable -> Expr -> Expr
alpha x e1 v e2 =
    let augment = T.cons '1'

        findNewVar z| z == v || occursFree z (App e1 e2) = findNewVar $ augment z
        findNewVar z = z

        z' = findNewVar $ augment x

    in Lam z' $ substitute x (Var z') e1
