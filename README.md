# lambduh
Untyped lambda calculus in Haskell

'''sh
$>  cabal sandbox init
$>  cabal install
$>  alias lambduh='./dist/build/lambduh/lambduh'

$>  lambduh "(((\z -> (\x -> \y -> ((x) y) z)) (\x -> x)) (\x -> x)) z"
    ### z (\x -> x)
'''
