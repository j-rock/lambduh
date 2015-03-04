module Main
  (
    main
  ) where

import           Control.Applicative ((<$>))
import           Data.List           (intercalate)
import           Language.Lambduh
import           System.Environment  (getArgs)

main :: IO ()
main = do
    input <- intercalate " " <$> getArgs
    case reduce <$> interpret input of
        Left err -> print $ "Error: " ++ err
        Right s  -> print s

