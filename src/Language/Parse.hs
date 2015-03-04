{-# LANGUAGE OverloadedStrings #-}

module Language.Parse
  (
    interpret
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Language.Types

type ErrorString = String


{-
    interpret tries to turn a String into an Expr.

    Expr: var or lambda or app

    identifier: [:alpha:][:alpha: | :digit: | _ ]*
            e.g. a, aa, a1, a_, or a_11_aaa

    Valid var: identifier or parenthesized once
            e.g. a, (a), or (a_11_)

    Valid lambda: \identifier -> Expr or parenthesized once
            e.g. \x -> x, \x -> \y -> x, or \x -> (\y -> x)

    Valid app: (Expr) Expr or parenthesized once
            e.g. (x) y, ((x) y), or (\x -> x) x
-}
interpret :: String -> Either ErrorString Expr
interpret = runTextParser parseExpr

runTextParser :: Parser a -> String -> Either String a
runTextParser p = parseOnly (p <* endOfInput) . T.pack

parseExpr :: Parser Expr
parseExpr = parseApp <|> parseVar <|> parseLam

parseVar :: Parser Expr
parseVar = Var <$> withWithoutWSP identifier

identifier :: Parser Text
identifier = do l <- letter
                ls <- many (letter <|> digit <|> char '_')
                return $ T.pack (l:ls)

--so far, first term must be parenthesized lest we incur an infinite loop
parseApp :: Parser Expr
parseApp = withWithoutWSP $ do e1 <- withParen parseExpr
                               e2 <- parseExpr
                               return (App e1 e2)

parseLam :: Parser Expr
parseLam = withWithoutWSP $ do _    <- char '\\'
                               var  <- identifier
                               _    <- withWS $ string "->"
                               body <- parseExpr
                               return $ Lam var body


with :: Parser before -> Parser after -> Parser a -> Parser a
with b a p = b *> p <* a

withWithout :: Parser before -> Parser after -> Parser a -> Parser a
withWithout b a p = p <|> with b a p

ws :: Parser ()
ws = skipWhile $ liftA2 (||) isHorizontalSpace isEndOfLine

withWS :: Parser a -> Parser a
withWS = with ws ws

withParen :: Parser a -> Parser a
withParen = with (char '(') (char ')') . withWS

withWithoutParen :: Parser a -> Parser a
withWithoutParen = withWithout (char '(') (char ')') . withWS

withWithoutWSP :: Parser a -> Parser a
withWithoutWSP = withWS . withWithoutParen



succed = either (const False) (const True)
failed = either (const True) (const False)
fs = filter (failed . interpret) examples
ts = filter (succed . interpret) examples

examples = [
        "x",
        "x_y11_",
        "(x)",
        "\\x -> x",
        "\\x -> (x)",
        "(\\x -> x)",
        " (\\x -> (x) )",
        "\\x -> \\y -> \\z -> \\q -> q",
        "\\x -> (x) (\\y -> y)",
        "(\\x -> (x) (\\y -> y) z)",
        "(x) (y)",
        "(x) y",
        "( (x) y ) z",
        " ( \\x -> y ) \\q -> j",
        " ( \\x -> y ) (  \\q ->   j )",

        "\\x -> x (\\y -> y z)",
        "\\x -> x y",
        "x y",
        "(x y)",
        " (x y) ",
        "((x y) z)"
    ]
