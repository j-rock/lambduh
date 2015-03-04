{-# LANGUAGE OverloadedStrings #-}

module Language.Parse where
    -- (
    --   interpret
    -- , interpretIO
    -- ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Language.Types


runTextParser :: Parser a -> String -> Either String a
runTextParser p = parseOnly (p <* endOfInput) . T.pack

interpret :: String -> Either String Expr
interpret = runTextParser parseExpr

parseExpr :: Parser Expr
parseExpr = parseApp <|> parseVar <|> parseLam

parseVar :: Parser Expr
parseVar = Var <$> withWithoutWSP identifier

identifier :: Parser Text
identifier = do l <- letter
                ls <- many (letter <|> digit <|> char '_')
                return $ T.pack (l:ls)

parseApp :: Parser Expr
parseApp = withWithoutWSP $ do e1 <- withParen parseExpr
                               e2 <- parseExpr
                               return (App e1 e2)

parseLam :: Parser Expr
parseLam = withWithoutWSP $ do _    <- char '\\' -- backslash char ==> \
                               var  <- identifier
                               _    <- with ws ws $ string "->"
                               body <- ws *> parseExpr
                               return $ Lam var body

ws :: Parser ()
ws = skipWhile $ liftA2 (||) isHorizontalSpace isEndOfLine

with :: Parser before -> Parser after -> Parser a -> Parser a
with b a p = b *> p <* a

withWithout :: Parser before -> Parser after -> Parser a -> Parser a
withWithout b a p = p <|> with b a p

paren f = f (char '(' >> ws) (ws >> char ')')

withParen :: Parser a -> Parser a
withParen = paren with

withWithoutParen :: Parser a -> Parser a
withWithoutParen = paren withWithout

withWithoutWSP :: Parser a -> Parser a
withWithoutWSP = with ws ws . withWithoutParen


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
