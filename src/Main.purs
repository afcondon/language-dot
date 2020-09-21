module Main where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Language.Dot.Parser (parseGraph)
import Prelude (class Eq, class Monad, class Show, Unit, bind, const, discard, pure, show, ($), (*), (+), (-), (/), (<$>), (<>), (>>=))

import Data.Either (Either(..))
import Data.List (fromFoldable)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, ParserT, runParser)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, token)

data TestToken = A | B

instance showTestTokens :: Show TestToken where
  show A = "A"
  show B = "B"

instance testTokensEq :: Eq TestToken where
  eq A A = true
  eq B B = true
  eq _ _ = false

isA :: TestToken -> Boolean
isA A = true
isA _ = false

testTokenParser :: TokenParser
testTokenParser = makeTokenParser haskellDef

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

quotes :: forall m a. Monad m => ParserT String m a -> ParserT String m a
quotes = between (string "\"") (string "\"")

nested :: forall m. Monad m => ParserT String m Int
nested = fix \p -> (do
  _ <- string "a"
  pure 0) <|> ((+) 1) <$> parens p

nested' :: forall m. Monad m => ParserT String m Int
nested' = fix \p -> (do
  _ <- string "a"
  pure 0) <|> ((+) 1) <$> quotes p

digit :: Parser String Int
digit = (string "0" >>= \_ -> pure 0)
        <|> (string "1" >>= \_ -> pure 1)
        <|> (string "2" >>= \_ -> pure 2)
        <|> (string "3" >>= \_ -> pure 3)
        <|> (string "4" >>= \_ -> pure 4)
        <|> (string "5" >>= \_ -> pure 5)
        <|> (string "6" >>= \_ -> pure 6)
        <|> (string "7" >>= \_ -> pure 7)
        <|> (string "8" >>= \_ -> pure 8)
        <|> (string "9" >>= \_ -> pure 9)

exprTest :: Parser String Int
exprTest = buildExprParser [ [ Infix (string "/" >>= \_ -> pure (/)) AssocRight ]
                           , [ Infix (string "*" >>= \_ -> pure (*)) AssocRight ]
                           , [ Infix (string "-" >>= \_ -> pure (-)) AssocRight ]
                           , [ Infix (string "+" >>= \_ -> pure (+)) AssocRight ]
                           ] digit

simpleGraph :: String
simpleGraph = """graph {
    a -- b;
    b -- c;
    a -- c;
    d -- c;
    e -- c;
    e -- a;
}"""

main :: Effect Unit
main = do
  let tokpos = const initialPos
  let tokList = fromFoldable [A, B]
  log $ case runParser tokList (token tokpos) of
    (Right result) -> "Worked " <> show result
    (Left err) -> "Left " <> show err
  log $ case runParser "(((a)))" nested of
    (Right result) -> "Worked " <> show result
    (Left err) -> "Left " <> show err
  log $ case runParser "\"\"\"\"a\"\"\"\"" nested' of
    (Right result) -> "Worked " <> show result
    (Left err) -> "Left " <> show err
  log $ case runParser "1*2+3/4-5" exprTest of
    (Right result) -> "Worked " <> show result
    (Left err) -> "Left " <> show err
  log $ case runParser simpleGraph parseGraph of
    (Right result) -> "Worked " <> show result
    (Left err) -> "Left " <> show err
  
