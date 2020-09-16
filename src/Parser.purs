module Language.Dot.Parser
  ( parseDot
  , parsePort
  , parseCompass
  , parseAttribute
  , parseId
  )
  where

import Control.Alternative
import Data.Array
import Language.Dot.Syntax
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Language
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (when)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.String (CodePoint, codePointFromChar, toLower, uncons)
import Data.String as String
import Data.Tuple (Tuple(..))
import Global (readFloat)
import Text.Parsing.Parser.Combinators (between) as Parser

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parsec's "parse" takes a file name for the error code but runParser doesn't
parseDot :: String  -- ^ DOT source code
         -> Either ParseError Graph
parseDot =
    runParser (whiteSpace' *> parseGraph) $ preprocess

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preprocess :: String -> String
preprocess file =
    unlines $ map commentPoundLines $ lines file
  where
    commentPoundLines :: String -> String
    commentPoundLines line =
      case uncons line of
        (Just { head, tail }) -> 
            if head == (codePointFromChar '#') then "// " <> tail else line
        Nothing -> line

    unlines :: forall f. Foldable f => f String -> String
    unlines = intercalate "\n"

    lines :: String -> Array String
    lines "" = []
    lines s = String.split (String.Pattern "\n") s

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraph :: Parser Graph
parseGraph =
    ( Graph <$>
          parseGraphStrictness
      <*> parseGraphDirectedness
      <*> optionMaybe parseId
      <*> parseStatementList
    )
    <?> "graph"

parseGraphStrictness :: Parser GraphStrictness
parseGraphStrictness =
    ((reserved' "strict" *> pure StrictGraph) <|> pure UnstrictGraph)
    <?> "graph strictness"

parseGraphDirectedness :: Parser GraphDirectedness
parseGraphDirectedness =
    (   (reserved' "graph"   *> pure UndirectedGraph)
    <|> (reserved' "digraph" *> pure DirectedGraph)
    )
    <?> "graph directedness"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- parseDottedList :: SParser LispVal -> SParser LispVal
-- parseDottedList pars = do
--   init <- pars `endBy` whiteSpace
--   last <- char '.' *> whiteSpace *> pars
--   return $ DottedList init last

parseStatementList :: Parser (Array Statement)
parseStatementList = 
  braces' (parseStatement `endBy` optional semi')
    <?> "statement list"

parseStatement :: Parser Statement
parseStatement =
    (   try parseEdgeStatement
    <|> try parseAttributeStatement
    <|> try parseAssignmentStatement
    <|> try parseSubgraphStatement
    <|>     parseNodeStatement
    )
    <?> "statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeStatement :: Parser Statement
parseNodeStatement =
    ( NodeStatement <$>
      parseNodeId <*> parseAttributeList
    )
    <?> "node statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEdgeStatement :: Parser Statement
parseEdgeStatement =
    ( EdgeStatement <$>
      parseEntityList <*> parseAttributeList
    )
    <?> "edge statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeStatement :: Parser Statement
parseAttributeStatement =
    ( AttributeStatement <$>
      parseAttributeStatementType <*> parseAttributeList
    )
    <?> "attribute statement"

parseAttributeStatementType :: Parser AttributeStatementType
parseAttributeStatementType =
    (   (reserved' "graph" *> pure GraphAttributeStatement)
    <|> (reserved' "node"  *> pure NodeAttributeStatement)
    <|> (reserved' "edge"  *> pure EdgeAttributeStatement)
    )
    <?> "attribute statement type"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAssignmentStatement :: Parser Statement
parseAssignmentStatement =
    ( AssignmentStatement <$>
      parseId <*> (reservedOp' "=" *> parseId)
    )
    <?> "assignment statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseSubgraphStatement :: Parser Statement
parseSubgraphStatement =
    ( SubgraphStatement <$>
       parseSubgraph
    )
    <?> "subgraph statement"

parseSubgraph :: Parser Subgraph
parseSubgraph =
    (   try parseNewSubgraph
    <|>     parseSubgraphRef
    )
    <?> "subgraph"

parseNewSubgraph :: Parser Subgraph
parseNewSubgraph =
    ( NewSubgraph <$>
      (optional (reserved' "subgraph") *> optionMaybe parseId) <*> parseStatementList
    )
    <?> "new subgraph"

parseSubgraphRef :: Parser Subgraph
parseSubgraphRef =
    ( SubgraphRef <$>
      (reserved' "subgraph" *> parseId)
    )
    <?> "subgraph ref"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEntityList :: Parser (Array Entity)
parseEntityList =
    ( (:) <$>
      parseEntity true <*> some (parseEntity false)
    )
    <?> "entity list"

parseEntity :: Boolean-> Parser Entity
parseEntity first =
    (   try (parseENodeId first)
    <|>     parseESubgraph first
    )
    <?> "entity"

parseENodeId :: Boolean-> Parser Entity
parseENodeId first =
    ( ENodeId <$>
      (if first then pure NoEdge else parseEdgeType) <*> parseNodeId
    )
    <?> "entity node id"

parseESubgraph :: Boolean-> Parser Entity
parseESubgraph first =
    ( ESubgraph <$>
      (if first then pure NoEdge else parseEdgeType) <*> parseSubgraph
    )
    <?> "entity subgraph"

parseEdgeType :: Parser EdgeType
parseEdgeType =
    (   try (reservedOp' "->" *> pure DirectedEdge)
    <|>     (reservedOp' "--" *> pure UndirectedEdge)
    )
    <?> "edge operator"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeId :: Parser NodeId
parseNodeId =
    ( NodeId <$>
      parseId <*> optionMaybe parsePort
    )
    <?> "node id"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePort :: Parser Port
parsePort =
    (   try parsePortC
    <|>     parsePortI
    )
    <?> "port"

parsePortC :: Parser Port
parsePortC =
    ( PortC <$>
      (colon' *> parseCompass)
    )
    <?> "port (compass variant)"

parsePortI :: Parser Port
parsePortI =
    ( PortI <$>
      (colon' *> parseId) <*> optionMaybe (colon' *> parseCompass)
    )
    <?> "port (id variant)"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- parseCompass :: Parser Compass
parseCompass =
    (map (toLower >>> convert) identifier' >>= maybe err pure)
    <?> "compass"
  where
    err = fail "invalid compass value" 
    convert = 
      case _ of
        "n"  -> Just CompassN
        "e"  -> Just CompassE
        "w"  -> Just CompassW
        "s"  -> Just CompassS
        "ne" -> Just CompassNE
        "nw" -> Just CompassNW 
        "se" -> Just CompassSE
        "sw" -> Just CompassSW
        _    -> Nothing

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeList :: Parser (Array Attribute)
parseAttributeList =
    (brackets' (parseAttribute `sepBy` optional comma') <|> pure [])
    <?> "attribute list"

parseAttribute :: Parser Attribute
parseAttribute =
    ( do
      id0 <- parseId
      id1 <- optionMaybe (reservedOp' "=" *> parseId)
      pure $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1
    )
    <?> "attribute"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseId :: Parser Id
parseId =
    (   try parseNameId
    <|> try parseStringId
    <|> try parseFloatId
    <|> try parseIntegerId
    <|>     parseXmlId
    )
    <?> "id"

-- parseNameId :: Parser Id
parseNameId =
    ( NameId <$>
      identifier'
    )
    <?> "name"

parseStringId :: Parser Id
parseStringId =
    ( StringId <$>
      lexeme' (char '"' *> manyTill stringChar (char '"'))
    )
    <?> "string literal"
  where
    stringChar =
        (try (string "\\\"" *> pure '"') <|> noneOf "\"")
        <?> "string character"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | DOT allows floating point numbers having no whole part like @.123@,
-- | and so does JavaScript's parseFloat which underlies the PureScript readFloat function
-- | which enables the PureScript implementation to simplify this parser
parseFloatId :: Parser Id
parseFloatId =
  ( FloatId <$>
    float'
  )
  <?> "float"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseIntegerId :: Parser Id
parseIntegerId =
    ( IntegerId <$>
      integer'
    )
    <?> "integer"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseXmlId :: Parser Id
parseXmlId =
    ( XmlId <$>
      angles' parseXml
    )
    <?> "XML id"

parseXml :: Parser Xml
parseXml =
    (   try parseXmlEmptyTag
    <|> try parseXmlTag
    <|>     parseXmlText
    )
    <?> "XML"

parseXmlEmptyTag :: Parser Xml
parseXmlEmptyTag =
    ( XmlEmptyTag <$>
      (char '<' *> parseXmlName) <*> (parseXmlAttributes <* (char '/' *> char '>'))
    )
    <?> "XML empty tag"

parseXmlTag :: Parser Xml
parseXmlTag =
    ( do (Tuple name attributes) <- parseXmlTagOpen
         elements           <- manyTill parseXml (lookAhead (try (parseXmlTagClose (Just name))))
         parseXmlTagClose (Just name)
         pure $ XmlTag name attributes elements
    )
    <?> "XML tag"

parseXmlTagOpen :: Parser (Tuple XmlName (Array XmlAttribute))
parseXmlTagOpen =
    ( Tuple <$>
      (char '<' *> parseXmlName) <*> (parseXmlAttributes <* char '>')
    )
    <?> "XML opening tag"

parseXmlTagClose :: Maybe XmlName -> Parser Unit
parseXmlTagClose mn0 =
    ( do _  <- char '<'
         _  <- char '/'
         n1 <- parseXmlName
         _  <- char '>'
         when (isJust mn0 && fromJust mn0 /= n1) empty
    )
    <?> "XML closing tag " <> "(" <> which <> ")"
  where
    which =
        case mn0 of
          Just (XmlName n) -> "for " <> show n
          Nothing          -> "any"

parseXmlText :: Parser Xml
parseXmlText =
    ( XmlText <$>
      anyChar `manyTill` lookAhead (   try (parseXmlEmptyTag *> pure unit)
                                   <|> try (parseXmlTag      *> pure unit)
                                   <|>      parseXmlTagClose Nothing
                                   )
    )
    <?> "XML text"

parseXmlAttributes :: Parser (Array XmlAttribute)
parseXmlAttributes =
    many parseXmlAttribute
    <?> "XML attribute list"

parseXmlAttribute :: Parser XmlAttribute
parseXmlAttribute =
    ( XmlAttribute <$>
      (parseXmlName <* reservedOp' "=") <*> parseXmlAttributeValue
    )
    <?> "XML attribute"

parseXmlAttributeValue :: Parser XmlAttributeValue
parseXmlAttributeValue =
    ( XmlAttributeValue <$>
      stringLiteral'
    )
    <?> "XML attribute value"

parseXmlName :: Parser XmlName
parseXmlName =
    ( XmlName <$>
      ((:) <$> c0 <*> (many c1 <* whiteSpace'))
    )
    <?> "XML name"
  where
    c0 = letter   <|> cs
    c1 = alphaNum <|> cs
    cs = oneOf "-.:_"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-----------------------------------------------------------
-- Bracketing
-----------------------------------------------------------
parens p        = Parser.between (string "(") (string ")") p
braces p        = Parser.between (string "{") (string "}") p
angles p        = Parser.between (string "<") (string ">") p
brackets p      = Parser.between (string "[") (string "]") p

semi            = string ";"
comma           = string ","
dot             = string "."
colon           = string ":"

commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi


angles'        :: Parser a -> Parser a
angles'        = dotParser.angles

braces'        :: Parser a -> Parser a
braces'        = dotParser.braces

brackets'      :: Parser a -> Parser a
brackets'      = dotParser.brackets

colon'         :: Parser String
colon'         = dotParser.colon

comma'         :: Parser String
comma'         = dotParser.comma

float'         :: Parser String
float'         = dotParser.float

identifier'    :: Parser String
identifier'    = dotParser.identifier

integer'       :: Parser Int
integer'       = dotParser.integer

lexeme'        :: Parser a -> Parser a
lexeme'        = dotParser.lexeme

reserved'      :: String -> Parser Unit
reserved'      = dotParser.reserved

reservedOp'    :: String -> Parser Unit
reservedOp'    = dotParser.reservedOp

semi'          :: Parser String
semi'          = dotParser.semi

stringLiteral' :: Parser String
stringLiteral' = dotParser.stringLiteral

whiteSpace'    :: Parser Unit
whiteSpace'    = dotParser.whiteSpace

-- | A lexer for the dot language.
dotParser :: TokenParser
dotParser = makeTokenParser dotDef


-- | The language definition for the language dot.
dotDef :: LanguageDef
dotDef = LanguageDef (unGenLanguageDef emptyDef)
                { reservedOpNames = ["->", "--", "="]
                , reservedNames   = ["digraph", "edge", "graph", "node", "strict", "subgraph"]
                , commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = true
                , identStart      = letter   <|> char '_'
                , identLetter     = alphaNum <|> char '_'
                , opStart         = oneOf ['-','=']
                , opLetter        = oneOf []
                , caseSensitive   = false
                }
