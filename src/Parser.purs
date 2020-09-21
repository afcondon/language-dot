module Language.Dot.Parser
  ( parseDot
  , parsePort
  , parseCompass
  , parseAttribute
  , parseId
  , parseGraph
  )
  where

import Control.Alternative (empty, (<|>))
import Control.Lazy (defer, fix)
import Control.Monad (class Monad, when)
import Data.Array (cons, many)
import Data.Array (fromFoldable) as A
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.List (List(..), intercalate, some, (:))
import Data.List (fromFoldable) as L
import Data.List (many) as L
import Data.Maybe (Maybe(..), maybe)
import Data.String (codePointFromChar, toLower, uncons)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Language.Dot.Syntax (Attribute(..), AttributeStatementType(..), Compass(..), EdgeType(..), Entity(..), Graph(..), GraphDirectedness(..), GraphStrictness(..), Id(..), NodeId(..), Port(..), Statement(..), Subgraph(..), Xml(..), XmlAttribute(..), XmlAttributeValue(..), XmlName(..))
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (*>), (/=), (<$>), (<*), (<*>), (<>), (==), (>>=), (>>>))
import Text.Parsing.Parser (ParseError, Parser, ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between) as Parser
import Text.Parsing.Parser.Combinators (endBy, lookAhead, manyTill, optionMaybe, optional, sepBy, sepBy1, try, (<?>))
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (class StringLike, anyChar, char, oneOf, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser, unGenLanguageDef)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parsec's "parse" takes a file name for the error code but runParser doesn't
parseDot :: String  -- ^ DOT source code
         -> Either ParseError Graph
parseDot raw =
    runParser (preprocess raw) (dotLexer.whiteSpace *> parseGraph)

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

    lines :: String -> List String
    lines "" = Nil
    lines s = L.fromFoldable $ String.split (String.Pattern "\n") s

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraph :: Parser String Graph
parseGraph = fix $ \p -> 
    ( Graph <$>
          (defer \_ -> parseGraphStrictness)
      <*> (defer \_ -> parseGraphDirectedness)
      <*> optionMaybe (defer \_ -> parseId p)
      <*> (defer \_ -> parseStatementList p)
    )
    <?> "graph"

parseGraphStrictness :: Parser String GraphStrictness
parseGraphStrictness =
    ((dotLexer.reserved "strict" *> pure StrictGraph) <|> pure UnstrictGraph)
    <?> "graph strictness"

parseGraphDirectedness :: Parser String GraphDirectedness
parseGraphDirectedness =
    (   (dotLexer.reserved "graph"   *> pure UndirectedGraph)
    <|> (dotLexer.reserved "digraph" *> pure DirectedGraph)
    )
    <?> "graph directedness"

parseStatementList :: forall a. Parser String a -> Parser String (List Statement)
parseStatementList p = 
  dotLexer.braces ((parseStatement p) `endBy` optional dotLexer.semi)
    <?> "statement list"

parseStatement :: forall a. Parser String a -> Parser String Statement
parseStatement p =
    (   try (defer \_ -> parseEdgeStatement p)
    <|> try (defer \_ -> parseAttributeStatement p)
    <|> try (defer \_ -> parseAssignmentStatement p)
    <|> try (defer \_ -> parseSubgraphStatement p)
    <|>     (defer \_ -> parseNodeStatement p)
    )
    <?> "statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeStatement :: forall a. Parser String a -> Parser String Statement
parseNodeStatement p =
    ( NodeStatement <$>
      (defer \_ -> parseNodeId p) <*> (defer \_ -> parseAttributeList p)
    )
    <?> "node statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEdgeStatement :: forall a. Parser String a -> Parser String Statement
parseEdgeStatement p =
    ( EdgeStatement <$>
      (defer \_ -> parseEntityList p) <*> (defer \_ -> parseAttributeList p)
    )
    <?> "edge statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeStatement :: forall a. Parser String a -> Parser String Statement
parseAttributeStatement p =
    ( AttributeStatement <$>
      (defer \_ -> parseAttributeStatementType) <*> (defer \_ -> parseAttributeList p)
    )
    <?> "attribute statement"

parseAttributeStatementType :: Parser String AttributeStatementType
parseAttributeStatementType =
    (   (dotLexer.reserved "graph" *> pure GraphAttributeStatement)
    <|> (dotLexer.reserved "node"  *> pure NodeAttributeStatement)
    <|> (dotLexer.reserved "edge"  *> pure EdgeAttributeStatement)
    )
    <?> "attribute statement type"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAssignmentStatement :: forall a. Parser String a -> Parser String Statement
parseAssignmentStatement p =
    ( AssignmentStatement <$>
      (defer \_ -> parseId p) <*> (dotLexer.reservedOp "=" *> (defer \_ -> parseId p))
    )
    <?> "assignment statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseSubgraphStatement :: forall a. Parser String a -> Parser String Statement
parseSubgraphStatement p =
    ( SubgraphStatement <$>
       (defer \_ -> parseSubgraph p)
    )
    <?> "subgraph statement"

parseSubgraph :: forall a. Parser String a -> Parser String Subgraph
parseSubgraph p =
    (   try (defer \_ -> parseNewSubgraph p)
    <|>     (defer \_ -> parseSubgraphRef p)
    )
    <?> "subgraph"

parseNewSubgraph :: forall a. Parser String a -> Parser String Subgraph
parseNewSubgraph p =
    ( NewSubgraph <$>
      (optional (dotLexer.reserved "subgraph") *> optionMaybe (defer \_ -> parseId p)) <*> (defer \_ -> parseStatementList p)
    )
    <?> "new subgraph"

parseSubgraphRef :: forall a. Parser String a -> Parser String Subgraph
parseSubgraphRef p =
    ( SubgraphRef <$>
      (dotLexer.reserved "subgraph" *> (defer \_ -> parseId p))
    )
    <?> "subgraph ref"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEntityList :: forall a. Parser String a -> Parser String (List Entity)
parseEntityList p =
    ( (:) <$>
      (defer \_ -> parseEntity p) true <*> some (defer \_ -> parseEntity p false)
    )
    <?> "entity list"

parseEntity :: forall a. Parser String a -> Boolean-> Parser String Entity
parseEntity p first =
    (   try (defer \_ -> parseENodeId p first)
    <|>     (defer \_ -> parseESubgraph p first)
    )
    <?> "entity"

parseENodeId :: forall a. Parser String a -> Boolean-> Parser String Entity
parseENodeId p first =
    ( ENodeId <$>
      (if first then pure NoEdge else defer \_ -> parseEdgeType) <*> (defer \_ -> parseNodeId p)
    )
    <?> "entity node id"

parseESubgraph :: forall a. Parser String a -> Boolean-> Parser String Entity
parseESubgraph p first =
    ( ESubgraph <$>
      (if first then pure NoEdge else defer \_ -> parseEdgeType) <*> (defer \_ -> parseSubgraph p)
    )
    <?> "entity subgraph"

-- parseEdgeType :: Parser EdgeType
parseEdgeType :: Parser String EdgeType
parseEdgeType =
    (   try (dotLexer.reservedOp "->" *> pure DirectedEdge)
    <|>     (dotLexer.reservedOp "--" *> pure UndirectedEdge)
    )
    <?> "edge operator"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeId :: forall a. Parser String a -> Parser String NodeId
parseNodeId p =
    ( NodeId <$>
      (parseId p) <*> optionMaybe (defer \_ -> parsePort p)
    )
    <?> "node id"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePort :: forall a. Parser String a -> Parser String Port
parsePort p =
    (   try parsePortC
    <|>     (defer \_ -> parsePortI p)
    )
    <?> "port"

parsePortC :: Parser String Port
parsePortC =
    ( PortC <$>
      (dotLexer.colon *> defer \_ -> parseCompass)
    )
    <?> "port (compass variant)"

parsePortI :: forall a. Parser String a -> Parser String Port
parsePortI p =
    ( PortI <$>
      (dotLexer.colon *> (defer \_ -> parseId p)) <*> optionMaybe (dotLexer.colon *> defer \_ -> parseCompass)
    )
    <?> "port (id variant)"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseCompass :: Parser String Compass
parseCompass =
    (map (toLower >>> convert) dotLexer.identifier >>= maybe err pure)
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

parseAttributeList :: forall a. Parser String a -> Parser String (List Attribute)
parseAttributeList p =
    (dotLexer.brackets ((defer \_ -> parseAttribute p) `sepBy` optional dotLexer.comma) <|> pure Nil)
    <?> "attribute list"

parseAttribute :: forall a. Parser String a -> Parser String Attribute
parseAttribute p =
    ( do
      id0 <- (defer \_ -> parseId p)
      id1 <- optionMaybe (dotLexer.reservedOp "=" *> (defer \_ -> parseId p))
      pure $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1
    )
    <?> "attribute"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseId :: forall a. Parser String a -> Parser String Id
parseId p = 
    (   try (defer \_ -> parseNameId)
    <|> try (defer \_ -> parseStringId)
    <|> try (defer \_ -> parseFloatId)
    <|> try (defer \_ -> parseIntegerId)
    <|>     (defer \_ -> parseXmlId p)
    )
    <?> "id"

parseNameId :: Parser String Id
parseNameId =
    ( NameId <$>
      dotLexer.identifier
    )
    <?> "name"

parseStringId :: Parser String Id
parseStringId =
    ( StringId <$>
      dotLexer.lexeme 
        (P.between (string "\"") (string "\"") dotLexer.identifier)
    )
    <?> "string literal"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | DOT allows floating point numbers having no whole part like @.123@,
-- | and so does JavaScript's parseFloat which underlies the PureScript readFloat function
-- | which enables the PureScript implementation to simplify this parser
parseFloatId :: Parser String Id
parseFloatId =
  dotLexer.lexeme 
  ( FloatId <$>
    dotLexer.float
  )
  <?> "float"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseIntegerId :: Parser String Id
parseIntegerId =
    ( IntegerId <$>
      dotLexer.integer
    )
    <?> "integer"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseXmlId :: forall a. Parser String a -> Parser String Id
parseXmlId p =
    ( XmlId <$>
      dotLexer.angles (defer \_ -> parseXml p)
    )
    <?> "XML id"

parseXml :: forall a. Parser String a -> Parser String Xml
parseXml p =
    (   try (defer \_ -> parseXmlEmptyTag)
    <|> try (defer \_ -> parseXmlTag p)
    <|>     (defer \_ -> parseXmlText p)
    )
    <?> "XML"

parseXmlEmptyTag :: Parser String Xml
parseXmlEmptyTag =
    ( XmlEmptyTag <$>
      (char '<' *> defer \_ -> parseXmlName) <*> (defer \_ -> parseXmlAttributes <* (char '/' *> char '>'))
    )
    <?> "XML empty tag"

parseXmlTag :: forall a. Parser String a -> Parser String Xml
parseXmlTag p =
    ( do (Tuple name attributes) <- defer \_ -> parseXmlTagOpen
         elements           <- manyTill (defer \_ -> parseXml p) (lookAhead (try (defer \_ -> parseXmlTagClose (Just name))))
         defer \_ -> parseXmlTagClose (Just name)
         pure $ XmlTag name attributes elements
    )
    <?> "XML tag"

parseXmlTagOpen :: Parser String (Tuple XmlName (List XmlAttribute))
parseXmlTagOpen =
    ( Tuple <$>
      (char '<' *> defer \_ -> parseXmlName) <*> (defer \_ -> parseXmlAttributes <* char '>')
    )
    <?> "XML opening tag"

parseXmlTagClose :: Maybe XmlName -> Parser String Unit
parseXmlTagClose mn0 =
    ( do _  <- char '<'
         _  <- char '/'
         n1 <- parseXmlName
         _  <- char '>'
         when (nonMatchingTag n1) empty
    )
    <?> "XML closing tag " <> "(" <> which <> ")"
  where
    nonMatchingTag tag = 
      case mn0 of
        (Just tagname) -> tagname /= tag
        Nothing -> false
    which =
        case mn0 of
          Just (XmlName n) -> "for " <> show n
          Nothing          -> "any"

parseXmlText :: forall a. Parser String a -> Parser String Xml
parseXmlText p =
    ( XmlText <$>
      parseTilTag''
    )
    <?> "XML text"
  where
    parseTilTag'' :: Parser String String
    parseTilTag'' = fromCharArray <$> parseTilTag'
    parseTilTag' :: Parser String (Array Char)
    parseTilTag' = A.fromFoldable <$> parseTilTag
    parseTilTag :: Parser String (List Char)
    parseTilTag = 
      anyChar `manyTill` lookAhead (   try (parseXmlEmptyTag *> pure unit)
                                   <|> try ((defer \_ -> parseXmlTag p)  *> pure unit)
                                   <|>      parseXmlTagClose Nothing
                                   )


parseXmlAttributes :: Parser String (List XmlAttribute)
parseXmlAttributes =
    L.many parseXmlAttribute
    <?> "XML attribute list"

parseXmlAttribute :: Parser String XmlAttribute
parseXmlAttribute =
    ( XmlAttribute <$>
      (parseXmlName <* dotLexer.reservedOp "=") <*> parseXmlAttributeValue
    )
    <?> "XML attribute"

parseXmlAttributeValue :: Parser String XmlAttributeValue
parseXmlAttributeValue =
    ( XmlAttributeValue <$>
      dotLexer.stringLiteral
    )
    <?> "XML attribute value"

parseXmlName :: Parser String XmlName
parseXmlName =
    ( XmlName <$> chars
    )
    <?> "XML name"
  where
    chars :: Parser String String
    chars = fromCharArray <$> (cons <$> c0 <*> (many c1) <* dotLexer.whiteSpace)
    c0 = letter   <|> cs
    c1 = alphaNum <|> cs
    cs = oneOf ['-','.',':','_']

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-----------------------------------------------------------
-- Bracketing
-----------------------------------------------------------
parens :: forall t57 t58 t59. Monad t59 => StringLike t58 => ParserT t58 t59 t57 -> ParserT t58 t59 t57
parens p        = Parser.between (string "(") (string ")") p
braces :: forall t868 t869 t870. Monad t870 => StringLike t869 => ParserT t869 t870 t868 -> ParserT t869 t870 t868
braces p        = Parser.between (string "{") (string "}") p
angles :: forall t879 t880 t881. Monad t881 => StringLike t880 => ParserT t880 t881 t879 -> ParserT t880 t881 t879
angles p        = Parser.between (string "<") (string ">") p
brackets :: forall t857 t858 t859. Monad t859 => StringLike t858 => ParserT t858 t859 t857 -> ParserT t858 t859 t857
brackets p      = Parser.between (string "[") (string "]") p
quoted :: forall t20 t23 t24 t25. Monad t25 => StringLike t24 => t20 -> ParserT t24 t25 t23 -> ParserT t24 t25 t23
quoted p        = Parser.between (string "\"") (string "\"") 

semi :: forall t1 t2. StringLike t2 => Monad t1 => ParserT t2 t1 String
semi            = string ";"
comma :: forall t832 t833. StringLike t833 => Monad t832 => ParserT t833 t832 String
comma           = string ","
dot :: forall t829 t830. StringLike t830 => Monad t829 => ParserT t830 t829 String
dot             = string "."
colon :: forall t851 t852. StringLike t852 => Monad t851 => ParserT t852 t851 String
colon           = string ":"

commaSep :: forall t837 t838 t839. Monad t839 => StringLike t838 => ParserT t838 t839 t837 -> ParserT t838 t839 (List t837)
commaSep p      = sepBy p comma
semiSep :: forall t6 t7 t8. Monad t8 => StringLike t7 => ParserT t7 t8 t6 -> ParserT t7 t8 (List t6)
semiSep p       = sepBy p semi

commaSep1 :: forall t845 t846 t847. Monad t847 => StringLike t846 => ParserT t846 t847 t845 -> ParserT t846 t847 (List t845)
commaSep1 p     = sepBy1 p comma
semiSep1 :: forall t14 t15 t16. Monad t16 => StringLike t15 => ParserT t15 t16 t14 -> ParserT t15 t16 (List t14)
semiSep1 p      = sepBy1 p semi

-- | A lexer for the dot language.
dotLexer :: TokenParser
dotLexer = makeTokenParser dotDef

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
