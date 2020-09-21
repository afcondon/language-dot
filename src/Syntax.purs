-- | DOT AST. See <http://www.graphviz.org/doc/info/lang.html>.

module Language.Dot.Syntax where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Graph
  = Graph GraphStrictness GraphDirectedness (Maybe Id) (List Statement)

data GraphStrictness
  = StrictGraph
  | UnstrictGraph
  -- deriving (Enum, Bounded)

data GraphDirectedness
  = DirectedGraph
  | UndirectedGraph
  -- deriving (Enum, Bounded)

data Id
  = NameId    String
  | StringId  String
  | IntegerId Int
  | FloatId   Number
  | XmlId     Xml

data Statement
  = NodeStatement       NodeId (List Attribute)
  | EdgeStatement       (List Entity) (List Attribute)
  | AttributeStatement  AttributeStatementType (List Attribute)
  | AssignmentStatement Id Id
  | SubgraphStatement   Subgraph

data AttributeStatementType
  = GraphAttributeStatement
  | NodeAttributeStatement
  | EdgeAttributeStatement
  -- deriving (Enum, Bounded)

data Attribute
  = AttributeSetTrue  Id
  | AttributeSetValue Id Id

data NodeId
  = NodeId Id (Maybe Port)
    
data Port
  = PortI Id (Maybe Compass)
  | PortC Compass

data Compass
  = CompassN  | CompassE  | CompassS  | CompassW
  | CompassNE | CompassNW | CompassSE | CompassSW

data Subgraph
  = NewSubgraph (Maybe Id) (List Statement)
  | SubgraphRef Id

data Entity
  = ENodeId   EdgeType NodeId
  | ESubgraph EdgeType Subgraph

data EdgeType
  = NoEdge
  | DirectedEdge
  | UndirectedEdge
  -- deriving (Enum, Bounded)

data Xml
  = XmlEmptyTag XmlName (List XmlAttribute)
  | XmlTag      XmlName (List XmlAttribute) (List Xml)
  | XmlText     String

data XmlName
  = XmlName String

data XmlAttribute
  = XmlAttribute XmlName XmlAttributeValue

data XmlAttributeValue
  = XmlAttributeValue String

-- ||         BOILERPLATE DERIVATIONS
derive instance genericNodeId :: Generic NodeId _
derive instance eqNodeId :: Eq NodeId
derive instance ordNodeId :: Ord NodeId
instance showNodeId :: Show NodeId where
  show = genericShow
  
derive instance genericAttribute :: Generic Attribute _
derive instance eqAttribute :: Eq Attribute
derive instance ordAttribute :: Ord Attribute
instance showAttribute :: Show Attribute where
  show = genericShow
  
derive instance genericAttributeStatementType :: Generic AttributeStatementType _
derive instance eqAttributeStatementType :: Eq AttributeStatementType
derive instance ordAttributeStatementType :: Ord AttributeStatementType
instance showAttributeStatementType :: Show AttributeStatementType where
  show = genericShow
  
derive instance genericStatement :: Generic Statement _
derive instance eqStatement :: Eq Statement
derive instance ordStatement :: Ord Statement
instance showStatement :: Show Statement where
  show (NodeStatement _ _)       = "NodeStatement"
  show (EdgeStatement _ _)       = "EdgeStatement"
  show (AttributeStatement _ _)  = "AttributeStatement"
  show (AssignmentStatement _ _) = "AssignmentStatement"
  show (SubgraphStatement _)     = "SubgraphStatement"
  
derive instance genericId :: Generic Id _
derive instance eqId :: Eq Id
derive instance ordId :: Ord Id
instance showId :: Show Id where
  show (NameId i)    =  "NameId " <> show i
  show (IntegerId i) =  "IntegerId " <> show i
  show (FloatId i)   =  "FloatId " <> show i
  show (XmlId i)     =  "XmlId " <> show i
  show (StringId i)  =  "StringId " <> show i
  
derive instance genericGraphDirectedness :: Generic GraphDirectedness _
derive instance eqGraphDirectedness :: Eq GraphDirectedness
derive instance ordGraphDirectedness :: Ord GraphDirectedness
instance showGraphDirectedness :: Show GraphDirectedness where
  show = genericShow
  
derive instance genericGraphStrictness :: Generic GraphStrictness _
derive instance eqGraphStrictness :: Eq GraphStrictness
derive instance ordGraphStrictness :: Ord GraphStrictness
instance showGraphStrictness :: Show GraphStrictness where
  show = genericShow
  
derive instance genericGraph :: Generic Graph _
derive instance eqGraph :: Eq Graph
derive instance ordGraph :: Ord Graph
instance showGraph :: Show Graph where
  show = genericShow
 

derive instance genericXmlName :: Generic XmlName _
derive instance eqXmlName :: Eq XmlName
derive instance ordXmlName :: Ord XmlName
instance showXmlName :: Show XmlName where
  show = genericShow

derive instance genericXmlAttributeValue :: Generic XmlAttributeValue _
derive instance eqXmlAttributeValue :: Eq XmlAttributeValue
derive instance ordXmlAttributeValue :: Ord XmlAttributeValue
instance showXmlAttributeValue :: Show XmlAttributeValue where
  show = genericShow
  
derive instance genericXmlAttribute :: Generic XmlAttribute _
derive instance eqXmlAttribute :: Eq XmlAttribute
derive instance ordXmlAttribute :: Ord XmlAttribute
instance showXmlAttribute :: Show XmlAttribute where
  show = genericShow
  
derive instance genericXml :: Generic Xml _
derive instance eqXml :: Eq Xml
derive instance ordXml :: Ord Xml
instance showXml :: Show Xml where
  show (XmlEmptyTag xmlname attrs) = "XmlEmptyTag " <> show attrs
  show (XmlTag xmlname attrs xmls) ="XmlTag " <> show attrs <> show xmls
  show (XmlText name) ="XmlText " <> name
  
derive instance genericEdgeType :: Generic EdgeType _
derive instance eqEdgeType :: Eq EdgeType
derive instance ordEdgeType :: Ord EdgeType
instance showEdgeType :: Show EdgeType where
  show = genericShow
  
derive instance genericEntity :: Generic Entity _
derive instance eqEntity :: Eq Entity
derive instance ordEntity :: Ord Entity
instance showEntity :: Show Entity where
  show = genericShow
  
derive instance genericSubgraph :: Generic Subgraph _
derive instance eqSubgraph :: Eq Subgraph
derive instance ordSubgraph :: Ord Subgraph
instance showSubgraph :: Show Subgraph where
  show = genericShow
  
derive instance genericCompass :: Generic Compass _
derive instance eqCompass :: Eq Compass
derive instance ordCompass :: Ord Compass
instance showCompass :: Show Compass where
  show = genericShow
  
derive instance genericPort :: Generic Port _
derive instance eqPort :: Eq Port
derive instance ordPort :: Ord Port
instance showPort :: Show Port where
  show = genericShow
