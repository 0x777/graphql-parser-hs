module Language.GraphQL.Test where

import Protolude
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Language.GraphQL.Request as R
import qualified Language.GraphQL.Validate.Parser as P

-- | The type system is as follows
-- type Author {
--   id Int!
--   name String!
--   articles [Article]!
-- }
--
-- type Article {
--   id Int!
--   title String!
--   author Author!
-- }
--
-- type QueryRoot {
--   getAuthor(id: Int!) Author
--   getArticle(id: Int!) Article
-- }

type TableName = Text
type FieldName = Text
type FieldType = Text
type TableArgs = Int
type FieldAlias = Text

type FieldMap a = OMap.InsOrdHashMap FieldAlias a

data TableOp
  = TableOp !TableName !TableArgs !(FieldMap TableField)
  deriving (Show, Eq)

data TableField
  -- name and type
  = TableColumn !FieldName !FieldType
  | TableRelationship !Text !TableOp
  deriving (Show, Eq)

parseQuery :: R.GQLReqParsed -> Either Text (FieldMap TableOp)
parseQuery = undefined
