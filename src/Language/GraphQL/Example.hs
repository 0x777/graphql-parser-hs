module Language.GraphQL.Example where

import Language.GraphQL.Validate.Scalar
import Language.GraphQL.Validate.InputObject
import Language.GraphQL.Validate.Enum
import Language.GraphQL.Validate.Object
import Language.GraphQL.Validate.Context
import qualified Language.GraphQL.Draft.Syntax   as G

import Protolude

-- type Author {
--   id Int!
--   name String!
--   articles [Article!]!
-- }

-- enum Language {
--   EN
--   FR
-- }

-- type Article {
--   id Int!
--   title String!
--   content(language: Language) String
--   author Author!
-- }

-- type QueryRoot {
--   getAuthor(id: Int!) Author
--   getArticle (id: Int!) Article
-- }

data AuthorField
  = AuthorId
  | AuthorName
  | AuthorArticles !ArticleSelectionSet

type AuthorSelectionSet = ObjectSelectionSet (Either TypeName AuthorField)

data Language
  = LanguageEn
  | LanguageFr

data ArticleField
  = ArticleId
  | ArticleName
  | ArticleContent !(Maybe Language)
  | ArticleAuthor !AuthorSelectionSet

type ArticleSelectionSet = ObjectSelectionSet (Either TypeName ArticleField)

data QueryOperation
  = QueryAuthor !Int !AuthorSelectionSet
  | QueryArticle !Int !ArticleSelectionSet

languageType :: EnumDefinition Language
languageType =
  mkEnumDefinition "Language" Nothing $
  [ mkEnumValueDefinition (G.EnumValue "EN") Nothing False LanguageEn
  , mkEnumValueDefinition (G.EnumValue "FR") Nothing False LanguageFr
  ]

authorType :: ObjectDefinition AuthorSelectionSet
authorType =
  mkObjectDefinition "Author" Nothing
  [ objectFieldScalar "id" Nothing noArguments
    (G.toGraphQLType $ G.NamedType "Int") (const AuthorId)
  , objectFieldScalar "name" Nothing noArguments
    (G.toGraphQLType $ G.NamedType "String") (const AuthorName)
  , objectField "articles" Nothing noArguments
    (G.TypeNamed articleType)
    (const AuthorArticles)
  ]

articleType :: ObjectDefinition ArticleSelectionSet
articleType =
  mkObjectDefinition "Article" Nothing
  [ objectFieldScalar "id" Nothing noArguments
    (G.toGraphQLType $ G.NamedType "Int") (const ArticleId)
  , objectFieldScalar "name" Nothing noArguments
    (G.toGraphQLType $ G.NamedType "String") (const ArticleName)
  , objectField "author" Nothing noArguments
    (G.TypeNamed authorType) (const ArticleAuthor)
  ]

