module Language.GraphQL.Validate.Context
  -- ( SchemaBuilder

  ( InputTypeDefinition(..)

  , ValueParser
  , listParser
  , nonNullableParser

  , SelectionSetParser
  , ResponseType(..)

  ) where

import           Protolude

import qualified Language.GraphQL.Draft.Syntax   as G
import qualified Language.GraphQL.Validate.Types as VT

-- type SchemaBuilder a = State VT.TypeMap a

type ValueParser t a = (t, G.ValueConst -> Either Text a)

class InputTypeDefinition i where
  type InputType i
  getValueParser :: i -> ValueParser G.NamedType (InputType i)

listParser
  :: (G.ToGType t)
  => ValueParser t a
  -> ValueParser (G.ListType G.NamedType) (Maybe [a])
listParser (currentType, parser) =
  (G.toListType currentType, newParser)
  where
    newParser = \case
      G.VCList listValue ->
        fmap Just $ traverse parser $ G.unListValue listValue
      G.VCNull -> return Nothing
      -- only if it is not a list and not null, the
      -- input should be coerced as a single element list
      value -> Just . pure <$> parser value

nonNullableParser
  :: (G.ToNonNullType t)
  => ValueParser t (Maybe a)
  -> ValueParser (G.NonNullType G.NamedType) a
nonNullableParser (currentType, parser) =
  (nonNullableType, newParser)
  where
    nonNullableType = G.toNonNullType currentType
    newParser value = do
      resultM <- parser value
      case resultM of
        Just result -> return result
        -- TODO: better error message
        Nothing     -> throwError "unexpected null for type: "

type SelectionSetParser s = G.SelectionSet -> Either Text s

class ResponseType r where
  type SelectionSetType r
  getTypeName :: r -> G.NamedType
  getSelectionSetParser :: r -> SelectionSetParser (SelectionSetType r)
