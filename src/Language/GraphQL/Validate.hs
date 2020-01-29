module Language.GraphQL.Validate where

import           Protolude

import           Control.Monad.Validate

import qualified Data.HashMap.Strict           as Map
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.GraphQL.Request
import           Language.GraphQL.Validate.Object
import           Language.GraphQL.Validate.Context

onNothing :: (Monad m) => Maybe a -> m a -> m a
onNothing m act = maybe act return m

showName :: G.Name -> Text
showName name = "\"" <> G.unName name <> "\""

groupListWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k)
  -> t v
  -> Map.HashMap k (NE.NonEmpty v)
groupListWith f l = groupTuples $ fmap (\v -> (f v, v)) l

groupTuples
  :: (Eq k, Hashable k, Foldable t) => t (k, v) -> Map.HashMap k (NE.NonEmpty v)
groupTuples = foldr groupFlds Map.empty
 where
  groupFlds (k, v) m = case Map.lookup k m of
    Nothing -> Map.insert k (v NE.:| []) m
    Just s  -> Map.insert k (v NE.<| s) m

-- either duplicate keys or the map
mkMapWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k)
  -> t v
  -> Either (NE.NonEmpty k) (Map.HashMap k v)
mkMapWith f l = case NE.nonEmpty dups of
  Just dupsNE -> Left dupsNE
  Nothing     -> Right $ Map.map NE.head mapG
 where
  mapG = groupListWith f l
  dups = Map.keys $ Map.filter ((> 1) . length) mapG

data ValidationError
  -- | more than one operations have the same name
  = OperationNameNotUnique !G.OperationName
  -- | anonymous operation must be the only defined operation
  | LoneAnonymousOperationError
  -- | the given operaton is not found in the list of operatons
  | UnknownOperation !G.OperationName
  -- | only named operatons, so operation name is required
  | OperationNameRequired
  deriving (Show, Eq)

data ValidatedOperation
  = OperationLone !G.SelectionSet
  | Operation

-- | Get the operation that needs to be executed
--   from the given GraphQL document
getOperation
  :: (MonadValidate [ValidationError] m)
  -- | An optional operation name
  => Maybe G.OperationName
  -- | List of operations
  -> [G.TypedOperationDefinition]
  -- | The operation that needs to be executed
  -> m G.TypedOperationDefinition
getOperation operationNameM operations = do

  -- 5.2
  anonymousOperationOrOperationMap <- do

    -- 5.2.1.1 validate that all named operations are unique
    operationMap <-
      either (refute . toList . fmap OperationNameNotUnique) return
        $ mkMapWith fst namedOperations

    -- 5.2.2.1 only one anoymous operation has to be present
    case anonymousOperations of
      []                   -> pure $ Right operationMap
      [anonymousOperation] -> if null operationMap
        -- if the anonymous operation is the only operation
        then pure $ Left anonymousOperation
        -- anonymous operation is defined when there are named operations
        else refute $ pure LoneAnonymousOperationError
      -- more than one anonymous operation is present
      _ -> refute $ pure LoneAnonymousOperationError

  -- The spec doesn't talk about validating a GraphQL request;
  -- so this mimics the behaviour of the js library
  case (operationNameM, anonymousOperationOrOperationMap) of

    -- Lone anonymous operation
    (Nothing, Left anonymousOperation) -> pure anonymousOperation

    -- Named operations and an operation name is specified
    (Just operationName, Right operationMap) ->
      fmap snd $
        onNothing (Map.lookup operationName operationMap) $
        refute $ pure $ UnknownOperation operationName

    -- Operation name is specified where there is only
    -- an anomyous operation
    (Just operationName, Left _) ->
      refute $ pure $ UnknownOperation operationName

    -- There are only named operations, operation name is required
    (Nothing, Right _) -> refute $ pure OperationNameRequired

 where
  anonymousOperations :: [G.TypedOperationDefinition]
  namedOperations :: [(G.OperationName, G.TypedOperationDefinition)]
  (anonymousOperations, namedOperations) =
    partitionEithers $ flip map operations $ \operation ->
      case G._todName operation of
        Just operationName -> Right (operationName, operation)
        Nothing            -> Left operation


data QueryRootField f
  = QRFTypeName
  | QRFGetType
  | QRFGetSchema
  | QRFCustom f
  deriving (Show, Eq)

newtype QueryDefinition q
  = QueryDefinition
  { _unQueryDefinition :: ObjectDefinition q
  }

mkQueryDefinition
  :: [ObjectFieldDefinition q] -> SchemaBuilder (QueryDefinition q)
mkQueryDefinition =
  undefined

type MutationRootField = Either TypeName

newtype MutationDefinition m
  = MutationDefinition
  { _unMutationDefinition :: ObjectDefinition m
  }

data SubscriptionDefinition s
  = SubscriptionDefinition

data SchemaDefinition q m s
  = SchemaDefinition
  { _sdQuery        :: !(QueryDefinition q)
  , _sdMutation     :: !(MutationDefinition m)
  , _sdSubscription :: !(SubscriptionDefinition s)
  }

mkSchemaDefinition
  :: [ObjectFieldDefinition q]
  -> [ObjectFieldDefinition m]
  -> [ObjectFieldDefinition s]
  -> SchemaBuilder (SchemaDefinition q m s)
mkSchemaDefinition = undefined

data ParsedOperation q m s
  = OperationQuery !(ObjectSelectionSet (QueryRootField q))
  | OperationMutation !(ObjectSelectionSet (MutationRootField q))
  | OperationSubscription !s
