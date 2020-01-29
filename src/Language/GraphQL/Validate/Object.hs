module Language.GraphQL.Validate.Object
  ( ObjectDefinition
  , mkObjectDefinition
  , TypeName
  , ObjectFieldDefinition
  , objectField
  , objectFieldScalar
  , ObjectSelectionSet
  , ArgumentsDefinition
  , noArguments
  ) where

import           Protolude

import qualified Data.List as L
import qualified Data.HashMap.Strict             as Map
import qualified Language.GraphQL.Draft.Syntax   as G
import qualified Language.GraphQL.Validate.Types as VT
import Language.GraphQL.Validate.Context
import qualified Data.Sequence.NonEmpty     as NE
import qualified Data.HashMap.Strict.InsOrd as OMap

data ArgumentsDefinition a
  = ArgumentsDefinition
  { _adInfo :: !(Map.HashMap G.Name VT.InpValInfo)
  , _adParser :: !([G.Argument] -> Either Text a)
  }

noArguments :: ArgumentsDefinition ()
noArguments =
  ArgumentsDefinition mempty $ \case
    [] -> return ()
    _ -> throwError "unexpected arguments"

-- groupTuples
--   :: (Eq k, Hashable k, Foldable t)
--   => t (k, v) -> OMap.InsOrdHashMap k (NE.NESeq v)
-- groupTuples =
--   undefined
  -- L.foldl' groupFlds OMap.empty
  -- where
  --   groupFlds m (k, v) =
  --     OMap.insertWith (\_ c -> c NE.|> v) k (NE.init v) m

-- groupListWith
--   :: (Eq k, Hashable k, Foldable t, Functor t)
--   => (v -> k) -> t v -> OMap.InsOrdHashMap k (NE.NESeq v)
-- groupListWith f l =
--   groupTuples $ fmap (\v -> (f v, v)) l

type ObjectFieldParser f = G.Field -> Either Text f
type ObjectFieldParsers f = Map.HashMap G.Name (ObjectFieldParser f)

data ObjectFieldDefinition f
  = ObjectFieldDefinition
  { _ofdInfo   :: !VT.ObjFldInfo
  , _ofdParser :: !(ObjectFieldParser f)
  }

objectField
  :: (ResponseType r)
  => G.Name
  -> Maybe G.Description
  -> ArgumentsDefinition a
  -> G.TypeStructure r
  -> (a -> SelectionSetType r -> f)
  -> ObjectFieldDefinition f
objectField name description argumentsDefinition fieldType fieldConstructor =
  ObjectFieldDefinition fieldInfo fieldParser
  where
    fieldInfo =
      VT.ObjFldInfo description name (_adInfo argumentsDefinition) $
      fmap getTypeName fieldType
    fieldParser field =
      fieldConstructor <$>
      (_adParser argumentsDefinition $ G._fArguments field) <*>
      (getSelectionSetParser (G.getBaseType fieldType) $ G._fSelectionSet field)

objectFieldScalar
  :: G.Name
  -> Maybe G.Description
  -> ArgumentsDefinition a
  -> G.GType
  -> (a -> f)
  -> ObjectFieldDefinition f
objectFieldScalar = undefined

-- data FieldGroup
--   = FGSingle !G.Field
--   | FGSelection !GroupedFields

-- type GroupedFields = OMap.InsOrdHashMap G.Alias (NE.NESeq G.Field)

-- flattenObjectSelectionSet
--   :: [G.Name]
--   -> G.SelectionSet
--   -> Either Text GroupedFields
-- flattenObjectSelectionSet visitedFragments selectionSet =
--   undefined
--   where
--     expandField = \case
--       G.SelectionField field -> return $ FGSingle field
--       G.SelectionInlineFragment inlineFragment ->
--         -- TODO: do a validaiton check
--         fmap FGSelection $ flattenObjectSelectionSet visitedFragments $
--           G._ifSelectionSet inlineFragment
--       G.SelectionFragmentSpread fragmentSpread ->
--         undefined

data TypeName

type ObjectSelectionSet = OMap.InsOrdHashMap G.Alias

objectSelectionSetParser
  :: G.NamedType
  -> ObjectFieldParsers f
  -> SelectionSetParser (ObjectSelectionSet f)
objectSelectionSetParser objectType parsers selectionSet = do
  undefined

data ObjectDefinition s
  = ObjectDefinition
  { _otdInfo         :: !VT.ObjTyInfo
  , _otdFieldParsers :: !(SelectionSetParser s)
  }

instance ResponseType (ObjectDefinition s) where
  type instance SelectionSetType (ObjectDefinition s) = s
  getTypeName = VT._otiName . _otdInfo
  getSelectionSetParser = _otdFieldParsers

mkObjectDefinition
  :: G.Name
  -> Maybe G.Description
  -> [ObjectFieldDefinition f]
  -> ObjectDefinition (ObjectSelectionSet (Either TypeName f))
mkObjectDefinition = undefined
