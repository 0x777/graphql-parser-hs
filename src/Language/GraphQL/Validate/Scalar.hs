module Language.GraphQL.Validate.Scalar
  ( ScalarDefinition
  , mkScalarDefinition
  ) where

import           Protolude

import qualified Language.GraphQL.Draft.Syntax   as G
import qualified Language.GraphQL.Validate.Types as VT
import Language.GraphQL.Validate.Context

data ScalarDefinition a
  = ScalarDefinition
  { _sdInfo   :: !VT.ScalarTyInfo
  , _sdParser :: !(ValueParser G.NamedType a)
  }

instance InputTypeDefinition (ScalarDefinition a) where
  type instance InputType (ScalarDefinition a) = a
  getValueParser = _sdParser

mkScalarDefinition
  :: Maybe G.Description
  -> ValueParser G.NamedType a
  -> (ScalarDefinition a)
mkScalarDefinition description parser = do
  -- modify (Map.insert typeName $ VT.TIScalar typeInfo)
  ScalarDefinition typeInfo parser
  where
    typeName = fst parser
    typeInfo = VT.ScalarTyInfo description typeName

instance ResponseType (ScalarDefinition s) where
  type instance SelectionSetType (ScalarDefinition s) = ()
  getTypeName = VT._stiType . _sdInfo
  getSelectionSetParser _ = \case
    [] -> return ()
    _ -> throwError "A selection set is not expected for a scalar type"

