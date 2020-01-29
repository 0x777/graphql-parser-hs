{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Description: The GraphQL AST
module Language.GraphQL.Draft.Syntax
  ( Name(..)
  , isValidName
  , Document(..)
  , ExecutableDocument(..)
  , SchemaDocument(..)
  , Definition(..)
  , ExecutableDefinition(..)
  , partitionExDefs
  , OperationDefinition(..)
  , OperationType(..)
  , OperationName(..)
  , TypedOperationDefinition(..)
  , TypeSystemDefinition(..)
  , SchemaDefinition(..)
  , RootOperationTypeDefinition(..)
  , VariableDefinition(..)
  , Variable(..)
  , SelectionSet
  , Selection(..)
  , Field(..)
  , Alias(..)
  , Argument(..)
  , FragmentSpread(..)
  , InlineFragment(..)
  , FragmentDefinition(..)
  , TypeCondition
  , ValueConst(..)
  , Value(..)
  , StringValue(..)
  , ListValueG(..)
  , ListValue
  , ListValueC
  , ObjectValueG(..)
  , ObjectValue
  , ObjectValueC
  , ObjectFieldG(..)
  , ObjectField
  , ObjectFieldC
  , DefaultValue
  , Directive(..)
  , TypeStructure(..)
  , GType
  , ToGType(..)
  , getBaseType
  , showGT
  , showLT
  , isNullable
  , isNotNull
  , isListType
  , showNT
  , NamedType(..)
  , NonNullType(..)
  , ToNonNullType(..)
  , ListType(..)
  , toListType
  , Description(..)
  , TypeDefinition(..)
  , ObjectTypeDefinition(..)
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , EnumValue(..)
  , InputObjectTypeDefinition(..)
  , DirectiveDefinition(..)
  , DirectiveLocation(..)
  , ExecutableDirectiveLocation(..)
  , TypeSystemDirectiveLocation(..)
  ) where

import           Control.Monad.Fail         (fail)
import           Data.Bool                  (not)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import           Protolude

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Text.Regex.TDFA            as TDFA

-- * Documents

-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document

newtype Name
  = Name { unName :: Text }
  deriving ( Eq, Ord, Show, Hashable, IsString, Lift, Semigroup
           , Monoid, J.ToJSONKey, J.ToJSON)

-- Ref: http://facebook.github.io/graphql/June2018/#sec-Names
isValidName :: Name -> Bool
isValidName (Name text) =
  TDFA.match compiledRegex $ T.unpack text
  where
    compiledRegex = TDFA.makeRegex ("^[_a-zA-Z][_a-zA-Z0-9]*$" :: BL.ByteString) :: TDFA.Regex

parseName :: Text -> J.Parser Name
parseName text =
  bool (fail $ T.unpack errorMessage) (pure name) $ isValidName name
  where
    name = Name text
    errorMessage = text <> " is not valid GraphQL name"

instance J.FromJSON Name where
  parseJSON = J.withText "Text" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

newtype Document
  = Document { getDefinitions :: [Definition] }
  deriving (Ord, Show, Eq, Lift)

data Definition
  = DefinitionExecutable !ExecutableDefinition
  | DefinitionTypeSystem !TypeSystemDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Definition

newtype ExecutableDocument
  = ExecutableDocument { getExecutableDefinitions :: [ExecutableDefinition] }
  deriving (Ord, Show, Eq, Lift, Hashable)

data ExecutableDefinition
  = ExecutableDefinitionOperation !OperationDefinition
  | ExecutableDefinitionFragment !FragmentDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDefinition

partitionExDefs
  :: [ExecutableDefinition]
  -> ([TypedOperationDefinition], [FragmentDefinition])
partitionExDefs =
  foldr f ([], [])
  where
    f d (ops, frags) = case d of
      ExecutableDefinitionOperation (OperationDefinitionUnTyped t) ->
        let operation = TypedOperationDefinition
                        OperationTypeQuery Nothing [] [] t
        in (operation:ops, frags)
      ExecutableDefinitionOperation (OperationDefinitionTyped t) ->
        (t:ops, frags)
      ExecutableDefinitionFragment frag ->
        (ops, frag:frags)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema !SchemaDefinition
  | TypeSystemDefinitionType !TypeDefinition
  -- TypeSystemDefinitionDir !DirectiveDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDefinition

data SchemaDefinition
  = SchemaDefinition
  { _sdDirectives                   :: !(Maybe [Directive])
  , _sdRootOperationTypeDefinitions :: ![RootOperationTypeDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable SchemaDefinition

data RootOperationTypeDefinition
  = RootOperationTypeDefinition
  { _rotdOperationType     :: !OperationType
  , _rotdOperationTypeType :: !NamedType
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable RootOperationTypeDefinition

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable OperationType

newtype SchemaDocument
  = SchemaDocument [TypeDefinition]
  deriving (Ord, Show, Eq, Lift, Hashable)

data OperationDefinition
  = OperationDefinitionTyped !TypedOperationDefinition
  | OperationDefinitionUnTyped !SelectionSet
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable OperationDefinition

newtype OperationName
  = OperationName { _unOperationName :: Name }
  deriving (Ord, Show, Eq, Lift, Generic, Hashable, J.ToJSON, J.FromJSON)

data TypedOperationDefinition
  = TypedOperationDefinition
  { _todType                :: !OperationType
  , _todName                :: !(Maybe OperationName)
  , _todVariableDefinitions :: ![VariableDefinition]
  , _todDirectives          :: ![Directive]
  , _todSelectionSet        :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypedOperationDefinition

data VariableDefinition
  = VariableDefinition
  { _vdVariable     :: !Variable
  , _vdType         :: !GType
  , _vdDefaultValue :: !(Maybe DefaultValue)
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable VariableDefinition

newtype Variable
  = Variable { unVariable :: Name }
  deriving ( Eq, Ord, Show, Hashable, Lift, J.ToJSONKey, J.FromJSONKey
           , J.ToJSON, J.FromJSON)

type SelectionSet = [Selection]

data Selection
  = SelectionField !Field
  | SelectionFragmentSpread !FragmentSpread
  | SelectionInlineFragment !InlineFragment
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Selection

data Field
  = Field
  { _fAlias        :: !(Maybe Alias)
  , _fName         :: !Name
  , _fArguments    :: ![Argument]
  , _fDirectives   :: ![Directive]
  , _fSelectionSet :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Field

newtype Alias
  = Alias { unAlias :: Name }
  deriving (Ord, Show, Eq, Hashable, Lift, J.ToJSON, J.FromJSON)

data Argument
  = Argument
  { _aName  :: !Name
  , _aValue :: !Value
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Argument

-- * Fragments

data FragmentSpread
  = FragmentSpread
  { _fsName       :: !Name
  , _fsDirectives :: ![Directive]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FragmentSpread

data InlineFragment
  = InlineFragment
  { _ifTypeCondition :: !(Maybe TypeCondition)
  , _ifDirectives    :: ![Directive]
  , _ifSelectionSet  :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InlineFragment

data FragmentDefinition
  = FragmentDefinition
  { _fdName          :: !Name
  , _fdTypeCondition :: !TypeCondition
  , _fdDirectives    :: ![Directive]
  , _fdSelectionSet  :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FragmentDefinition

type TypeCondition = NamedType

-- * Values

-- data ValueLeaf
--   = VLInt !Int32
--   | VLFloat !Double
--   | VLBoolean !Bool
--   | VLString !StringValue
--   | VLEnum !EnumValue
--   | VLNull
--   deriving (Ord, Show, Eq, Lift)

-- data ValueConst
--   = VCLeaf !ValueLeaf
--   | VCList !ListValueC
--   | VCObject !ObjectValueC
--   deriving (Ord, Show, Eq, Lift)

-- data Value
--   = VVariable !Variable
--   | VLeaf !ValueLeaf
--   | VList !ListValue
--   | VObject !ObjectValue
--   deriving (Ord, Show, Eq, Lift)

data ValueConst
  = VCInt !Int32
  | VCFloat !Double
  | VCString !StringValue
  | VCBoolean !Bool
  | VCNull
  | VCEnum !EnumValue
  | VCList !ListValueC
  | VCObject !ObjectValueC
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ValueConst

data Value
  = VVariable !Variable
  | VInt !Int32
  | VFloat !Double
  | VString !StringValue
  | VBoolean !Bool
  | VNull
  | VEnum !EnumValue
  | VList !ListValue
  | VObject !ObjectValue
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Value

newtype StringValue
  = StringValue { unStringValue :: Text }
  deriving (Ord, Show, Eq, Lift, Hashable)

newtype ListValueG a
  = ListValueG {unListValue :: [a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ListValue = ListValueG Value

type ListValueC = ListValueG ValueConst

newtype ObjectValueG a
  = ObjectValueG {unObjectValue :: [ObjectFieldG a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ObjectValue = ObjectValueG Value

type ObjectValueC = ObjectValueG ValueConst

data ObjectFieldG a
  = ObjectFieldG
  { _ofName  :: Name
  , _ofValue :: a
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (ObjectFieldG a)

type ObjectField = ObjectFieldG Value
type ObjectFieldC = ObjectFieldG ValueConst

type DefaultValue = ValueConst

-- * Directives

data Directive
  = Directive
  { _dName      :: !Name
  , _dArguments :: ![Argument]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Directive

data NonNullType a
  = NonNullList !(ListType a)
  | NonNullNamed !a
  deriving (Show, Ord, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance ToGType (NonNullType NamedType) where
  toGraphQLType = TypeNonNull

class ToNonNullType t where
  toNonNullType :: t -> NonNullType NamedType

instance (Hashable a) => Hashable (NonNullType a)

newtype ListType a
  = ListType { unListType :: TypeStructure a }
  deriving (Show, Ord, Eq, Lift, Generic, Hashable, Functor, Foldable, Traversable)

instance ToNonNullType (ListType NamedType) where
  toNonNullType =  NonNullList

instance ToGType (ListType NamedType) where
  toGraphQLType = TypeList

toListType :: (ToGType t) => t -> ListType NamedType
toListType = ListType . toGraphQLType

data TypeStructure a
  = TypeNamed !a
  | TypeList !(ListType a)
  | TypeNonNull !(NonNullType a)
  deriving (Show, Ord, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable a) => Hashable (TypeStructure a)

type GType = TypeStructure NamedType

class ToGType t where
  toGraphQLType :: t -> GType

instance ToGType GType where
  toGraphQLType t = t

getBaseType :: TypeStructure a -> a
getBaseType = \case
  TypeNamed l -> l
  TypeList (ListType ty) -> getBaseType ty
  TypeNonNull nonNullType -> case nonNullType of
    NonNullList (ListType ty) -> getBaseType ty
    NonNullNamed l -> l

instance J.ToJSON GType where
  toJSON = J.toJSON . showGT

showGT :: GType -> Text
showGT = \case
  TypeNamed namedType -> showNT namedType
  TypeList listType -> showLT listType
  TypeNonNull nonNullType -> (<> "!") $ case nonNullType of
    NonNullList listType -> showLT listType
    NonNullNamed namedType -> showNT namedType

showNT :: NamedType -> Text
showNT = unName . unNamedType

showLT :: ListType NamedType -> Text
showLT lt = "[" <> showGT (unListType lt) <> "]"

isNullable :: GType -> Bool
isNullable = \case
  TypeNamed _   -> False
  TypeList _    -> False
  TypeNonNull _ -> True

isListType :: GType -> Bool
isListType = \case
  TypeNamed _   -> False
  TypeList _    -> True
  TypeNonNull _ -> False

isNotNull :: GType -> Bool
isNotNull = not . isNullable

newtype NamedType
  = NamedType { unNamedType :: Name }
  deriving (Eq, Ord, Show, Hashable, Lift, J.ToJSON,
            J.ToJSONKey, J.FromJSON, J.FromJSONKey)

instance ToNonNullType NamedType where
  toNonNullType = NonNullNamed

instance ToGType NamedType where
  toGraphQLType = TypeNamed

-- * Type definition

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeDefinition

newtype Description
  = Description { unDescription :: Text }
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid, Hashable,
            J.ToJSON, J.FromJSON)

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdDescription          :: !(Maybe Description)
  , _otdName                 :: !Name
  , _otdImplementsInterfaces :: ![NamedType]
  , _otdDirectives           :: ![Directive]
  , _otdFieldsDefinition     :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ObjectTypeDefinition

data FieldDefinition
  = FieldDefinition
  { _fldDescription         :: !(Maybe Description)
  , _fldName                :: !Name
  , _fldArgumentsDefinition :: !ArgumentsDefinition
  , _fldType                :: !GType
  , _fldDirectives          :: ![Directive]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FieldDefinition

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition
  = InputValueDefinition
  { _ivdDescription  :: !(Maybe Description)
  , _ivdName         :: !Name
  , _ivdType         :: !GType
  , _ivdDefaultValue :: !(Maybe DefaultValue)
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputValueDefinition

data InterfaceTypeDefinition
  = InterfaceTypeDefinition
  { _itdDescription      :: !(Maybe Description)
  , _itdName             :: !Name
  , _itdDirectives       :: ![Directive]
  , _itdFieldsDefinition :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InterfaceTypeDefinition

data UnionTypeDefinition
  = UnionTypeDefinition
  { _utdDescription :: !(Maybe Description)
  , _utdName        :: !Name
  , _utdDirectives  :: ![Directive]
  , _utdMemberTypes :: ![NamedType]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable UnionTypeDefinition

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdDescription :: !(Maybe Description)
  , _stdName        :: !Name
  , _stdDirectives  :: ![Directive]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ScalarTypeDefinition

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdDescription      :: !(Maybe Description)
  , _etdName             :: !Name
  , _etdDirectives       :: ![Directive]
  , _etdValueDefinitions :: ![EnumValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumTypeDefinition

data EnumValueDefinition
  = EnumValueDefinition
  { _evdDescription :: !(Maybe Description)
  , _evdName        :: !EnumValue
  , _evdDirectives  :: ![Directive]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumValueDefinition

newtype EnumValue
  = EnumValue { unEnumValue :: Name }
  deriving (Show, Eq, Lift, Hashable, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdDescription      :: !(Maybe Description)
  , _iotdName             :: !Name
  , _iotdDirectives       :: ![Directive]
  , _iotdValueDefinitions :: ![InputValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputObjectTypeDefinition

data DirectiveDefinition
  = DirectiveDefinition
  { _ddDescription :: !(Maybe Description)
  , _ddName        :: !Name
  , _ddArguments   :: !ArgumentsDefinition
  , _ddLocations   :: ![DirectiveLocation]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveDefinition

data DirectiveLocation
  = DLExecutable !ExecutableDirectiveLocation
  | DLTypeSystem !TypeSystemDirectiveLocation
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveLocation

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDirectiveLocation

data TypeSystemDirectiveLocation
  = TSDLSCHEMA
  | TSDLSCALAR
  | TSDLOBJECT
  | TSDLFIELD_DEFINITION
  | TSDLARGUMENT_DEFINITION
  | TSDLINTERFACE
  | TSDLUNION
  | TSDLENUM
  | TSDLENUM_VALUE
  | TSDLINPUT_OBJECT
  | TSDLINPUT_FIELD_DEFINITION
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDirectiveLocation
