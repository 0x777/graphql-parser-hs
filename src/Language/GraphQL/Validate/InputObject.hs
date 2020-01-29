module Language.GraphQL.Validate.InputObject
  ( InputObjectDefinition
  , mkInputObjectDefinition

  , InputObjectBuilder
  , inputObjectField
  , concatInputObjectBuilder

  ) where

import           Protolude

import qualified Data.List as L
import qualified Data.HashMap.Strict             as Map
import qualified Language.GraphQL.Draft.Syntax   as G
import qualified Language.GraphQL.Validate.Types as VT
import Language.GraphQL.Validate.Context

-- input type Order {
--   addressId Int!
--   items [OrderItem!]!
-- }
-- input type OrderItem {
--   productId Int!
--   quantity Int!
-- }
-- type OrderDetail {
--   value Float!
--   status String!
-- }
-- placeOrder(order: Order!) OrderStatus
--
-- data Order = Order { addressId :: Int!, items :: [OrderItem]! }
-- data OrderItem = OrderItem { value :: Float!, quantity :: Int! }
-- data PlaceOrder = PlaceOrder !Order
-- data Mutation = MutationPlaceOrder !PlaceOrder
--
-- type SchemaBuilder = State TypeMap a

-- data InputObjectDefinition a = (Info, ValueParser a)
--
--
-- type ScalarParser a = ScalarValue -> Either Text a
--
-- mkOrderItemDefinition :: SchemaBuilder (InputObjectDefinition OrderItem)
-- mkOrderItemDefinition =
--   mkInputObjectDefinition "OrderItem" "an item in an order" $
--   OrderItem <$>
--   field "productId" "the id of the product" intParser <*>
--   field "quantity" "quantity of the product" intParser
--
--
-- data InputValueNonNullable
--   = IVCInt !Int32
--   | IVCFloat !Double
--   | IVCString !G.StringValue
--   | IVCBoolean !Bool
--   | IVCNull
--   | IVCEnum !G.EnumValue
--   | IVCList !(G.ListValueG InputValue)
--   | IVCObject !(G.ObjectValueG InputValue)
--   deriving (Ord, Show, Eq, Lift, Generic)

-- data InputValue
--   = InputValueNonNull !InputValueNonNullable
--   | InputValueNull
--   deriving (Ord, Show, Eq, Lift, Generic)
--

data InputObjectDefinition a
  = InputObjectDefinition
  { _iodInfo   :: !VT.InpObjTyInfo
  , _iodParser :: !(InputObjectParser a)
  }

instance InputTypeDefinition (InputObjectDefinition a) where
  type instance InputType (InputObjectDefinition a) = a
  getValueParser = inputObjectValueParser

inputObjectValueParser
  :: InputObjectDefinition a
  -> ValueParser G.NamedType a
inputObjectValueParser definition =
  (typeName, parser)
  where
    typeName = VT._iotiName $ _iodInfo definition
    parser = \case
      G.VCObject object -> runReaderT (_iodParser definition) object
      -- TODO: better error name
      _                 -> throwError "expecting an object for the type: "

mkInputObjectDefinition
  :: G.Name
  -> Maybe G.Description
  -> InputObjectBuilder a
  -> InputObjectDefinition a
mkInputObjectDefinition name description builder = do
  -- modify (Map.insert typeName $ VT.TIInpObj typeInfo)
  -- TODO: how do we make sure that the type info is added to the state?
  -- maybe some reference from typemap should be used in the type?
  InputObjectDefinition typeInfo parser
  where
    typeName = G.NamedType name
    InputObjectBuilder fieldInfoMap parser = builder
    typeInfo = VT.InpObjTyInfo description typeName fieldInfoMap

type InputObjectParser
  = ReaderT G.ObjectValueC (Either Text)

-- While currently it is possible to define an applicative instance for this
-- type, it wouldn't make sense in the future if we have InpObjFldMap as a
-- NonEmptyMap
data InputObjectBuilder a
  = InputObjectBuilder
  { _iobFieldMap     :: !VT.InpObjFldMap
  , _iobObjectParser :: !(InputObjectParser a)
  }

-- Note: we do not have any other means to combine InputObjectBuilders
concatInputObjectBuilder
  :: InputObjectBuilder a
  -> InputObjectBuilder b
  -> InputObjectBuilder (a, b)
concatInputObjectBuilder (InputObjectBuilder m1 p1) (InputObjectBuilder m2 p2) =
  InputObjectBuilder (m1 <> m2) ((,) <$> p1 <*> p2)

inputObjectField
  :: (G.ToGType t)
  => G.Name
  -> Maybe G.Description
  -> ValueParser t a
  -> InputObjectBuilder a
inputObjectField name description (fieldType, fieldParser) = do
  InputObjectBuilder (Map.singleton name fieldInfo) parser
  where
    fieldInfo = VT.InpValInfo description name Nothing $
                G.toGraphQLType fieldType
    parser = do
      fields <- asks G.unObjectValue
      -- Currently, we treat the absence of a field the same as
      -- the field being set to null
      let fieldValue = maybe G.VCNull G._ofValue $
                       L.find (\field -> G._ofName field == name) fields
      either throwError return $ fieldParser fieldValue

