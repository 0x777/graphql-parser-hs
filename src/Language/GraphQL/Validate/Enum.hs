module Language.GraphQL.Validate.Enum
  ( EnumValueDefinition
  , mkEnumValueDefinition
  , EnumDefinition
  , mkEnumDefinition
  ) where

import           Protolude

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

data EnumValueDefinition a
  = EnumValueDefinition
  { _evdInfo  :: !VT.EnumValInfo
  , _evdValue :: a
  }

mkEnumValueDefinition
  :: G.EnumValue
  -> Maybe G.Description
  -> Bool
  -> a
  -> EnumValueDefinition a
mkEnumValueDefinition name description isDeprecated value =
  EnumValueDefinition info value
  where
    info = VT.EnumValInfo description name isDeprecated

data EnumDefinition a
  = EnumDefinition
  { _edInfo   :: !VT.EnumTyInfo
  , _edValues :: !(Map.HashMap G.EnumValue a)
  }

instance InputTypeDefinition (EnumDefinition a) where
  type instance InputType (EnumDefinition a) = a
  getValueParser = enumValueParser

enumValueParser :: EnumDefinition a -> ValueParser G.NamedType a
enumValueParser definition =
  (typeName, parser)
  where
    typeName = VT._etiName $ _edInfo definition
    parser = \case
      G.VCEnum enumValue ->
        case Map.lookup enumValue $ _edValues definition of
          Just a  -> return a
          Nothing -> throwError "enum value not among the expected values: "
      _ -> throwError "unexpected value when expecting enum value"

mkEnumDefinition
  :: G.Name
  -> Maybe G.Description
  -> [EnumValueDefinition a]
  -> EnumDefinition a
mkEnumDefinition name description values = do
  EnumDefinition typeInfo $ fmap _evdValue valuesMap
  where
    valuesMap = Map.fromList $ flip map values $
                \value -> (VT._eviVal $ _evdInfo value, value)
    typeName = G.NamedType name
    typeInfo = VT.EnumTyInfo description typeName $ fmap _evdInfo valuesMap

instance ResponseType (EnumDefinition s) where
  type instance SelectionSetType (EnumDefinition s) = ()
  getTypeName = VT._etiName . _edInfo
  getSelectionSetParser _ = \case
    [] -> return ()
    _ -> throwError "A selection set is not expected for an enum type"

