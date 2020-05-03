module Frontend.UI.FormWidget
  ( HasInitialValue(..)
  , MakeConfig(..)
  , FormWidgetConfig
  , FormWidgetConfig'
  , _formWidgetConfig_initialValue
  , _formWidgetConfig_setValue
  , formWidgetConfig_initialValue
  , formWidgetConfig_setValue
  , iec2fwc
  , fwc2iec
  , PrimFormWidgetConfig
  , PrimFormWidgetConfig'
  , _primFormWidgetConfig_fwc
  , _primFormWidgetConfig_initialAttributes
  , _primFormWidgetConfig_modifyAttributes
  , primFormWidgetConfig_fwc
  , primFormWidgetConfig_initialAttributes
  , primFormWidgetConfig_modifyAttributes
  , iec2pfwc
  , pfwc2iec
  , FormWidget(..)
  , formWidget_value
  , formWidget_input
  , formWidget_hasFocus
  , ie2iw
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

class HasInitialValue a where
  type InitialValue a :: *
  _initialValue :: a -> (InitialValue a)
  initialValue :: Lens' a (InitialValue a)

class MakeConfig cfg a | cfg -> a where
  mkCfg :: a -> cfg

-- A dot-separated namespace
--newtype WidgetName = WidgetName { unWidgetName :: Text }
--  deriving (Eq, Ord, IsString)

-- | Describes form config information where the value and update types can
-- differ.
data FormWidgetConfig t u v = FormWidgetConfig
  { _formWidgetConfig_initialValue :: v
  , _formWidgetConfig_setValue :: Maybe (Event t u)

  --, _formWidgetConfig_name :: WidgetName
  --, _formWidgetConfig_initialAttributes :: Map WidgetName (Map AttributeName Text)
  --, _formWidgetConfig_modifyAttributes :: Map WidgetName (Maybe (Event t (Map AttributeName (Maybe Text))))
  }
type FormWidgetConfig' t a = FormWidgetConfig t a a

formWidgetConfig_initialValue :: Lens' (FormWidgetConfig t u v) v
formWidgetConfig_initialValue f (FormWidgetConfig iv sv) = (\iv' -> FormWidgetConfig iv' sv) <$> f iv

formWidgetConfig_setValue :: Lens' (FormWidgetConfig t u v) (Maybe (Event t u))
formWidgetConfig_setValue f (FormWidgetConfig iv sv) = (\sv' -> FormWidgetConfig iv sv') <$> f sv

instance Reflex t => Functor (FormWidgetConfig t u) where
  fmap f (FormWidgetConfig iv sv) = FormWidgetConfig (f iv) sv

instance Reflex t => Bifunctor (FormWidgetConfig t) where
  bimap f g (FormWidgetConfig iv sv) = FormWidgetConfig (g iv) (fmap f <$> sv)

instance MakeConfig (FormWidgetConfig t u v) v where
  mkCfg a = FormWidgetConfig a Nothing
  {-# INLINABLE mkCfg #-}

instance HasInitialValue (FormWidgetConfig t u v) where
  type InitialValue (FormWidgetConfig t u v) = v
  _initialValue = _formWidgetConfig_initialValue
  initialValue = formWidgetConfig_initialValue

instance HasSetValue (FormWidgetConfig t u v) where
  type SetValue (FormWidgetConfig t u v) = Maybe (Event t u)
  setValue = formWidgetConfig_setValue

fwc2iec :: (Reflex t, DomSpace s) => (v -> Text) -> FormWidgetConfig t v v -> InputElementConfig EventResult t s
fwc2iec toText (FormWidgetConfig iv sv) = def
  & inputElementConfig_initialValue .~ (toText iv)
  & inputElementConfig_setValue .~ (maybe never (fmap toText) sv)

iec2fwc :: Reflex t => (Text -> v) -> InputElementConfig EventResult t s -> FormWidgetConfig t v v
iec2fwc fromText iec = FormWidgetConfig
  (fromText $ _inputElementConfig_initialValue iec)
  (fmap fromText <$> (_inputElementConfig_setValue iec))

------------------------------------------------------------------------------

-- A primitive config defines one set of attributes for a single element.
data PrimFormWidgetConfig t u v = PrimFormWidgetConfig
  { _primFormWidgetConfig_fwc :: FormWidgetConfig t u v
  , _primFormWidgetConfig_initialAttributes :: Map AttributeName Text
  , _primFormWidgetConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
  }
type PrimFormWidgetConfig' t a = PrimFormWidgetConfig t a a

primFormWidgetConfig_fwc :: Lens' (PrimFormWidgetConfig t u v) (FormWidgetConfig t u v)
primFormWidgetConfig_fwc f a =
  (\newval -> a { _primFormWidgetConfig_fwc = newval }) <$> f (_primFormWidgetConfig_fwc a)

primFormWidgetConfig_initialAttributes :: Lens' (PrimFormWidgetConfig t u v) (Map AttributeName Text)
primFormWidgetConfig_initialAttributes f a =
  (\newval -> a { _primFormWidgetConfig_initialAttributes = newval }) <$> f (_primFormWidgetConfig_initialAttributes a)

primFormWidgetConfig_modifyAttributes :: Reflex t => Lens' (PrimFormWidgetConfig t u v) (Event t (Map AttributeName (Maybe Text)))
primFormWidgetConfig_modifyAttributes f a =
    (\newval -> a { _primFormWidgetConfig_modifyAttributes = Just newval }) <$> f (getter a)
  where
    getter = fromMaybe never . _primFormWidgetConfig_modifyAttributes

instance Reflex t => Functor (PrimFormWidgetConfig t u) where
  fmap f (PrimFormWidgetConfig fwc ia ma) = PrimFormWidgetConfig (f <$> fwc) ia ma

instance Reflex t => Bifunctor (PrimFormWidgetConfig t) where
  bimap f g (PrimFormWidgetConfig fwc ia ma) = PrimFormWidgetConfig (bimap f g fwc) ia ma

instance MakeConfig (PrimFormWidgetConfig t u v) v where
  mkCfg a = PrimFormWidgetConfig (mkCfg a) mempty Nothing
  {-# INLINABLE mkCfg #-}

instance InitialAttributes (PrimFormWidgetConfig t u v) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = primFormWidgetConfig_initialAttributes

instance ModifyAttributes t (PrimFormWidgetConfig t u v) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = primFormWidgetConfig_modifyAttributes

instance HasInitialValue (PrimFormWidgetConfig t u v) where
  type InitialValue (PrimFormWidgetConfig t u v) = v
  _initialValue = _initialValue . _primFormWidgetConfig_fwc
  initialValue = primFormWidgetConfig_fwc . initialValue

instance HasSetValue (PrimFormWidgetConfig t u v) where
  type SetValue (PrimFormWidgetConfig t u v) = Maybe (Event t u)
  setValue = primFormWidgetConfig_fwc . setValue

pfwc2iec :: (Reflex t, DomSpace s) => (v -> Text) -> PrimFormWidgetConfig t v v -> InputElementConfig EventResult t s
pfwc2iec toText (PrimFormWidgetConfig (FormWidgetConfig iv sv) ia ma) = def
  & inputElementConfig_initialValue .~ (toText iv)
  & inputElementConfig_setValue .~ (maybe never (fmap toText) sv)
  & initialAttributes .~ ia
  & modifyAttributes .~ fromMaybe never ma

iec2pfwc :: Reflex t => (Text -> v) -> InputElementConfig EventResult t s -> PrimFormWidgetConfig t v v
iec2pfwc fromText iec = PrimFormWidgetConfig fwc ia ma
  where
    ia = (_elementConfig_initialAttributes $ _inputElementConfig_elementConfig iec)
    ma = (_elementConfig_modifyAttributes $ _inputElementConfig_elementConfig iec)
    fwc = FormWidgetConfig
            (fromText $ _inputElementConfig_initialValue iec)
            (fmap fromText <$> (_inputElementConfig_setValue iec))

------------------------------------------------------------------------------

-- Implementation note: the _input field is 'Event t ()' instead of 'Event t a'
-- because this makes it possible to construct an Applicative instance which is
-- very useful when you need to compose input widgets. If you need the 'Event t
-- a' formulation, you can get it by tagging the _value field with the _input
-- event.

data FormWidget t a = FormWidget
  { _formWidget_value :: Dynamic t a
  -- ^ Contains the definitive value for this widget
  , _formWidget_input :: Event t ()
  -- ^ Fires whenever input happens in the widget itself.  This does not fire when the setValue event fires.
  , _formWidget_hasFocus :: Dynamic t Bool
  -- ^ Tells whether any of the sub-widgets in this widget have focus.
  }

formWidget_value :: Lens' (FormWidget t a) (Dynamic t a)
formWidget_value f (FormWidget v i hf) = (\v' -> FormWidget v' i hf) <$> f v

formWidget_input :: Lens' (FormWidget t a) (Event t ())
formWidget_input f (FormWidget v i hf) = (\i' -> FormWidget v i' hf) <$> f i

formWidget_hasFocus :: Lens' (FormWidget t a) (Dynamic t Bool)
formWidget_hasFocus f (FormWidget v i hf) = (\hf' -> FormWidget v i hf') <$> f hf

instance Reflex t => Functor (FormWidget t) where
  fmap f (FormWidget v i hf) = FormWidget (f <$> v) i hf

instance Reflex t => Applicative (FormWidget t) where
  pure a = FormWidget (constDyn a) never (constDyn False)
  FormWidget fv fi fh <*> FormWidget v i h = FormWidget (fv <*> v) (leftmost [fi, i]) ((||) <$> fh <*> h)

instance Reflex t => Monad (FormWidget t) where
  (FormWidget v i hf) >>= f = FormWidget v2 (leftmost [i, i2]) ((||) <$> hf <*> hf2)
    where
      diw = f <$> v
      v2 = join $ _formWidget_value <$> diw
      i2 = switchPromptlyDyn $ _formWidget_input <$> diw
      hf2 = join $ _formWidget_hasFocus <$> diw

instance HasValue (FormWidget t a) where
  type Value (FormWidget t a) = Dynamic t a
  value = _formWidget_value

ie2iw :: Reflex t => (Text -> a) -> InputElement er d t -> FormWidget t a
ie2iw fromText ie = FormWidget
  (fromText <$> value ie) (() <$ _inputElement_input ie) (_inputElement_hasFocus ie)

