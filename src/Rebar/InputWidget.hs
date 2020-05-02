module Rebar.InputWidget
  ( InputWidgetConfig
  , inputWidgetConfig_initialValue
  , inputWidgetConfig_setValue
  , InputWidget
  , inputWidget_value
  , inputWidget_input
  , inputWidget_hasFocus
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

-- A dot-separated namespace
newtype WidgetName = WidgetName { unWidgetName :: Text }
  deriving (Eq, Ord, IsString)

data InputWidgetConfig t a = InputWidgetConfig
  { _inputWidgetConfig_initialValue :: a
  , _inputWidgetConfig_setValue :: Maybe (Event t a)

  , _inputWidgetConfig_name :: WidgetName
  , _inputWidgetConfig_initialAttributes :: Map WidgetName (Map AttributeName Text)
  , _inputWidgetConfig_modifyAttributes :: Map WidgetName (Maybe (Event t (Map AttributeName (Maybe Text))))
  }

inputWidgetConfig_initialValue :: Lens' (InputWidgetConfig t a) a
inputWidgetConfig_initialValue f (InputWidgetConfig iv sv) = (\iv' -> InputWidgetConfig iv' sv) <$> f iv

inputWidgetConfig_setValue :: Lens' (InputWidgetConfig t a) (Maybe (Event t a))
inputWidgetConfig_setValue f (InputWidgetConfig iv sv) = (\sv' -> InputWidgetConfig iv sv') <$> f sv

instance Reflex t => Functor (InputWidgetConfig t) where
  fmap f (InputWidgetConfig iv sv) = InputWidgetConfig (f iv) (fmap f <$> sv)

mkIW :: (Reflex t, Default a) => InputWidgetConfig t a
mkIW name = InputWidgetConfig def Nothing name mempty mempty

-- Implementation note: the _input field is 'Event t ()' instead of 'Event t a'
-- because this makes it possible to construct an Applicative instance which is
-- very useful when you need to compose input widgets.

data InputWidget t a = InputWidget
  { _inputWidget_value :: Dynamic t a
  -- ^ Contains the definitive value for this widget
  , _inputWidget_input :: Event t ()
  -- ^ Fires whenever input happens in the widget itself.  This does not fire when the setValue event fires.
  , _inputWidget_hasFocus :: Dynamic t Bool
  -- ^ Tells whether any of the sub-widgets in this widget have focus.
  }

inputWidget_value :: Lens' (InputWidget t a) (Dynamic t a)
inputWidget_value f (InputWidget v i hf) = (\v' -> InputWidget v' i hf) <$> f v

inputWidget_input :: Lens' (InputWidget t a) (Event t ())
inputWidget_input f (InputWidget v i hf) = (\i' -> InputWidget v i' hf) <$> f i

inputWidget_hasFocus :: Lens' (InputWidget t a) (Dynamic t Bool)
inputWidget_hasFocus f (InputWidget v i hf) = (\hf' -> InputWidget v i hf') <$> f hf

instance Reflex t => Functor (InputWidget t) where
  fmap f (InputWidget v i hf) = InputWidget (f <$> v) i hf

instance Reflex t => Applicative (InputWidget t) where
  pure a = InputWidget (constDyn a) never (constDyn False)
  InputWidget fv fi fh <*> InputWidget v i h = InputWidget (fv <*> v) (leftmost [fi, i]) ((||) <$> fh <*> h)

instance HasValue (InputWidget t a) where
  type Value (InputWidget t a) = Dynamic t a
  value = _inputWidget_value

