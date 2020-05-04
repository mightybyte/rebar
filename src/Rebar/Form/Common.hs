module Rebar.Form.Common where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Rebar.FormWidget
------------------------------------------------------------------------------

-- | reflex-dom `inputElement` with chainweaver default styling:
textFormWidget
  :: DomBuilder t m
  => PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
textFormWidget cfg = do
    ie <- inputElement $ pfwc2iec id cfg
    return (ie2iw id ie)

-- | reflex-dom `inputElement` with chainweaver default styling:
parsingFormWidget
  :: DomBuilder t m
  => (Text -> Either String a)
  -> (Either String a -> Text)
  -> PrimFormWidgetConfig t (Either String a)
  -> m (FormWidget t (Either String a))
parsingFormWidget fromText toText cfg = do
    ie <- inputElement $ pfwc2iec toText cfg
    return (ie2iw fromText ie)

dropdownFormWidget
  :: DomBuilder t m
  => (a -> Text)
  -> (Text -> a)
  -> PrimFormWidgetConfig t a
  -> m b
  -> m (FormWidget t a, b)
dropdownFormWidget toText fromText cfg children = do
  let seCfg = def
        & selectElementConfig_initialValue .~ toText (_initialValue cfg)
        & selectElementConfig_setValue .~ (maybe never (fmap toText) $ view setValue cfg)
        & initialAttributes .~ view initialAttributes cfg
        & modifyAttributes .~ view modifyAttributes cfg
  (se, b) <- selectElement seCfg children
  let fw = FormWidget (fromText <$> _selectElement_value se)
                      (() <$ _selectElement_change se)
                      (_selectElement_hasFocus se)
  return (fw, b)

-- | Like comboBoxGlobalDatalist but handles creation of a unique datalist ID
-- for you and includes the datalist along with the combo box.
comboBox
  :: ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , MonadIO m
     )
  => Dynamic t [Text]
  -> PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
comboBox options cfg = do
  listNum :: Word64 <- liftIO randomIO
  let listId = "datalist-" <> T.pack (show listNum)
  res <- comboBoxGlobalDatalist listId cfg
  comboBoxDatalist listId options
  pure res

-- | HTML5 combo box widget that presents a dropdown but also allows work like a
-- text input.
comboBoxGlobalDatalist
  :: ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , MonadIO m
     )
  => Text
  -> PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
comboBoxGlobalDatalist datalistId cfg = do
  ie <- inputElement $ pfwc2iec id cfg
    & initialAttributes %~ (<> "list" =: datalistId)
  pure $ ie2iw id ie

-- | Renders an options list suitable for use with HTML5 combo boxes.
comboBoxDatalist
  :: ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     )
  => Text
  -> Dynamic t [Text]
  -> m ()
comboBoxDatalist datalistId items = elAttr "datalist" ("id" =: datalistId) $ do
  void $ simpleList items $ \v ->
    elDynAttr "option" (("value" =:) <$> v) blank
  pure ()

