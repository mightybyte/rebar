module Rebar.Form.Common where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Rebar.FormWidget
------------------------------------------------------------------------------

textFormWidget
  :: DomBuilder t m
  => PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
textFormWidget cfg = do
    ie <- inputElement $ pfwc2iec id cfg
    return (ie2iw id ie)

parsingFormWidget
  :: DomBuilder t m
  => (Text -> a)
  -> (a -> Text)
  -> PrimFormWidgetConfig t a
  -> m (FormWidget t a)
parsingFormWidget fromText toText cfg = do
    ie <- inputElement $ pfwc2iec toText cfg
    return (ie2iw fromText ie)

dropdownFormWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord a)
  => Dynamic t (Map a Text)
  -> PrimFormWidgetConfig t a
  -> m (FormWidget t a)
dropdownFormWidget options cfg = do
  let k0 = _initialValue cfg
      setK = fromMaybe never $ view setValue cfg
      initAttrs = view initialAttributes cfg
      modifyAttrs = view modifyAttributes cfg
  optionsWithAddedKeys <- fmap (zipDynWith Map.union options) $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
  defaultKey <- holdDyn k0 setK
  let (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
        in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
  let cfg = def
        & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
        & selectElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
        & selectElementConfig_setValue .~ fmap (T.pack . show) (attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK)
  (eRaw, _) <- selectElement cfg $ listWithKey indexedOptions $ \(ix, k) v -> do
    let optionAttrs = fmap (\dk -> "value" =: T.pack (show ix) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
    elDynAttr "option" optionAttrs $ dynText v
  let lookupSelected ks v = do
        key <- T.readMaybe $ T.unpack v
        Bimap.lookup key ks
  let eChange = attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return $ FormWidget dValue (() <$ attachPromptlyDynWith readKey ixKeys eChange) (_selectElement_hasFocus eRaw)

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

