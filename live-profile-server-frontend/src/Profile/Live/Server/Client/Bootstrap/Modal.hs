{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module Profile.Live.Server.Client.Bootstrap.Modal(
    modal
  , simpleModal
  , simpleValidateModal
  , confirm
  , ModalId(..)
  , Modal(..)
  -- * Modal configuration
  , ModalConfig(..)
  , defaultModalCfg
  , modalCfgDismiss
  , modalCfgTitle
  , modalCfgShow
  , modalCfgHideBody
  -- * Simple modal configuration
  , SimpleModalConfig(..)
  , defaultSimpleModalCfg
  , HasAcceptTitle(..)
  , HasCancelTitle(..)
  , HasModalCfg(..)
  -- * Confirm configuration
  , ConfirmConfig(..)
  , defaultConfirmConfig
  , HasTitle(..)
  -- * Utils
  , modalShowOn
  , modalHideOn
  , modalShown
  , modalHidden
  , cancelModalBtn
  , acceptModalBtn
  ) where 

import Control.Lens.TH
import Control.Monad.IO.Class
import Data.Default
import Data.Dependent.Map
import Data.JSString (pack)
import GHCJS.Foreign.Callback
import GHCJS.Types
import Reflex.Dom 
import Reflex.Host.Class

import qualified Data.Map as Map 

import Profile.Live.Server.Client.Utils

foreign import javascript unsafe "$('#'+$1).modal({ backdrop: 'static', keyboard: false });" js_showModal :: JSString -> IO ()
foreign import javascript unsafe "$('#'+$1).modal('hide');" js_hideModal :: JSString -> IO ()
foreign import javascript unsafe "$('#'+$1).on('show.bs.modal',$2);" js_onModalShown :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).off('show.bs.modal',$2);" js_offModalShown :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).on('hide.bs.modal',$2);" js_onModalHidden :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).off('hide.bs.modal',$2);" js_offModalHidden :: JSString -> Callback (IO ()) -> IO ()

-- | Unique modal id
newtype ModalId = ModalId { unModalId :: String }
  deriving (Eq, Show)

-- | Show given modal
showModal :: MonadIO m => ModalId -> m ()
showModal (ModalId i) = liftIO $ js_showModal $ pack i 

-- | Hide given modal
hideModal :: MonadIO m => ModalId -> m ()
hideModal (ModalId i) = liftIO $ js_hideModal $ pack i 

-- | Bind showing the modal on given event
modalShowOn :: MonadWidget t m => ModalId -> Event t a -> m ()
modalShowOn i e = performEvent_ (const (liftIO $ showModal i) <$> e)

-- | Bind hiding the modal on given event
modalHideOn :: MonadWidget t m => ModalId -> Event t a -> m ()
modalHideOn i e = performEvent_ (const (liftIO $ hideModal i) <$> e)

-- | Holds prerequisites for modal creation
data ModalConfig t = ModalConfig {
  _modalCfgDismiss :: !Bool -- ^ Use dismiss button
, _modalCfgTitle :: !String -- ^ Display modal title
, _modalCfgShow :: Event t () -- ^ When to show the modal
, _modalCfgHideBody :: !Bool -- ^ Should render full body widget
}

$(makeLenses ''ModalConfig)

-- | Default values for modal config
defaultModalCfg :: Reflex t => ModalConfig t
defaultModalCfg = ModalConfig {
    _modalCfgDismiss = True
  , _modalCfgTitle = "Modal title"
  , _modalCfgShow = never
  , _modalCfgHideBody = False
  }

instance Reflex t => Default (ModalConfig t) where 
  def = defaultModalCfg

-- | Holds important values that are needed after a modal creation
data Modal t a = Modal {
  modalId :: !ModalId -- ^ Unique modal id
, modalValue :: !(Event t (Maybe a)) -- ^ Modal output value event, 'Nothing' means that user dismissed the dialog
}

-- | Create bootstrap dialog
modal :: MonadWidget t m => ModalConfig t 
 -> m a -- ^ Modal body
 -> (ModalId -> a -> m (Event t (Maybe b))) -- ^ Modal footer that defines event of dialog success/dismiss
 -> m (Modal t b)
modal ModalConfig{..} bodyWidget footerWidget = do
  i <- genModalId
  let i' = ModalId i
  modalShowOn i' _modalCfgShow
  elAttr "div" (Map.fromList 
    [ ("class", "modal fade")
    , ("tabindex", "-1") 
    , ("role", "dialog")
    , ("id", i)]) $
      elClass "div" "modal-dialog" $
      elClass "div" "modal-content" $ do
        closeEv <- elClass "div" "modal-header" modalHeader

        let renderBody = if _modalCfgHideBody then id else elClass "div" "modal-body"
        a <- renderBody bodyWidget 

        ev <- elClass "div" "modal-footer" $ footerWidget i' a
        return $ Modal {
            modalId = i'
          , modalValue = leftmost [closeEv, ev]
          }
  where
  genModalId = do 
    i <- genId
    return $ "modal" ++ show i 

  modalHeader = do 
    closeEv <- if _modalCfgDismiss 
      then do 
        (e, _) <- elAttr' "button" (Map.fromList 
          [ ("type", "button")
          , ("class", "close")
          , ("data-dismiss", "modal")
          , ("aria-label", "Close")] ) $ 
            elAttr "span" (Map.singleton "aria-hidden" "true") $ text "×"
        return $ fmap (const Nothing) $ domEvent Click e
      else return never
    elClass "h4" "modal-title" $ text _modalCfgTitle
    return closeEv 

-- | Help to create modal cancel button
cancelModalBtn :: MonadWidget t m => String -> m (Event t ())
cancelModalBtn title = do
  (e, _) <- elAttr' "button" (Map.fromList 
    [ ("type", "button")
    , ("class", "btn btn-default")
    , ("data-dismiss", "modal")
    ]) $ text title 
  return $ domEvent Click e

-- | Help to create modal accept button
acceptModalBtn :: MonadWidget t m => String -> m (Event t ())
acceptModalBtn title = do 
  (e, _) <- elAttr' "button" (Map.fromList 
    [ ("type", "button")
    , ("class", "btn btn-primary")
    ]) $ text title 
  return $ domEvent Click e

-- | Holds prerequisites for modal creation
data SimpleModalConfig t = SimpleModalConfig {
  _simpleModalConfigAcceptTitle :: !String -- ^ Display for OK button
, _simpleModalConfigCancelTitle :: !String -- ^ Display for Cancel button
, _simpleModalConfigModalCfg :: !(ModalConfig t) -- ^ More general config
}

$(makeFields ''SimpleModalConfig)

-- | Default values for modal config
defaultSimpleModalCfg :: Reflex t => SimpleModalConfig t
defaultSimpleModalCfg = SimpleModalConfig {
    _simpleModalConfigAcceptTitle = "OK"
  , _simpleModalConfigCancelTitle = "Cancel"
  , _simpleModalConfigModalCfg = defaultModalCfg
  }

instance Reflex t => Default (SimpleModalConfig t) where 
  def = defaultSimpleModalCfg

-- | Create simple modal with "OK" and "Cancel" buttons
simpleModal :: MonadWidget t m => SimpleModalConfig t
  -> m (Dynamic t a) -- ^ Modal body
  -> m (Modal t a)
simpleModal SimpleModalConfig{..} body = modal _simpleModalConfigModalCfg body footer 
  where 
  footer i dyna = do 
    cancelEv <- cancelModalBtn _simpleModalConfigCancelTitle
    acceptEv <- acceptModalBtn _simpleModalConfigAcceptTitle
    let acceptEv' = fmap Just $ dyna `tagDyn` acceptEv
        cancelEv' = fmap (const Nothing) cancelEv
    modalHideOn i acceptEv'
    return $ leftmost [cancelEv', acceptEv']

-- | Configuration of confirm modal
data ConfirmConfig t = ConfirmConfig {
  _confirmConfigTitle :: !String -- ^ Display modal title
, _confirmConfigAcceptTitle :: !String -- ^ String on OK button
, _confirmConfigCancelTitle :: !String -- ^ String on Cancel button
} 

$(makeFields ''ConfirmConfig)

-- | Default configuration for confirm modal
defaultConfirmConfig :: Reflex t => ConfirmConfig t 
defaultConfirmConfig = ConfirmConfig {
    _confirmConfigTitle = "Are you shure?"
  , _confirmConfigAcceptTitle = "OK"
  , _confirmConfigCancelTitle = "Cancel"
  }

instance Reflex t => Default (ConfirmConfig t) where 
  def = defaultConfirmConfig

-- | Create confirm modal with "OK" and "Cancel" buttons
--
-- Passes events through itself only when user clicked "OK"
confirm :: forall t m a . (Reflex t, MonadWidget t m)
  => ConfirmConfig t -- Display configuration
  -> Event t a -- ^ Value we want to ask user about
  -> m (Event t a) -- ^ Fires only when user accepted the modal
confirm ConfirmConfig{..} ea = fmapMaybe id . modalValue <$> modal mcfg body footer 
  where 
  body = holdDyn Nothing (Just <$> ea)
  mcfg = ModalConfig {
      _modalCfgDismiss = True 
    , _modalCfgTitle = _confirmConfigTitle
    , _modalCfgShow = const () <$> ea
    , _modalCfgHideBody = True
    }

  footer i dyna = do 
    cancelEv <- cancelModalBtn _confirmConfigCancelTitle
    acceptEv <- acceptModalBtn _confirmConfigAcceptTitle
    let acceptEv' = dyna `tagDyn` acceptEv
        cancelEv' = fmap (const Nothing) cancelEv
    modalHideOn i acceptEv'
    return $ leftmost [cancelEv', acceptEv']

-- | Create simple modal with "OK" and "Cancel" buttons
simpleValidateModal :: forall t m a b . MonadWidget t m 
  => SimpleModalConfig t -- ^ Configuration of simple modal
  -> m (Dynamic t a) -- ^ Modal body
  -> (a -> WidgetHost m (Either String b)) -- ^ Validation
  -> m (Modal t b)
simpleValidateModal SimpleModalConfig{..} body validate = modal _simpleModalConfigModalCfg body footer 
  where 
  footer i dyna = do 
    cancelEv <- cancelModalBtn _simpleModalConfigCancelTitle
    acceptEv <- acceptModalBtn _simpleModalConfigAcceptTitle
    let acceptEv'  = dyna `tagDyn` acceptEv :: Event t a

    (validateEv :: Event t (Either String b)) <- performEvent $ validate <$> acceptEv'
    
    let acceptedEv = fmapMaybe (either (const Nothing) Just) validateEv :: Event t b
        failedEv = fmapMaybe (either Just (const Nothing)) validateEv :: Event t String
        cancelEv'  = fmap (const Nothing) cancelEv :: Event t (Maybe b)

    _ <- widgetHold (pure ()) $ danger <$> failedEv 

    modalHideOn i acceptedEv
    return $ leftmost [cancelEv', fmap Just acceptedEv]

-- | Add callback when the modal is shown, returns teardown callback
onModalShown :: MonadIO m => ModalId -> IO () -> m (IO ())
onModalShown (ModalId i) f = liftIO $ do
  let i' = pack i
  c <- syncCallback ContinueAsync f
  js_onModalShown i' c
  return $ do
    js_offModalShown i' c
    releaseCallback c

-- | Add callback when the modal is hidden, returns teardown callback
onModalHidden :: MonadIO m => ModalId -> IO () -> m (IO ())
onModalHidden (ModalId i) f = liftIO $ do
  let i' = pack i
  c <- syncCallback ContinueAsync f
  js_onModalHidden i' c
  return $ do
    js_offModalHidden i' c
    releaseCallback c

-- | Make event that fires when the modal is shown
modalShown :: MonadWidget t m => ModalId -> m (Event t ())
modalShown i = do
  runWithActions <- askRunWithActions
  postGui <- askPostGui
  newEventWithTrigger $ \tr -> onModalShown i $ postGui $ runWithActions [tr :=> pure ()]

-- | Make event that fires when the modal is hidden
modalHidden :: MonadWidget t m => ModalId -> m (Event t ())
modalHidden i = do 
  runWithActions <- askRunWithActions
  postGui <- askPostGui
  newEventWithTrigger $ \tr -> onModalHidden i $ postGui $ runWithActions [tr :=> pure ()]

-- Example of generated modal:
-- <div class="modal fade" tabindex="-1" role="dialog">
--   <div class="modal-dialog">
--     <div class="modal-content">
--       <div class="modal-header">
--         <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
--         <h4 class="modal-title">Modal title</h4>
--       </div>
--       <div class="modal-body">
--         <p>One fine body&hellip;</p>
--       </div>
--       <div class="modal-footer">
--         <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
--         <button type="button" class="btn btn-primary">Save changes</button>
--       </div>
--     </div><!-- /.modal-content -->
--   </div><!-- /.modal-dialog -->
-- </div><!-- /.modal -->