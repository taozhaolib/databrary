{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Asset
  ( viewAsset
  , postAsset
  , createAsset
  ) where

import Control.Monad ((<=<), when, void)
import Control.Monad.Trans.Class (lift)
import qualified Data.Foldable as Fold
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Database.PostgreSQL.Typed.Range as Range
import Network.Wai.Parse (FileInfo(..))

import Control.Applicative.Ops
import Control.Has (peeks)
import Databrary.Web.Form
import Databrary.Web.Form.Errors
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Token
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Slot
import Databrary.Model.SlotAsset
import Databrary.Store.Asset
import Databrary.Store.Upload
import Databrary.Store.Temp
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Asset

withAsset :: Permission -> Id Asset -> (AssetSlot -> AuthAction) -> AppAction
withAsset p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupAssetSlot i

viewAsset :: API -> Id Asset -> AppRAction
viewAsset api i = action GET (api, i) $
  withAsset PermissionPUBLIC i $
    case api of
      JSON -> okResponse [] . assetJSON
      HTML -> okResponse [] . show . assetId -- TODO

deformLookup :: (Monad m, Functor m, Deform a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT m (Maybe b)
deformLookup e l = Trav.mapM (deformMaybe' e <=< lift . l) =<< deform

assetForm :: Either Asest SlotAsset ->

postAsset :: API -> Id Asset -> AppRAction
postAsset api ai = action POST (api, ai) $
  withAsset PermissionEDIT ai $ \asset -> do
    sa <- lookupAssetSlotAsset asset
    asset' <- runForm (api == HTML ?> htmlAssetForm (Right asset)) $ do
      name <- "name" .:> deform
      classification <- "classification" .:> deform
      cont <- "container" .:> deformLookup "Container not found." (lookupVolumeContainer vol)
      pos <- "position" .:> deform
      return asset
        { assetName = name
        , assetClassification = classification
        }
    changeAsset asset'
    case api of
      JSON -> okResponse [] $ assetJSON asset'
      HTML -> redirectRouteResponse [] $ viewAsset api ai

createAsset :: API -> Id Volume -> AppRAction
createAsset api vi = action POST (api, vi, "asset" :: T.Text) $
  withVolume PermissionEDIT vi $ \vol -> do
    -- adm <- peeks ((PermissionADMIN <=) . accessMember')
    (fd, ufs) <- getFormData [("file", maxAssetSize)]
    let file = lookup "file" ufs
    (ba, upfile, name, slot) <- runFormWith fd (api == HTML ?> htmlAssetForm (Left vol)) $ do
      upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
      upfile <- case (file, upload) of
        (Just f, Nothing) -> return (Left f)
        (Nothing, Just u) -> return (Right u)
        _ -> deformError' "Either file XOR upload required."
      let fname = either fileName uploadFilename upfile
      (fname', fmt) <- deformMaybe' "Unknown or unsupported file format." $ getFormatByFilename fname
      name <- "name" .:> deform
      classification <- "classification" .:> deform
      cont <- "container" .:> deformLookup "Container not found." (lookupVolumeContainer vol)
      pos <- "position" .:> deform
      return
        ( (blankAsset vol)
          { assetFormat = fmt
          , assetClassification = classification
          , assetName = Just $ TE.decodeUtf8 fname'
          }
        , upfile
        , name
        , fmap (`Slot` Range.normal pos pos) cont
        )
    path <- either (return . tempFilePath . fileContent) (peeks . uploadFile) upfile
    asset <- addAsset ba (Just path)
    either (releaseTempFile . fileContent) (void . removeUpload) upfile
    when (name /= assetName asset) $ changeAsset asset{ assetName = name }
    let sa = fmap (\s -> SlotAsset asset s Nothing) slot
    Fold.mapM_ (changeSlotAsset) sa
    let asa = maybe (Left asset) Right sa
    case api of
      JSON -> okResponse [] $ either assetJSON slotAssetJSON asa
      HTML -> redirectRouteResponse [] $ viewAsset api $ assetId $ either id slotAsset asa
