{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Asset
  ( viewAsset
  , createAsset
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import Network.Wai.Parse (FileInfo(..))

import Control.Applicative.Ops
import Control.Has (peeks)
import Databrary.Web.Form
import Databrary.Web.Form.Errors
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Action.Route
import Databrary.Model.Permission
import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Token
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Store.Asset
import Databrary.Store.Upload
import Databrary.Store.Temp
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Asset

withAsset :: Permission -> Id Asset -> (Asset -> AuthAction) -> AppAction
withAsset p i f = withAuth $
  f =<< checkPermission p =<< maybeAction =<< lookupAsset i

viewAsset :: API -> Id Asset -> AppRAction
viewAsset api i = action GET (api, i) $
  withAsset PermissionPUBLIC i $
    case api of
      JSON -> okResponse [] . assetJSON
      HTML -> okResponse [] . show . assetId -- TODO

deformLookup :: (Monad m, Functor m, Deform a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT m (Maybe b)
deformLookup e l = Trav.mapM (deformMaybe' e <=< lift . l) =<< deform

createAsset :: API -> Id Volume -> AppRAction
createAsset api vi = action POST (api, vi, "asset" :: T.Text) $
  withVolume PermissionEDIT vi $ \vol -> do
    adm <- peeks ((PermissionADMIN <=) . accessMember')
    (fd, ufs) <- getFormData [("file", maxAssetSize)]
    let file = lookup "file" ufs
    asset <- runFormWith fd (api == HTML ?> htmlAssetForm vol) $ do
      upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
      upfile <- case (file, upload) of
        (Just f, Nothing) -> return (Left f)
        (Nothing, Just u) -> return (Right u)
        _ -> deformError' "Either file XOR upload required."
      let fname = either fileName uploadFilename upfile
      (fname', fmt) <- deformMaybe' "Unknown or unsupported file format." $ getFormatByFilename fname
      classification <- "classification" .:> deform
      path <- either (return . tempFilePath . fileContent) (lift . peeks . uploadFile) upfile
      asset <- lift $ addAsset (blankAsset vol)
        { assetFormat = fmt
        , assetClassification = classification
        , assetName = Just $ TE.decodeUtf8 fname'
        } (Just path)
      name :: Maybe T.Text <- "name" .:> deform
      cont <- "container" .:> deformLookup "Container not found." (lookupVolumeContainer vol)
      pos :: Maybe Offset <- "position" .:> deform
      return asset
    case api of
      JSON -> okResponse [] $ assetJSON asset
      HTML -> redirectRouteResponse [] $ viewAsset api $ assetId asset
