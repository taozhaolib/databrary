{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Asset
  ( viewAsset
  , postAsset
  , createAsset
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), when, void)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Database.PostgreSQL.Typed.Range as Range
import Network.HTTP.Types (conflict409)
import Network.Wai.Parse (FileInfo(..))

import Control.Applicative.Ops
import Control.Has (peeks)
import Databrary.Resource
import Databrary.DB
import Databrary.Web.Form
import Databrary.Web.Form.Errors
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Token
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Store
import Databrary.Store.Storage
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
      JSON -> okResponse [] . assetSlotJSON
      HTML -> okResponse [] . show . assetId . slotAsset -- TODO

data FileUpload
  = FileUploadForm
    { _fileUploadForm :: FileInfo TempFile
    , fileUploadFormat :: Format
    }
  | FileUploadToken
    { _fileUploadToken :: Upload
    , fileUploadFormat :: Format
    }

fileUploadName :: FileUpload -> BS.ByteString
fileUploadName (FileUploadForm f _) = fileName f
fileUploadName (FileUploadToken u _) = uploadFilename u

fileUploadPath :: FileUpload -> Storage -> RawFilePath
fileUploadPath (FileUploadForm f _) _ = tempFilePath $ fileContent f
fileUploadPath (FileUploadToken u _) s = uploadFile u s

fileUploadRemove :: (MonadResourceT c m, DBM m, MonadStorage c m) => FileUpload -> m ()
fileUploadRemove (FileUploadForm f _) = releaseTempFile $ fileContent f
fileUploadRemove (FileUploadToken u _) = void $ removeUpload u

deformLookup :: (Monad m, Functor m, Deform a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT m (Maybe b)
deformLookup e l = Trav.mapM (deformMaybe' e <=< lift . l) =<< deform

processAsset :: API -> Either Volume AssetSlot -> AuthAction
processAsset api target = do
  (fd, ufs) <- getFormData [("file", maxAssetSize)]
  let as@AssetSlot{ slotAsset = a, assetSlot = s } = either (assetNoSlot . blankAsset) id target
  (as', upfile) <- runFormWith fd (api == HTML ?> htmlAssetForm target) $ do
    let file = lookup "file" ufs
    upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
    let ffmt = deformMaybe' "Unknown or unsupported file format." . getFormatByFilename
    upfile <- case (file, upload) of
      (Just f, Nothing) -> Just . FileUploadForm f <$> ffmt (fileName f)
      (Nothing, Just u) -> Just . FileUploadToken u <$> ffmt (uploadFilename u)
      (Nothing, Nothing) -> do
        when (isLeft target) $ deformError "File or upload required."
        return $ Nothing
      _ -> Nothing <$ deformError "Conflicting uploaded files found."
    let fmt = maybe (assetFormat a) fileUploadFormat upfile
    name <- "name" .:> fmap (dropFormatExtension fmt) <$> deform
    classification <- "classification" .:> deform
    slot <-
      "container" .:> (<|> slotContainer <$> s) <$> deformLookup "Container not found." (lookupVolumeContainer (assetVolume a))
      >>= Trav.mapM (\c -> "position" .:> do
        let seg = slotSegment <$> s
        p <- (<|> (lowerBound =<< seg)) <$> deform
        Slot c . maybe Range.full
          (\l -> Range.bounded l (l + fromMaybe 0 ((segmentLength =<< seg) <|> assetDuration a)))
          <$> orElseM p (flatMapM (lift . findAssetContainerEnd) (isNothing s && isJust (assetDuration a) ?> c)))
    return
      ( as
        { slotAsset = a
          { assetName = TE.decodeUtf8 <$> name
          , assetClassification = classification
          , assetFormat = fmt
          }
        , assetSlot = slot
        }
      , upfile
      )
  as'' <- maybe (return as') (\up -> do
    a' <- addAsset (slotAsset as')
      { assetName = Just $ TE.decodeUtf8 $ fileUploadName up
      } . Just =<< peeks (fileUploadPath up)
    fileUploadRemove up
    when (isRight target) $ supersedeAsset a a'
    return as'
      { slotAsset = a'
        { assetName = assetName (slotAsset as')
        }
      })
    upfile
  changeAsset (slotAsset as'')
  changeAssetSlot as''
  case api of
    JSON -> okResponse [] $ assetSlotJSON as''
    HTML -> redirectRouteResponse [] $ viewAsset api (assetId (slotAsset as''))

postAsset :: API -> Id Asset -> AppRAction
postAsset api ai = action POST (api, ai) $
  withAsset PermissionEDIT ai $ \asset -> do
    r <- assetIsSuperseded (slotAsset asset)
    guardAction (not r) $
      returnResponse conflict409 [] ("This file has already been replaced." :: T.Text)
    processAsset api (Right asset)

createAsset :: API -> Id Volume -> AppRAction
createAsset api vi = action POST (api, vi, "asset" :: T.Text) $
  withVolume PermissionEDIT vi $
    processAsset api . Left
