{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Asset
  ( getAsset
  , viewAsset
  , AssetTarget(..)
  , postAsset
  , viewEditAsset
  , createAsset
  , viewCreateAsset
  , createSlotAsset
  , viewCreateSlotAsset
  , deleteAsset
  , downloadAsset
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad ((<=<), when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Database.PostgreSQL.Typed.Range as Range
import Network.HTTP.Types (StdMethod(DELETE), conflict409)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has (Has, view, peek, peeks)
import Databrary.Service.Types
import Databrary.Service.ResourceT
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.HTTP.Form.Errors
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Action.Route
import Databrary.Model.Segment
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Token
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.Model.Transcode
import Databrary.Store
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Store.Upload
import Databrary.Store.Temp
import Databrary.Media.AV
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.AssetSegment
import Databrary.Controller.Angular
import Databrary.View.Asset

getAsset :: Permission -> Id Asset -> AuthActionM AssetSlot
getAsset p i =
  checkPermission p =<< maybeAction =<< lookupAssetSlot i

assetJSONField :: (MonadDB m, MonadHasIdentity c m) => AssetSlot -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
assetJSONField a "creation" _ | view a >= PermissionEDIT = do
  (t, n) <- assetCreation $ slotAsset a
  return $ Just $ JSON.toJSON $ JSON.object $ catMaybes
    [ ("date" JSON..=) <$> t
    , ("name" JSON..=) <$> n
    ]
assetJSONField a "excerpts" _ =
  Just . JSON.toJSON . map (assetSegmentJSON . excerptAsset) <$> lookupAssetExcerpts a
assetJSONField _ _ _ = return Nothing

assetJSONQuery :: (MonadDB m, MonadHasIdentity c m) => AssetSlot -> JSON.Query -> m JSON.Object
assetJSONQuery vol = JSON.jsonQuery (assetSlotJSON vol) (assetJSONField vol)

viewAsset :: API -> Id Asset -> AppRAction
viewAsset api i = action GET (api, i) $ withAuth $ do
  when (api == HTML) angular
  asset <- getAsset PermissionPUBLIC i
  case api of
    JSON -> okResponse [] =<< assetJSONQuery asset =<< peeks Wai.queryString
    HTML -> okResponse [] $ T.pack $ show $ assetId $ slotAsset asset -- TODO

data AssetTarget
  = AssetTargetVolume Volume
  | AssetTargetSlot Slot
  | AssetTargetAsset AssetSlot

data FileUploadFile
  = FileUploadForm (FileInfo TempFile)
  | FileUploadToken Upload

fileUploadName :: FileUploadFile -> BS.ByteString
fileUploadName (FileUploadForm f) = fileName f
fileUploadName (FileUploadToken u) = uploadFilename u

fileUploadPath :: FileUploadFile -> Storage -> RawFilePath
fileUploadPath (FileUploadForm f) _ = tempFilePath $ fileContent f
fileUploadPath (FileUploadToken u) s = uploadFile u s

fileUploadRemove :: (MonadResourceT c m, MonadDB m, MonadStorage c m) => FileUploadFile -> m ()
fileUploadRemove (FileUploadForm f) = releaseTempFile $ fileContent f
fileUploadRemove (FileUploadToken u) = void $ removeUpload u

data FileUpload = FileUpload
  { fileUploadFile :: FileUploadFile
  , fileUploadFormat :: Format
  , fileUploadProbe :: Maybe AVProbe
  }

deformLookup :: (Monad m, Functor m, Deform a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT m (Maybe b)
deformLookup e l = Trav.mapM (deformMaybe' e <=< lift . l) =<< deformNonEmpty deform

detectUpload :: (MonadHasService c m, Has AV c, Has Storage c, MonadIO m) => FileUploadFile -> DeformT m FileUpload
detectUpload f =
  fd =<< deformMaybe' "Unknown or unsupported file format."
    (getFormatByFilename (fileUploadName f)) where
  fd fmt = case formatTranscodable fmt of
    Nothing -> return $ u Nothing
    Just t | t == videoFormat -> do
      av <- lift peek
      pr <- liftIO . try . avProbe av =<< lift (peeks (fileUploadPath f))
      case pr of
        Left e -> fail $ "Could not read video file: " ++ avErrorString e
        Right p -> return $ u $ Just p
    _ -> fail "Unhandled format conversion."
    where u = FileUpload f fmt

processAsset :: API -> AssetTarget -> AuthAction
processAsset api target = do
  let as@AssetSlot{ slotAsset = a, assetSlot = s } = case target of
        AssetTargetVolume t -> assetNoSlot $ blankAsset t
        AssetTargetSlot t -> AssetSlot (blankAsset (view t)) (Just t)
        AssetTargetAsset t -> t
  (as', up') <- runFormFiles [("file", maxAssetSize)] (api == HTML ?> htmlAssetForm target) $ do
    file <- "file" .:> deform
    upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
    upfile <- case (file, upload) of
      (Just f, Nothing) -> return $ Just $ FileUploadForm f
      (Nothing, Just u) -> return $ Just $ FileUploadToken u
      (Nothing, Nothing)
        | AssetTargetAsset _ <- target -> return Nothing
        | otherwise -> Nothing <$ deformError "File or upload required."
      _ -> Nothing <$ deformError "Conflicting uploaded files found."
    up <- Trav.mapM detectUpload upfile
    let fmt = maybe (assetFormat a) fileUploadFormat up
    name <- "name" .:> fmap (dropFormatExtension fmt) <$> deformNonEmpty deform
    classification <- "classification" .:> deformNonEmpty deform
    slot <-
      "container" .:> (<|> slotContainer <$> s) <$> deformLookup "Container not found." (lookupVolumeContainer (assetVolume a))
      >>= Trav.mapM (\c -> "position" .:> do
        let seg = slotSegment <$> s
        p <- (<|> (lowerBound . segmentRange =<< seg)) <$> deformNonEmpty deform
        Slot c . maybe fullSegment
          (\l -> Segment $ Range.bounded l (l + fromMaybe 0 ((segmentLength =<< seg) <|> assetDuration a)))
          <$> orElseM p (flatMapM (lift . findAssetContainerEnd) (isNothing s && isJust (assetDuration a) ?> c)))
    return
      ( as
        { slotAsset = a
          { assetName = TE.decodeUtf8 <$> name
          , assetRelease = classification
          , assetFormat = fmt
          }
        , assetSlot = slot
        }
      , up
      )
  as'' <- maybe (return as') (\up@FileUpload{ fileUploadFile = upfile } -> do
    a' <- addAsset (slotAsset as')
      { assetName = Just $ TE.decodeUtf8 $ fileUploadName upfile
      } . Just =<< peeks (fileUploadPath upfile)
    fileUploadRemove upfile
    case target of
      AssetTargetAsset _ -> supersedeAsset a a'
      _ -> return ()
    t <- Trav.mapM (addTranscode a' fullSegment defaultTranscodeOptions) (fileUploadProbe up)
    -- TODO startTranscode
    return as'
      { slotAsset = (maybe a' transcodeAsset t)
        { assetName = assetName (slotAsset as')
        }
      })
    up'
  changeAsset (slotAsset as'') Nothing
  _ <- changeAssetSlot as''
  case api of
    JSON -> okResponse [] $ assetSlotJSON as''
    HTML -> redirectRouteResponse [] $ viewAsset api (assetId (slotAsset as''))

postAsset :: API -> Id Asset -> AppRAction
postAsset api ai = multipartAction $ action POST (api, ai) $ withAuth $ do
  asset <- getAsset PermissionEDIT ai
  r <- assetIsSuperseded (slotAsset asset)
  guardAction (not r) $
    returnResponse conflict409 [] ("This file has already been replaced." :: T.Text)
  processAsset api $ AssetTargetAsset asset

viewEditAsset :: Id Asset -> AppRAction
viewEditAsset ai = action GET (HTML, ai, "edit" :: T.Text) $ withAuth $ do
  angular
  asset <- getAsset PermissionEDIT ai
  blankForm $ htmlAssetForm $ AssetTargetAsset asset

createAsset :: API -> Id Volume -> AppRAction
createAsset api vi = multipartAction $ action POST (api, vi, "asset" :: T.Text) $ withAuth $ do
  v <- getVolume PermissionEDIT vi
  processAsset api $ AssetTargetVolume v

viewCreateAsset :: Id Volume -> AppRAction
viewCreateAsset vi = action GET (HTML, vi, "asset" :: T.Text) $ withAuth $ do
  angular
  v <- getVolume PermissionEDIT vi
  blankForm $ htmlAssetForm $ AssetTargetVolume v

createSlotAsset :: API -> Id Slot -> AppRAction
createSlotAsset api si = multipartAction $ action POST (api, si, "asset" :: T.Text) $ withAuth $ do
  v <- getSlot PermissionEDIT si
  processAsset api $ AssetTargetSlot v

viewCreateSlotAsset :: Id Slot -> AppRAction
viewCreateSlotAsset si = action GET (HTML, si, "asset" :: T.Text) $ withAuth $ do
  angular
  s <- getSlot PermissionEDIT si
  blankForm $ htmlAssetForm $ AssetTargetSlot s

deleteAsset :: API -> Id Asset -> AppRAction
deleteAsset api ai = action DELETE (api, ai) $ withAuth $ do
  asset <- getAsset PermissionEDIT ai
  let asset' = asset{ assetSlot = Nothing }
  _ <- changeAssetSlot asset'
  case api of
    JSON -> okResponse [] $ assetSlotJSON asset'
    HTML -> redirectRouteResponse [] $ viewAsset api (assetId (slotAsset asset'))

downloadAsset :: Id Asset -> AppRAction
downloadAsset ai = action GET (ai, "download" :: T.Text) $ withAuth $ do
  as <- getAsset PermissionREAD ai
  serveAssetSegment $ assetSlotSegment as
