{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , lookupTag
  , findTags
  , addTag
  , addTagUse
  , removeTagUse
  , lookupTopTagWeight
  , lookupSlotTagCoverage
  , tagWeightJSON
  , tagCoverageJSON
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

useTPG

lookupTag :: MonadDB m => TagName -> m (Maybe Tag)
lookupTag n =
  dbQuery1 $(selectQuery selectTag "$WHERE tag.name = ${n}::varchar")

findTags :: MonadDB m => TagName -> m [Tag]
findTags (TagName n) = -- TagName restrictions obviate pattern escaping
  dbQuery $(selectQuery selectTag "$WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar")

addTag :: MonadDB m => TagName -> m Tag
addTag n =
  dbQuery1' $ (`Tag` n) <$> [pgSQL|!SELECT get_tag(${n})|]

addTagUse :: MonadDB m => TagUse -> m Bool
addTagUse t =
  either (const False) ((0 <) . fst) <$> dbTryQuery (guard . isExclusionViolation)
    (if tagKeyword t
      then $(insertTagUse True 't)
      else $(insertTagUse False 't))

removeTagUse :: MonadDB m => TagUse -> m Bool
removeTagUse t =
  dbExecute1
    (if tagKeyword t 
      then $(deleteTagUse True 't)
      else $(deleteTagUse False 't))

lookupTopTagWeight :: MonadDB m => Int -> m [TagWeight]
lookupTopTagWeight lim =
  dbQuery $(selectQuery (selectTagWeight "") "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

lookupSlotTagCoverage :: (MonadDB m, MonadHasIdentity c m) => Slot -> Int -> m [TagCoverage]
lookupSlotTagCoverage slot lim = do
  ident <- peek
  dbQuery $(selectSlotTagCoverage 'ident 'slot >>= (`selectQuery` "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}"))

tagWeightJSON :: TagWeight -> JSON.Object
tagWeightJSON TagWeight{..} = JSON.record (tagName tagWeightTag) $
  [ "weight" JSON..= tagWeightWeight
  ]

tagCoverageJSON :: TagCoverage -> JSON.Object
tagCoverageJSON TagCoverage{..} = tagWeightJSON tagCoverageWeight JSON..++ catMaybes
  [ Just $ "coverage" JSON..= tagCoverageSegments
  , null tagCoverageKeywords ?!> "keyword" JSON..= tagCoverageKeywords
  , null tagCoverageVotes ?!> "vote" JSON..= tagCoverageVotes
  ]
