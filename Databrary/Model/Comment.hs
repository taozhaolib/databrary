{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Comment
  ( module Databrary.Model.Comment.Types
  , blankComment
  , lookupComment
  , addComment
  , commentJSON
  ) where

import Data.Maybe (catMaybes, listToMaybe)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Comment.Types
import Databrary.Model.Comment.SQL

useTPG

blankComment :: Account -> Slot -> Comment
blankComment who slot = Comment
  { commentId = error "blankComment"
  , commentWho = who
  , commentSlot = slot
  , commentTime = error "blankComment"
  , commentText = ""
  , commentParents = []
  }

lookupComment :: (DBM m, MonadHasIdentity c m) => Id Comment -> m (Maybe Comment)
lookupComment i = do
  ident <- peek
  dbQuery1 $(selectQuery (selectComment 'ident) "$!WHERE comment.id = ${i}")

addComment :: DBM m => Comment -> m Comment
addComment c@Comment{..} = do
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO comment (who, container, segment, text, parent) VALUES (${partyId $ accountParty commentWho}, ${containerId $ slotContainer commentSlot}, ${slotSegment commentSlot}, ${commentText}, ${listToMaybe commentParents}) RETURNING id, time|]
  return c
    { commentId = i
    , commentTime = t
    }

commentJSON :: Comment -> JSON.Object
commentJSON Comment{ commentSlot = Slot{..}, ..} = JSON.record commentId $ catMaybes
  [ Just $ "container" JSON..= containerJSON slotContainer
  , segmentFull slotSegment ?!> ("segment" JSON..= slotSegment)
  , Just $ "who" JSON..= partyJSON (accountParty commentWho)
  , Just $ "time" JSON..= commentTime
  , Just $ "text" JSON..= commentText
  , null commentParents ?!> "parents" JSON..= commentParents
  ]
