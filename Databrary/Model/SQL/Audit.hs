module Databrary.Model.SQL.Audit
  ( auditAddQuery
  , auditRemoveQuery
  , auditChangeQuery
  ) where

import Data.List (intercalate)
import Database.PostgreSQL.Typed.Query (makePGQuery)
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteral)
import qualified Language.Haskell.TH as TH

import Databrary.Model.Types.Audit
import Databrary.Model.SQL

actionCmd :: AuditAction -> String
actionCmd AuditActionAdd = "INSERT INTO"
actionCmd AuditActionChange = "UPDATE"
actionCmd AuditActionRemove = "DELETE FROM"
actionCmd a = error $ "actionCmd: " ++ show a

auditQuery :: AuditAction -> String -> String -> Maybe SelectOutput -> TH.ExpQ
auditQuery action table stmt returning =
  maybe (makePGQuery flags sql) (makeQuery flags ((sql ++) . (" RETURNING " ++))) returning
  where
  flags = simpleQueryFlags
  sql = "WITH audit_row AS (" ++ actionCmd action ++ ' ' : table ++ ' ' : stmt
    ++ " RETURNING *) INSERT INTO audit." ++ table
    ++ " SELECT CURRENT_TIMESTAMP, ${identityId}, ${clientIP}, " ++ pgSafeLiteral action ++ ", * FROM audit_row"

auditAddQuery :: String -> [(String, String)] -> Maybe SelectOutput -> TH.ExpQ
auditAddQuery table args =
  auditQuery AuditActionAdd table 
    ('(' : intercalate "," (map fst args) ++ ") VALUES (" ++ intercalate "," (map snd args) ++ ")")

auditRemoveQuery :: String -> String -> Maybe SelectOutput -> TH.ExpQ
auditRemoveQuery table wher =
  auditQuery AuditActionRemove table ("WHERE " ++ wher)

auditChangeQuery :: String -> [(String, String)] -> String -> Maybe SelectOutput -> TH.ExpQ
auditChangeQuery table sets wher =
  auditQuery AuditActionChange table
    ("SET " ++ intercalate "," (map (\(c,v) -> c ++ "=" ++ v) sets) ++ " WHERE " ++ wher)
