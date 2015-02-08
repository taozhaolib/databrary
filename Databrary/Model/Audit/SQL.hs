{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Audit.SQL
  ( auditInsert
  , auditDelete
  , auditUpdate
  ) where

import Data.List (intercalate)
import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteral)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Audit.Types

actionCmd :: AuditAction -> String
actionCmd AuditActionAdd = "INSERT INTO"
actionCmd AuditActionChange = "UPDATE"
actionCmd AuditActionRemove = "DELETE FROM"
actionCmd a = error $ "actionCmd: " ++ show a

auditQuery :: AuditAction -> String -> String -> Maybe SelectOutput -> TH.ExpQ
auditQuery action table stmt returning = do
  who <- TH.newName "who"
  ip <- TH.newName "ip"
  let sql = "WITH audit_row AS (" ++ actionCmd action ++ ' ' : table ++ ' ' : stmt
        ++ " RETURNING *) INSERT INTO audit." ++ table
        ++ " SELECT CURRENT_TIMESTAMP, ${" ++ nameRef who ++ "}, ${" ++ nameRef ip ++ "}, " ++ pgSafeLiteral action ++ ", * FROM audit_row"
  TH.doE
    [ return $ TH.BindS
      (TH.RecP 'AuditIdentity [('auditWho, TH.VarP who), ('auditIp, TH.VarP ip)])
      (TH.VarE (TH.mkName "Databrary.Model.Audit.getAuditIdentity"))
    , TH.noBindS $ TH.appE (TH.varE 'return) $
      maybe (makePGQuery flags sql) (makeQuery flags ((sql ++) . (" RETURNING " ++))) returning
    ]
  where
  flags = simpleQueryFlags

auditInsert :: String -> [(String, String)] -> Maybe SelectOutput -> TH.ExpQ
auditInsert table args =
  auditQuery AuditActionAdd table 
    ('(' : intercalate "," (map fst args) ++ ") VALUES (" ++ intercalate "," (map snd args) ++ ")")

auditDelete :: String -> String -> Maybe SelectOutput -> TH.ExpQ
auditDelete table wher =
  auditQuery AuditActionRemove table ("WHERE " ++ wher)

auditUpdate :: String -> [(String, String)] -> String -> Maybe SelectOutput -> TH.ExpQ
auditUpdate table sets wher =
  auditQuery AuditActionChange table
    ("SET " ++ intercalate "," (map (\(c,v) -> c ++ "=" ++ v) sets) ++ " WHERE " ++ wher)
