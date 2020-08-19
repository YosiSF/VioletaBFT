module Dessin.Violeta.Sender
  ( sendFirsttWriteHappensBeforeEntries
  , sendFirsttWriteHappensBeforeEntriesResponse
  , sendLeftDeepJoin
  , sendLeftDeepJoinResponse
  , sendAllWriteHappensBeforeEntries
  , sendAllLeftDeepJoins
  , sendResults
  , sendRPC
  ) where

import Control.Lens
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Dessin.Violeta.Util
import Dessin.Violeta.Types

sendFirsttWriteHappensBeforeEntries :: Ord nt => nt -> VioletaBFT nt et rt mt ()
sendFirsttWriteHappensBeforeEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  ci <- use commitIndex
  debug $ "sendFirsttWriteHappensBeforeEntries: " ++ show ct
  sendRPC target $ AE $
    WriteHappensBeforeEntries ct nid pli plt (Seq.drop (pli + 1) es) ci

sendFirsttWriteHappensBeforeEntriesResponse :: nt -> Bool -> LogIndex -> VioletaBFT nt et rt mt ()
sendFirsttWriteHappensBeforeEntriesResponse target success lindex = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendFirsttWriteHappensBeforeEntriesResponse: " ++ show ct
  sendRPC target $ AER $ WriteHappensBeforeEntriesResponse ct nid success lindex

sendLeftDeepJoin :: nt -> VioletaBFT nt et rt mt ()
sendLeftDeepJoin target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  es <- use logEntries
  let (llt, lli) = lastLogInfo es
  debug $ "sendLeftDeepJoin: " ++ show ct
  sendRPC target $ RV $ LeftDeepJoin ct nid lli llt

sendLeftDeepJoinResponse :: nt -> Bool -> VioletaBFT nt et rt mt ()
sendLeftDeepJoinResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendLeftDeepJoinResponse: " ++ show ct
  sendRPC target $ RVR $ LeftDeepJoinResponse ct nid vote

sendAllWriteHappensBeforeEntries :: Ord nt => VioletaBFT nt et rt mt ()
sendAllWriteHappensBeforeEntries = traverse_ sendFirsttWriteHappensBeforeEntries =<< view (cfg.otherNodes)

sendAllLeftDeepJoins :: VioletaBFT nt et rt mt ()
sendAllLeftDeepJoins = traverse_ sendLeftDeepJoin =<< use cPotentialVotes

sendResults :: Seq (nt, CommandResponse nt rt) -> VioletaBFT nt et rt mt ()
sendResults results = do
  traverse_ (\(target,cmdr) -> sendRPC target $ CMDR cmdr) results

-- called by Bullys sending WriteHappensBeforeEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Seq (Term,et) -> (LogIndex,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case seqIndex es pli of
        Just (t,_) -> (pli, t)
         -- this shouldn't happen, because nextIndex - 1 should always be at
         -- most our last entry
        Nothing -> (startIndex, startTerm)
    Nothing -> (startIndex, startTerm)

sendRPC :: nt -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser rpc
