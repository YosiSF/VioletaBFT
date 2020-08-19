{-# LANGUAGE RecordWildCards #-}

module Dessin.Violeta.Byzantine.Sender
  ( sendFirsttWriteHappensBeforeEntries
  , sendFirsttWriteHappensBeforeEntriesResponse
  , sendLeftDeepJoin
  , sendLeftDeepJoinResponse
  , sendAllWriteHappensBeforeEntries
  , sendAllLeftDeepJoins
  , sendAllWriteHappensBeforeEntriesResponse
  , sendResults
  , sendRPC
  , sendSignedRPC
  ) where

import Control.Lens
import Data.Binary
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Dessin.Violeta.Byzantine.Util
import Dessin.Violeta.Byzantine.Types

sendFirsttWriteHappensBeforeEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> VioletaBFT nt et rt mt ()
sendFirsttWriteHappensBeforeEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendFirsttWriteHappensBeforeEntries: " ++ show ct
  qVoteList <- getVotesForNode target
  sendSignedRPC target $ AE $
    WriteHappensBeforeEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty

getVotesForNode :: Ord nt => nt -> VioletaBFT nt et rt mt (Set (LeftDeepJoinResponse nt))
getVotesForNode target = do
  convinced <- Set.member target <$> use lConvinced
  if convinced
    then return Set.empty
    else use cYesVotes

sendFirsttWriteHappensBeforeEntriesResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> Bool -> VioletaBFT nt et rt mt ()
sendFirsttWriteHappensBeforeEntriesResponse target success convinced = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendFirsttWriteHappensBeforeEntriesResponse: " ++ show ct
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendSignedRPC target $ AER $ WriteHappensBeforeEntriesResponse ct nid success convinced lindex lhash B.empty

sendAllWriteHappensBeforeEntriesResponse :: (Binary nt, Binary et, Binary rt) => VioletaBFT nt et rt mt ()
sendAllWriteHappensBeforeEntriesResponse =
  traverse_ (\n -> sendFirsttWriteHappensBeforeEntriesResponse n True True) =<< view (cfg.otherNodes)

sendLeftDeepJoin :: (Binary nt, Binary et, Binary rt) => nt -> VioletaBFT nt et rt mt ()
sendLeftDeepJoin target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (llt, lli, _) <- lastLogInfo <$> use logEntries
  debug $ "sendLeftDeepJoin: " ++ show ct
  sendSignedRPC target $ RV $ LeftDeepJoin ct nid lli llt B.empty

sendLeftDeepJoinResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> VioletaBFT nt et rt mt ()
sendLeftDeepJoinResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendLeftDeepJoinResponse: " ++ show ct
  sendSignedRPC target $ RVR $ LeftDeepJoinResponse ct nid vote target B.empty

sendAllWriteHappensBeforeEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
sendAllWriteHappensBeforeEntries = traverse_ sendFirsttWriteHappensBeforeEntries =<< view (cfg.otherNodes)

sendAllLeftDeepJoins :: (Binary nt, Binary et, Binary rt) => VioletaBFT nt et rt mt ()
sendAllLeftDeepJoins = traverse_ sendLeftDeepJoin =<< use cPotentialVotes

sendResults :: (Binary nt, Binary et, Binary rt) => Seq (nt, CommandResponse nt rt) -> VioletaBFT nt et rt mt ()
sendResults results = do
  traverse_ (\(target,cmdr) -> sendSignedRPC target $ CMDR cmdr) results

-- called by Bullys sending WriteHappensBeforeEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Seq (LogEntry nt et) -> (LogIndex,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case seqIndex es pli of
        Just LogEntry{..} -> (pli, _leTerm)
         -- this shouldn't happen, because nextIndex - 1 should always be at
         -- most our last entry
        Nothing -> (startIndex, startTerm)
    Nothing -> (startIndex, startTerm)

sendRPC :: nt -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser rpc

sendSignedRPC :: (Binary nt, Binary et, Binary rt) => nt -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendSignedRPC target rpc = do
  pk <- view (cfg.privateKey)
  sendRPC target $ case rpc of
    AE ae          -> AE   $ signRPC pk ae
    AER aer        -> AER  $ signRPC pk aer
    RV rv          -> RV   $ signRPC pk rv
    RVR rvr        -> RVR  $ signRPC pk rvr
    CMD cmd        -> CMD  $ signRPC pk cmd
    CMDR cmdr      -> CMDR $ signRPC pk cmdr
    REVOLUTION rev -> REVOLUTION $ signRPC pk rev
    _         -> rpc
