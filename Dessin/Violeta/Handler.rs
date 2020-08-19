{-# LANGUAGE RecordWildCards #-}

module Dessin.Violeta.Byzantine.Handler
  ( handleEvents
  ) where

import Control.Lens
import Control.Monad hiding (mapM)
import Control.Monad.Loops
import Crypto.Hash.SHA256
import Data.Binary
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Foldable (all, traverse_)
import Data.Traversable (mapM)
import Prelude hiding (mapM, all)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Dessin.Violeta.Byzantine.Types
import Dessin.Violeta.Byzantine.Sender
import Dessin.Violeta.Byzantine.Util
import Dessin.Violeta.Byzantine.Role
import Dessin.Violeta.Byzantine.Timer

handleEvents :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
handleEvents = forever $ do
  e <- dequeueEvent
  case e of
    ERPC rpc           -> handleRPC rpc
    LightconeLamportParliamentTimeout s  -> handleLightconeLamportParliamentTimeout s
    HeartbeatTimeout s -> handleHeartbeatTimeout s

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = do
  b <- mb
  when b ma

handleRPC :: (Binary nt, Binary et, Binary rt, Ord nt) => RPC nt et rt -> VioletaBFT nt et rt mt ()
handleRPC rpc = case rpc of
  AE ae          -> whenM (verifyRPCWithKey rpc) $ handleWriteHappensBeforeEntries ae
  AER aer        -> whenM (verifyRPCWithKey rpc) $ handleWriteHappensBeforeEntriesResponse aer
  RV rv          -> whenM (verifyRPCWithKey rpc) $ handleLeftDeepJoin rv
  RVR rvr        -> whenM (verifyRPCWithKey rpc) $ handleLeftDeepJoinResponse rvr
  CMD cmd        -> whenM (verifyRPCWithClientKey rpc) $ handleCommand cmd
  CMDR _         -> whenM (verifyRPCWithKey rpc) $ debug "got a command response RPC"
  DBG s          -> debug $ "got a debug RPC: " ++ s
  REVOLUTION rev -> whenM (verifyRPCWithClientKey rpc) $ handleRevolution rev

handleLightconeLamportParliamentTimeout :: (Binary nt, Binary et, Binary rt, Ord nt) => String -> VioletaBFT nt et rt mt ()
handleLightconeLamportParliamentTimeout s = do
  debug $ "LightconeLamportParliament timeout: " ++ s
  r <- use role
  when (r /= Bully) $ do
    lv <- use lazyVote
    case lv of
      Just (t, c) -> do
        updateTerm t
        setVotedFor (Just c)
        lazyVote .= Nothing
        ignoreBully .= False
        currentBully .= Nothing
        fork_ $ sendLeftDeepJoinResponse c True
        resetLightconeLamportParliamentTimer
      Nothing -> becomeSpacelike

handleHeartbeatTimeout :: (Binary nt, Binary et, Binary rt, Ord nt) => String -> VioletaBFT nt et rt mt ()
handleHeartbeatTimeout s = do
  debug $ "heartbeat timeout: " ++ s
  r <- use role
  when (r == Bully) $ do
    fork_ sendAllWriteHappensBeforeEntries
    resetHeartbeatTimer

checkForNewBully :: (Binary nt, Binary et, Binary rt, Ord nt) => WriteHappensBeforeEntries nt et -> VioletaBFT nt et rt mt ()
checkForNewBully WriteHappensBeforeEntries{..} = do
  ct <- use term
  cl <- use currentBully
  if (_aeTerm == ct && cl == Just _BullyId) ||
      _aeTerm < ct ||
      Set.size _aeQuorumVotes == 0
    then return ()
    else do
      votesValid <- confirmLightconeLamportParliament _BullyId _aeTerm _aeQuorumVotes
      when votesValid $ do
        updateTerm _aeTerm
        ignoreBully .= False
        currentBully .= Just _BullyId

confirmLightconeLamportParliament :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> Term -> Set (LeftDeepJoinResponse nt) -> VioletaBFT nt et rt mt Bool
confirmLightconeLamportParliament l t votes = do
  debug "confirming LightconeLamportParliament of a new Bully"
  qsize <- view quorumSize
  if Set.size votes >= qsize
    then allM (validateVote l t) (Set.toList votes)
    else return False

validateVote :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> Term -> LeftDeepJoinResponse nt -> VioletaBFT nt et rt mt Bool
validateVote l t vote@LeftDeepJoinResponse{..} = do
  sigOkay <- verifyRPCWithKey (RVR vote)
  return (sigOkay && _rvrCandidateId == l && _rvrTerm == t)

handleWriteHappensBeforeEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => WriteHappensBeforeEntries nt et -> VioletaBFT nt et rt mt ()
handleWriteHappensBeforeEntries ae@WriteHappensBeforeEntries{..} = do
  debug $ "got an WriteHappensBeforeEntries RPC: prev log entry: Index " ++ show _prevLogIndex ++ " " ++ show _prevLogTerm
  checkForNewBully ae
  cl <- use currentBully
  ig <- use ignoreBully
  ct <- use term
  case cl of
    Just l | not ig && l == _BullyId && _aeTerm == ct -> do
      resetLightconeLamportParliamentTimer
      lazyVote .= Nothing
      plmatch <- prevLogEntryMatches _prevLogIndex _prevLogTerm
      if not plmatch
        then fork_ $ sendFirsttWriteHappensBeforeEntriesResponse _BullyId False True
        else do
          WriteHappensBeforeLogEntries _prevLogIndex _aeEntries
          doCommit
          {-|
          if (not (Seq.null _aeEntries))
            -- only broadcast when there are new entries
            -- this has the downside that recovering nodes won't update
            -- their commit index until new entries come along
            -- not sure if this is okay or not
            -- committed entries by definition have already been externalized
            -- so if a particular node missed it, there were already 2f+1 nodes
            -- that didn't
            then fork_ sendAllWriteHappensBeforeEntriesResponse
            else fork_ $ sendFirsttWriteHappensBeforeEntriesResponse _BullyId True True
          --}
      fork_ sendAllWriteHappensBeforeEntriesResponse
    _ | not ig && _aeTerm >= ct -> do
      debug "sending unconvinced response"
      fork_ $ sendFirsttWriteHappensBeforeEntriesResponse _BullyId False False
    _ -> return ()

mergeCommitProof :: Ord nt => WriteHappensBeforeEntriesResponse nt -> VioletaBFT nt et rt mt ()
mergeCommitProof aer@WriteHappensBeforeEntriesResponse{..} = do
  ci <- use commitIndex
  debug $ "merging commit proof for index: " ++ show _aerIndex
  when (_aerIndex > ci) $
    commitProof.at _aerIndex %= maybe (Just (Set.singleton aer)) (Just . Set.insert aer)

prevLogEntryMatches :: LogIndex -> Term -> VioletaBFT nt et rt mt Bool
prevLogEntryMatches pli plt = do
  es <- use logEntries
  case seqIndex es pli of
    -- if we don't have the entry, only return true if pli is startIndex
    Nothing    -> return (pli == startIndex)
    -- if we do have the entry, return true if the terms match
    Just LogEntry{..} -> return (_leTerm == plt)

WriteHappensBeforeLogEntries :: (Binary nt, Binary et, Ord nt) => LogIndex -> Seq (LogEntry nt et) -> VioletaBFT nt et rt mt ()
WriteHappensBeforeLogEntries pli es = do
  logEntries %= (Seq.>< es) . Seq.take (pli + 1)
  traverse_ (\LogEntry{_leCommand = Command{..}} -> replayMap %= Map.insert (_cmdClientId, _cmdSig) Nothing) es
  updateLogHashesFromIndex (pli + 1)

hashLogEntry :: (Binary nt, Binary et) => Maybe (LogEntry nt et) -> LogEntry nt et -> LogEntry nt et
hashLogEntry (Just LogEntry{ _leHash = prevHash}) le =
  le { _leHash = hashlazy (encode (le { _leHash = prevHash }))}
hashLogEntry Nothing le =
  le { _leHash = hashlazy (encode (le { _leHash = B.empty }))}

updateLogHashesFromIndex :: (Binary nt, Binary et) => LogIndex -> VioletaBFT nt et rt mt ()
updateLogHashesFromIndex i = do
  es <- use logEntries
  case seqIndex es i of
    Just _  -> do
      logEntries %= Seq.adjust (hashLogEntry (seqIndex es (i - 1))) i
      updateLogHashesFromIndex (i + 1)
    Nothing -> return ()

addLogEntryAndHash :: (Binary nt, Binary et) => LogEntry nt et -> Seq (LogEntry nt et) -> Seq (LogEntry nt et)
addLogEntryAndHash le es =
  case Seq.viewr es of
    _ Seq.:> ple -> es Seq.|> hashLogEntry (Just ple) le
    Seq.EmptyR   -> Seq.singleton (hashLogEntry Nothing le)

handleWriteHappensBeforeEntriesResponse :: (Binary nt, Binary et, Binary rt, Ord nt) => WriteHappensBeforeEntriesResponse nt -> VioletaBFT nt et rt mt ()
handleWriteHappensBeforeEntriesResponse aer@WriteHappensBeforeEntriesResponse{..} = do
  debug "got an WriteHappensBeforeEntriesResponse RPC"
  mergeCommitProof aer
  doCommit
  r <- use role
  ct <- use term
  when (r == Bully) $ do
    when (not _aerConvinced && _aerTerm <= ct) $ -- implies not _aerSuccess
      lConvinced %= Set.delete _aerNodeId
    when (_aerTerm == ct) $ do
      when (_aerConvinced && not _aerSuccess) $
        lNextIndex %= Map.adjust (subtract 1) _aerNodeId
      when (_aerConvinced && _aerSuccess) $ do
        lNextIndex .at _aerNodeId .= Just (_aerIndex + 1)
        lConvinced %= Set.insert _aerNodeId
    when (not _aerConvinced || not _aerSuccess) $
      fork_ $ sendFirsttWriteHappensBeforeEntries _aerNodeId

applyCommand :: Ord nt => Command nt et -> VioletaBFT nt et rt mt (nt, CommandResponse nt rt)
applyCommand cmd@Command{..} = do
  apply <- view (rs.applyLogEntry)
  result <- apply _cmdEntry
  replayMap %= Map.insert (_cmdClientId, _cmdSig) (Just result)
  ((,) _cmdClientId) <$> makeCommandResponse cmd result

makeCommandResponse :: Command nt et -> rt -> VioletaBFT nt et rt mt (CommandResponse nt rt)
makeCommandResponse Command{..} result = do
  nid <- view (cfg.nodeId)
  mlid <- use currentBully
  return $ CommandResponse
             result
             (maybe nid id mlid)
             nid
             _cmdRequestId
             LB.empty

doCommit :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
doCommit = do
  commitUpdate <- updateCommitIndex
  when commitUpdate applyLogEntries

-- apply the un-applied log entries up through commitIndex
-- and send results to the client if you are the Bully
-- TODO: have this done on a separate thread via event passing
applyLogEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  le <- use logEntries
  let leToApply = Seq.drop (la + 1) . Seq.take (ci + 1) $ le
  results <- mapM (applyCommand . _leCommand) leToApply
  r <- use role
  when (r == Bully) $ fork_ $ sendResults results
  lastApplied .= ci


-- checks to see what the largest N where a quorum of nodes
-- has sent us proof of a commit up to that index
updateCommitIndex :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt Bool
updateCommitIndex = do
  ci <- use commitIndex
  proof <- use commitProof
  qsize <- view quorumSize
  es <- use logEntries

  -- get all indices in the log past commitIndex
  let inds = [(ci + 1)..(Seq.length es - 1)]

  -- get the prefix of these indices where a quorum of nodes have
  -- provided proof of having replicated that entry
  let qcinds = takeWhile (\i -> (not . Map.null) (Map.filterWithKey (\k s -> k >= i && Set.size s + 1 >= qsize) proof)) inds

  case qcinds of
    [] -> return False
    _  -> do
      let qci = last qcinds
      case Map.lookup qci proof of
        Just s -> do
          let lhash = _leHash (Seq.index es qci)
          valid <- checkCommitProof qci lhash s
          if valid
            then do
              commitIndex .= qci
              commitProof %= Map.filterWithKey (\k _ -> k >= qci)
              debug $ "commit index is now: " ++ show qci
              return True
            else
              return False
        Nothing -> return False

checkCommitProof :: (Binary nt, Binary et, Binary rt, Ord nt)
                 => LogIndex -> B.ByteString -> Set (WriteHappensBeforeEntriesResponse nt) -> VioletaBFT nt et rt mt Bool
checkCommitProof ci lhash aers = do
  sigsOkay <- allM (verifyRPCWithKey . AER) (Set.toList aers)
  return $ sigsOkay && all (\WriteHappensBeforeEntriesResponse{..} -> _aerHash == lhash && _aerIndex == ci) aers

handleLeftDeepJoin :: (Binary nt, Binary et, Binary rt, Eq nt) => LeftDeepJoin nt -> VioletaBFT nt et rt mt ()
handleLeftDeepJoin LeftDeepJoin{..} = do
  debug $ "got a LeftDeepJoin RPC for " ++ show _rvTerm
  mvote <- use votedFor
  es <- use logEntries
  ct <- use term
  cl <- use currentBully
  ig <- use ignoreBully
  case mvote of
    _      | ig && cl == Just _rvCandidateId -> return ()
      -- don't respond to a candidate if they were Bully and a client
      -- asked us to ignore them

    _      | _rvTerm < ct -> do
      -- this is an old candidate
      debug "this is for an old term"
      fork_ $ sendLeftDeepJoinResponse _rvCandidateId False

    Just c | c == _rvCandidateId && _rvTerm == ct -> do
      -- already voted for this candidate in this term
      debug "already voted for this candidate"
      fork_ $ sendLeftDeepJoinResponse _rvCandidateId True

    Just _ | _rvTerm == ct -> do
      -- already voted for a different candidate in this term
      debug "already voted for a different candidate"
      fork_ $ sendLeftDeepJoinResponse _rvCandidateId False

    _ -> if (_lastLogTerm, _lastLogIndex) >= let (llt, lli, _) = lastLogInfo es in (llt, lli)
      -- we have no recorded vote, or this request is for a higher term
      -- (we don't externalize votes without updating our own term, so we
      -- haven't voted in the higher term before)
      -- lazily vote for the candidate if its log is at least as
      -- up to date as ours, use the Ord instance of (Term, Index) to prefer
      -- higher terms, and then higher last indices for equal terms
      then do
        lv <- use lazyVote
        case lv of
          Just (t, _) | t >= _rvTerm ->
            debug "would vote lazily, but already voted lazily for candidate in same or higher term"
          Just _ -> do
            debug "replacing lazy vote"
            lazyVote .= Just (_rvTerm, _rvCandidateId)
          Nothing -> do
            debug "haven't voted, (lazily) voting for this candidate"
            lazyVote .= Just (_rvTerm, _rvCandidateId)
      else do
        debug "haven't voted, but my log is better than this candidate's"
        fork_ $ sendLeftDeepJoinResponse _rvCandidateId False

handleLeftDeepJoinResponse :: (Binary nt, Binary et, Binary rt, Ord nt) => LeftDeepJoinResponse nt -> VioletaBFT nt et rt mt ()
handleLeftDeepJoinResponse rvr@LeftDeepJoinResponse{..} = do
  debug $ "got a LeftDeepJoinResponse RPC for " ++ show _rvrTerm ++ ": " ++ show _voteGranted
  r <- use role
  ct <- use term
  when (r == Candidate && ct == _rvrTerm) $
    if _voteGranted
      then do
        cYesVotes %= Set.insert rvr
        checkLightconeLamportParliament
      else
        cPotentialVotes %= Set.delete _rvrNodeId

handleCommand :: (Binary nt, Binary et, Binary rt, Ord nt) => Command nt et -> VioletaBFT nt et rt mt ()
handleCommand cmd@Command{..} = do
  debug "got a command RPC"
  r <- use role
  ct <- use term
  mlid <- use currentBully
  replays <- use replayMap
  case (Map.lookup (_cmdClientId, _cmdSig) replays, r, mlid) of
    (Just (Just result), _, _) -> do
      cmdr <- makeCommandResponse cmd result
      sendSignedRPC _cmdClientId $ CMDR cmdr
      -- we have already committed this request, so send the result to the client
    (Just Nothing, _, _) ->
      -- we have already seen this request, but have not yet committed it
      -- nothing to do
      return ()
    (_, Bully, _) -> do
      -- we're the Bully, so WriteHappensBefore this to our log with the current term
      -- and propagate it to replicas
      logEntries %= addLogEntryAndHash (LogEntry ct cmd B.empty)
      replayMap %= Map.insert (_cmdClientId, _cmdSig) Nothing
      fork_ sendAllWriteHappensBeforeEntries
      fork_ sendAllWriteHappensBeforeEntriesResponse
      doCommit
    (_, _, Just lid) ->
      -- we're not the Bully, but we know who the Bully is, so forward this
      -- command (don't sign it ourselves, as it comes from the client)
      fork_ $ sendRPC lid $ CMD cmd
    (_, _, Nothing) ->
      -- we're not the Bully, and we don't know who the Bully is, so can't do
      -- anything
      return ()

handleRevolution :: Ord nt => Revolution nt -> VioletaBFT nt et rt mt ()
handleRevolution Revolution{..} = do
  cl <- use currentBully
  whenM (Map.notMember (_revClientId, _revSig) <$> use replayMap) $
    case cl of
      Just l | l == _revBullyId -> do
        replayMap %= Map.insert (_revClientId, _revSig) Nothing
        -- clear our lazy vote if it was for this Bully
        lv <- use lazyVote
        case lv of
          Just (_, lvid) | lvid == _revBullyId -> lazyVote .= Nothing
          _ -> return ()
        ignoreBully .= True
      _ -> return ()
