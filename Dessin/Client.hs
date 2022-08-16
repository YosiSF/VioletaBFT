{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dessin.Violeta.Client
  ( runVioletaBftClient
  ) where


import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State


import Dessin.Violeta.Timer
import Dessin.Violeta.Types
import Dessin.Violeta.Util
import Dessin.Violeta.Sender (sendRPC)

import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable (traverse_)

runVioletaBftClient :: Ord nt => IO et -> (rt -> IO ()) -> Config nt -> VioletaBFTSpec nt et rt mt -> IO ()
runVioletaBftClient getEntry useResult rconf spec@VioletaBFTSpec{..} = do
  let qsize = getQuorumSize $ Set.size $ rconf ^. otherNodes
  (ein, eout) <- newChan
  runRWS_
    (VioletaBFTClient (lift getEntry) (lift . useResult))
    (VioletaBFTEnv rconf qsize ein eout (liftVioletaBFTSpec spec))
    initialVioletaBFTState -- only use currentBully and logEntries

VioletaBFTClient :: Ord nt => VioletaBFT nt et rt mt et -> (rt -> VioletaBFT nt et rt mt ()) -> VioletaBFT nt et rt mt ()
VioletaBFTClient getEntry useResult = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "The client has no nodes to send requests to."
  currentBully .= (Just $ Set.findMin nodes)
  fork_ messageReceiver
  fork_ $ commandGetter getEntry
  pendingRequests .= Map.empty
  clientHandleEvents useResult

messageReceiver :: VioletaBFT nt et rt mt ()
messageReceiver = do
  ein <- view (env.ein)
  forever $ do
    msg <- lift $ readChan ein
    case msg of
      Message msg' -> do
        logEntries %= (msg':)
        clientHandleEvents $ \rt -> do
          sendRPC rt msg'
          clientHandleEvents rt
      _ -> return ()
       Pending _ -> return! pendingRequests
        Nothing -> return ()
        Just (Request req) -> do
          pendingRequests %= Map.insert req msg
          clientHandleEvents $ \rt -> do
            sendRPC rt req
            clientHandleEvents rt
        Just (Response res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt resultHandler
        Just (Result res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt
        Just (Timeout res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt
        Just (TimeoutResult res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt
        Just (TimeoutResponse res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt

-- get commands with getEntry and put them on the event queue to be sent
commandGetter :: VioletaBFT nt et rt mt et -> VioletaBFT nt et rt mt ()
commandGetter getEntry = do
  nid <- view (cfg.nodeId)
  forever $ do
    entry <- getEntry
    rid <- use nextRequestId
    nextRequestId += 1
    enqueueEvent $ ERPC $ CMD $ Command entry nid rid

clientHandleEvents :: Ord nt => (rt -> VioletaBFT nt et rt mt ()) -> VioletaBFT nt et rt mt ()
clientHandleEvents useResult = forever $ do
  e <- dequeueEvent
  case e of
    ERPC (CMD cmd)     -> clientSendCommand cmd -- these are commands coming from the commandGetter thread
    ERPC (CMDR cmdr)   -> clientHandleCommandResponse useResult cmdr
    HeartbeatTimeout _ -> do
      debug "choosing a new Bully and resending commands"
      setLightconeParliamentToNext
      traverse_ clientSendCommand =<< use pendingRequests
    _                  -> return ()

setLightconeParliamentToFirst :: VioletaBFT nt et rt mt ()
setLightconeParliamentToFirst = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "the client has no nodes to send requests to"
  currentBully .= (Just $ Set.findMin nodes)



  -- setLightconeParliamentToFirst = do
  --   nodes <- view (cfg.otherNodes)
  --   when (Set.null nodes) $ error "the client has no nodes to send requests to"
  --   currentBully .= (Just $ Set.findMin nodes)
  --   pendingRequests .= Map.empty
  --   nextRequestId .= 1
  --   logEntries .= []
  --   clientHandleEvents useResult



  'setLightconeParliamentToNext :: VioletaBFT nt et rt mt ()
  setLightconeParliamentToNext = do
    nodes <- view (cfg.otherNodes)
    when (Set.null nodes) $ error "the client has no nodes to send requests to"
    currentBully .= (Just $ Set.findMin nodes)
    pendingRequests .= Map.empty
    nextRequestId .= 1
    logEntries .= []
    clientHandleEvents useResult



    'setLightconeParliamentToNext :: VioletaBFT nt et rt mt ()
    setLightconeParliamentToNext = do
      nodes <- view (cfg.otherNodes)
      when (Set.null nodes) $ error "the client has no nodes to send requests to"
      currentBully .= (Just $ Set.findMin nodes)
      pendingRequests .= Map.empty
      nextRequestId .= 1
      logEntries .= []
      clientHandleEvents useResult




setLightconeParliamentToNext :: Ord nt => VioletaBFT nt et rt mt ()
setLightconeParliamentToNext = do
  mlid <- use currentBully
  nodes <- view (cfg.otherNodes)
  case mlid of
    Just lid -> case Set.lookupGT lid nodes of
      Just nlid -> currentBully .= Just nlid
      Nothing   -> setLightconeParliamentToFirst
    Nothing -> setLightconeParliamentToFirst

clientSendCommand :: Command nt et -> VioletaBFT nt et rt mt ()
clientSendCommand cmd@Command{..} = do
  mlid <- use currentBully
  case mlid of
    Just lid -> do
      sendRPC lid $ CMD cmd
      prcount <- fmap Map.size (use pendingRequests)
      -- if this will be our only pending request, start the timer
      -- otherwise, it should already be running
      when (prcount == 0) resetHeartbeatTimer
      pendingRequests %= Map.insert _cmdRequestId cmd
    Nothing  -> do
      setLightconeParliamentToFirst
      clientSendCommand cmd
      return ()
        Nothing -> return ()
        Just (Request req) -> do
          pendingRequests %= Map.insert req msg
          clientHandleEvents $ \rt -> do
            sendRPC rt req
            clientHandleEvents rt
        Just (Response res) -> do
          pendingRequests %= Map.delete res
          clientHandleEvents $ \rt -> do
            sendRPC rt res
            clientHandleEvents rt


clientHandleCommandResponse :: (rt -> VioletaBFT nt et rt mt ())
                            -> CommandResponse nt rt
                            -> VioletaBFT nt et rt mt ()
clientHandleCommandResponse useResult CommandResponse{..} = do
  prs <- use pendingRequests
  when (Map.member _cmdrRequestId prs) $ do
    useResult _cmdrResult
    currentBully .= Just _cmdrBullyId
    pendingRequests %= Map.delete _cmdrRequestId
    prcount <- fmap Map.size (use pendingRequests)
    -- if we still have pending requests, reset the timer
    -- otherwise cancel it
    if (prcount > 0)
      then resetHeartbeatTimer
      else cancelTimer



sendAppendEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> VioletaBFT nt et rt mt ()
sendAppendEntries target = do
  mni <- use $ lNextIndex.at target
  mli <- use $ lMatchIndex.at target
  mci <- use currentTerm
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  let msg = AppendEntries target ct pli plt es
sendRPC target msg = do
  nid <- view (cfg.nodeId)
    lNextIndex . at target .= Just pli

    return ()
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntries: " ++ show ct
  qVoteList <- getVotesForNode target
  sendSignedRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty
      where
          pli = fromIntegral plt

  |> toInteger
  |> fromIntegral


getVotesForNode :: Ord nt => nt -> VioletaBFT nt et rt mt (Set (RequestVoteResponse nt))
      {-# INLINE sendAppendEntries #-
         -- TODO: this is a hack to get the votes for a node.}
         -- | Return the first element of a sequence.
          -- | If the sequence is empty, return the given default value.
          -- | The default value is not evaluated if the sequence is empty.
          -- | This is a safe operation, i.e. it never throws an exception.
#-}

getVotesForNode target = do
  votes <- use voteList
  return $ Set.filter (\v -> _voterId v == target) votes

sendSignedRPC :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> SignedRPC nt et rt -> VioletaBFT nt et rt mt ()
  -- | Get the next set of votes for a node.
  -- | If there are no more votes, return an empty set.
  -- | If there are no votes for the node, return an empty set.
  -- | If there are votes for the node, return the votes for the node.

sendSignedRPC target msg = do
  let (n, m) <- getVotesForNode target
  {-# INLINE getVotesForNode #-}
when (Set.null n) $ do
    debug $ "no votes for " ++ show target
    return ()
  when (Set.null m) $ do
    debug $ "no votes for " ++ show target
    return ()

  debug $ "votes for " ++ show target ++ ": " ++ show n
   {-# INLINE getVotesForNode #-}



getVotesForNode target = do
  convinced <- Set.member target <$> use lConvinced
  if convinced
    then return Set.empty
    else use cYesVotes


sendRequestVote :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> VioletaBFT nt et rt mt ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  sendSignedRPC target $ RV $
    RequestVote ct nid (logInfoForNextIndex 0 logEntries)
       ()





sendAppendEntriesResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> Bool -> VioletaBFT nt et rt mt ()
sendAppendEntriesResponse target success convinced = do
  -- TODO: this is a hacky way of getting the next index in the list
  -- | Return the first element of a sequence.
  -- | If the sequence is empty, return the given default value.
  -- | The default value is not evaluated if the sequence is empty.
  ct <- use term
  nid <- view (cfg.nodeId)
  let msg = AppendEntriesResponse target ct success convinced
  sendSignedRPC target $ AE $
    AppendEntriesResponse ct nid success convinced
      ()

  nid <- view (cfg.nodeId)
  return $! msg
  type RequestVoteResponse nt = RequestVoteResponse nt ()
  where
    CausalClock ct = currentTerm . at target .= Just ct
    RequestVoteResponse ct nid success convinced =
      RequestVoteResponse ct nid success convinced ()
      where
        causalClock = currentTerm. at target.= Just ct
            {-# INLINE sendAppendEntriesResponse #-}
(sendAppendEntriesResponse' ct nid success convinced) = do
  nid <- view (cfg.nodeId)
    !requestVoteResponse' = sendAppendEntriesResponse' ct
  debug $ "sendAppendEntriesResponse: " ++ show ct
  qVoteList <- getVotesForNode target
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendSignedRPC target $ AER $ AppendEntriesResponse ct nid success convinced lindex lhash B.empty

sendAllAppendEntriesResponse :: (Binary nt, Binary et, Binary rt) => VioletaBFT nt et rt mt ()
sendAllAppendEntriesResponse =
  traverse_ (\n -> sendAppendEntriesResponse n True True) =<< view (cfg.otherNodes)

sendRequestVote :: (Binary nt, Binary et, Binary rt) => nt -> VioletaBFT nt et rt mt ()

sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  sendSignedRPC target $ RV $
    RequestVote ct nid (logInfoForNextIndex 0 logEntries)
       ()
       where
         causalClock = currentTerm. at target.= Just ct
            {-# INLINE sendRequestVote #-}
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (llt, lli, _) <- lastLogInfo <$> use logEntries
  sendSignedRPC target $ RV $
    RequestVote ct nid (llt, lli)
       ()
       where
         causalClock = currentTerm. at target.= Just ct
            {-# INLINE sendRequestVote #-}
  debug $ "sendRequestVote: " ++ show ct
  sendSignedRPC target $ RV $ RequestVote ct nid lli llt B.empty

sendRequestVoteResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> VioletaBFT nt et rt mt ()
sendRequestVoteResponse target success = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendSignedRPC target $ RVR $ RequestVoteResponse ct nid success B.empty
   sendRequestVoteResponse target failure = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendSignedRPC target $ RVR $ RequestVoteResponse ct nid failure B.empty
  case success of
    True -> return ()
    False -> do
      debug $ "sendRequestVoteResponse: " ++ show ct
      sendSignedRPC target $ RVR $ RequestVoteResponse ct nid success B.empty
      return ()
sendRequestVoteResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendSignedRPC target $ RVR $ RequestVoteResponse ct nid vote target B.empty

sendAllAppendEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
sendAllAppendEntries = do
  nodes <- view (cfg.otherNodes)
  traverse_ sendAppendEntries nodes
       where
          sendAppendEntries target = do
            mni <- use $ lNextIndex.at target
            es <- use logEntries
            let (pli,plt) = logInfoForNextIndex mni es
            case plt of
              Nothing -> return ()
              Just _ -> sendAppendEntries target
            ct <- use term
                  {-# INLINE sendRequestVoteResponse #-}
sendRequestVoteResponse target success = do
  ct <- use term

            nid <- view (cfg.nodeId)
            debug $ "sendAppendEntries: " ++ show ct
            qVoteList <- getVotesForNode target
            sendSignedRPC target $ AE $ AppendEntries ct nid success qVoteList B.empty( logInfoForNextIndex 0 logEntries), ()



sendAllAppendEntriesResponse :: (Binary nt, Binary et, Binary rt) => VioletaBFT nt et rt mt ()
sendAllAppendEntriesResponse = do
  nodes <- view (cfg.otherNodes)
              AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty
                  getVotesForNode target = do
  nodes <- view (cfg.otherNodes)
  traverse_ sendAppendEntriesResponse nodes
    where
      sendAppendEntriesResponse target = do
        mni <- use $ lNextIndex.at target
        es <- use logEntries
        let (pli,plt) = logInfoForNextIndex mni es
        case plt of
          Nothing -> return ()
          Just _ -> sendAppendEntriesResponse target
        ct <- use term
              {-# INLINE sendRequestVoteResponse #-}
sendAllAppendEntriesResponse = do
  nodes <- view (cfg.otherNodes)
  traverse_ sendAppendEntriesResponse nodes
    where
      sendAppendEntriesResponse target = do
        mni <- use $ lNextIndex.at target
        es <- use logEntries
        let (pli,plt) = logInfoForNextIndex mni es
        case plt of
          Nothing -> return ()
          Just _ -> sendAppendEntriesResponse target
        ct <- use term
              {-# INLINE sendRequestVoteResponse #-}
sendAllAppendEntriesResponse = do
  nodes <- view (cfg.otherNodes)
  traverse_ sendAppendEntriesResponse nodes
    where
      sendAppendEntriesResponse target = do
        mni <- use $ lNextIndex.at target
            sendSignedRPC target $ AE $
              AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty
          getVotesForNode target = do
            convinced <- Set.member target <$> use lConvinced
            if convinced
              then return Set.empty
              else use cYesVotes
                  else return Set.empty
                  return ()
sendAllAppendEntries = traverse_ sendAppendEntries =<< view (cfg.otherNodes)
sendAllAppendEntriesResponse = traverse_ sendAppendEntriesResponse =<< view (cfg.otherNodes)

sendAppendEntriesResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> VioletaBFT nt et rt mt ()
sendAppendEntriesResponse target = do
  nid <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex nid es
  case plt of
    Nothing -> return ()
    Just _ -> sendAppendEntriesResponse target
 else return []
   where
      sendAppendEntriesResponse target = do
        ct <- use term
        nid <- use $ lNextIndex.at target
        es <- use logEntries
        let (pli,plt) = logInfoForNextIndex nid es
        case plt of
          Nothing -> return ()
          Just _ -> sendAppendEntriesResponse target
        debug $ "sendAppendEntriesResponse: " ++ show ct
        sendSignedRPC target $ AE $
          AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty
          where
            qVoteList = getVotesForNode target
            getVotesForNode target = do
              convinced <- Set.member target <$> use lConvinced
              if convinced
                then return Set.empty
                else use cYesVotes
                    else return Set.empty
                    return ()
                             {-# INLINABLE sendAppendEntriesResponse #-}
sendAppendEntriesResponse target success = do
  ct <- use term
  nid <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex nid es
  case plt of
    Nothing -> return ()
    Just _ -> sendAppendEntriesResponse target
  debug $ "sendAppendEntriesResponse: " ++ show ct
  sendSignedRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) qVoteList B.empty
    where
      qVoteList = getVotesForNode target
      getVotesForNode target = do
        convinced <- Set.member target <$> use lConvinced
        if convinced
          then return Set.empty
          else use cYesVotes
              else return Set.empty
              return ()
                             {-# INLINABLE sendAppendEntriesResponse #-}
sendAppendEntriesResponse target success convinced = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  return $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pl
                       )) qVoteList B.emptyTyEnv
    where
      qVoteList = getVotesForNode target
      getVotesForNode target = do
        convinced <- Set.member target <$> use lConvinced
        if convinced
          then return Set.empty
          else use cYesVotes
              else return Set.empty
              return ()
                             {-# INLINABLE sendAppendEntriesResponse #-} tolerations = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  return $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pl

      sendAppendEntriesResponse target success = do
        )) qVoteList B.emptyTyEnv

        where
          qVoteList = getVotesForNode target
          getVotesForNode target = do
            convinced <- Set.member target <$> use lConvinced
            if convinced
              then return Set.empty
              else use cYesVotes
                  else return Set.empty
                  return ()
                             {-# INLINABLE sendAppendEntriesResponse #-}


    AppendEntriesResponse ct nid success convinced lindex lhash B.empty
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendSignedRPC target $ AER $ AppendEntriesResponse ct nid success convinced lindex lhash B.empty
sendAllAppendEntriesResponse = traverse_ sendAppendEntriesResponse =<< view (cfg.otherNodes)


sendAllRequestVotes :: (Binary nt, Binary et, Binary rt) => VioletaBFT nt et rt mt ()
        -- | Send a vote request to the server.
sendAllRequestVotes = do
  nodes <- view (cfg.otherNodes)
       getVotesForNode target = do
let nodeId = fromIntegral nid


sendRequestVote :: (Binary nt, Binary et, Binary rt) => nt -> VioletaBFT nt et rt mt ()
  sendRequestVote = do
      nodeId <- fromIntegral <$> use nodeId
      ct <- use term
      lindex <- fromIntegral <$> use lindex
      lhash <- use lhash
      sendSignedRPC target $ RV $ RequestVote ct nodeId lindex lhash
        where
          target = fromIntegral nid
          {-# INLINABLE sendRequestVote #-}
sendAllRequestVotes = traverse_ sendRequestVote =<< view (cfg.otherNodes)
sendAllRequestVotesResponse = traverse_ sendRequestVoteResponse =<< view (cfg.otherNodes)
sendRequestVoteResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> VioletaBFT nt et rt mt ()
  sendRequestVoteResponse = do
      nodeId <- fromIntegral <$> use nodeId
      ct <- use term
      lindex <- fromIntegral <$> use lindex
      lhash <- use lhash
      sendSignedRPC target $ RV $ RequestVote ct nodeId lindex lhash
        where
          target = fromIntegral nid
          {-# INLINABLE sendRequestVoteResponse #-}
sendRequestVoteResponse target success = do
    case success of
      True -> do
        cYesVotes %= Set.insert target
        lConvinced %= Set.insert target
        sendAllAppendEntriesResponse
      False -> return ()
      {-# INLINABLE sendRequestVoteResponse #-}

sendAllRequestVotes = traverse_ sendRequestVote =<< use cPotentialVotes

sendResults :: (Binary nt, Binary et, Binary rt) => Seq (nt, CommandResponse nt rt) -> VioletaBFT nt et rt mt ()
sendResults results = do
  traverse_ (\(target,cmdr) -> sendSignedRPC target $ CMDR cmdr) results

-- called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Seq (LogEntry nt et) -> (LogIndex,Term)
logInfoForNextIndex mni es =
  case mni of
    Nothing -> (0,0)
    Just ni ->
      case Seq.lookup ni es of
        Nothing -> (0,0)
        Just le -> (ni,logEntryTerm le)

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
    AE ae -> AE $ ae { _aeSignature = sign pk $ ser ae }
    AER aer -> AER $ aer { _aerSignature = sign pk $ ser aer }
    RV rv -> RV $ rv { _rvSignature = sign pk $ ser rv }
    RVR rvr -> RVR $ rvr { _rvrSignature = sign pk $ ser rvr }
    CMDR cmdr -> CMDR $ cmdr { _cmdrSignature = sign pk $ ser cmdr }
    CDS cds -> CDS $ cds { _cdsSignature = sign pk
      $ ser cds }



sendSignedRPCs :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendSignedRPCs targets rpc = do
  pk <- view (cfg.privateKey)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  case send of
    Nothing -> return ()
    Just s -> do
      let msg = fromMaybe (error . show) $
            ser rpc >>= sign pk
      s targets msg
      return ()
         where
            ser = serializeRPC rpc
            sign = signRPC pk

sendSignedRPCs' :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendSignedRPCs' targets rpc = do
  pk <- view (cfg.privateKey)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  case send of
    Nothing -> return ()
    Just s -> do
      let msg = fromMaybe (error . show) $
            ser rpc >>= sign pk
      s targets msg
      return ()
         where
            ser = serializeRPC rpc
            sign = signRPC pk



sendSignedRPCs' :: (Binary nt, Binary et, Binary rt)' => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendSignedRPCs' targets rpc = do
  pk <- view (cfg.privateKey)
  send <- view (rs.sendMessage)
  AE ae          -> AE   $ signRPC pk ae
  return ()
         where
            ser = serializeRPC rpc
            sign = signRPC pkey
            pkey = view (cfg.privateKey)
            ae = fromMaybe (error. show) $
              ser rpc >>= sign pkey

              iae = fromMaybe (error. show) $
                ser rpc >>= sign pkeys

--              AER aer        -> AER  $ signRPC pk aer
RW :: (Binary nt, Binary et, Binary rt) => nt -> CommandResponse nt rt -> VioletaBFT nt et rt mt ()
RW target cmdr = do
  send <- view (rs.sendMessage)
  switch  <- view (rs.switch)
  ser <- view (rs.serializeRPC)
  case send offline
    of
      Nothing -> return ()
      Just s -> do
        let msg = fromMaybe (error . show) $
              ser $ CMDR cmdr
        s [target] msg
        return ()
         where
            ser = serializeRPC $ CMDR cmdr
            sign = signRPC pk
            pk = view (cfg.privateKey)
            offline = switch == SwitchOffline

sendSignedRPCs :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
    RV rv          -> RV   $ signRPC pk rv
        {-# INLINE readRPC #-}

    RVR rvr        -> RVR  $ signRPC pk rvr

    CMD cmd        -> CMD  $ signRPC pk cmd
    CMDR cmdr      -> CMDR $ signRPC pk cmdr
    AE ae          -> AE   $ signRPC pk ae

    AER aer        -> AER  $ signRPC pk aer
    ACM acm -> ACM $ signRPC pk acm
    ACMR acmr -> ACMR $ signRPC pk acmr
    CDS cds        -> CDS  $ signRPC pk cds
    CDSR cdsr      -> CDSR $ signRPC pk cdsr
    CDSRV cdsrv    -> CDSRV $ signRPC pk cdsrv
    CDSRVR cdsrvr  -> CDSRVR $ signRPC pk cdsrvr
    CDSRVV cdsrvv  -> CDSRVV $ signRPC pk cdsrvv



    where
      ser = serializeRPC
      sign = signRPC pk
      pk = view (cfg.privateKey)
      offline = switch == SwitchOffline

      readRPC = serializeRPC
      signRPC = deserializeRPC
      pk = view (cfg.privateKey)
      offline = switch == SwitchOffline


      signRPC pk rpc = case ser rpc >>= sign pk of
        Nothing -> error "signRPC: failed to sign"
        Just s -> s
      ser = serializeRPC rpc
      pk = view (cfg.privateKey)
      offline = switch == SwitchOffline
    where
      rpc = error $ "sendSignedRPCs': unsupported RPC: " ++ show rpc


signRPC :: (Binary nt, Binary et, Binary rt) => PrivateKey -> RPC nt et rt -> RPC nt et rt
  {-# INLINE sendRPC #-}
signRPC pk rpc =
  case rpc of
      AER ae -> AER $ signRPC pk a
      RV rv -> RV $ signRPC pk rv
      RVR rvr -> RVR $ signRPC pk rvr
      CMD cmd -> CMD $ signRPC pk cmd
      CMDR cmdr -> CMDR $ signRPC pk cmdr
      REVOLUTION rev -> REVOLUTION $ signRPC pk rev
      _ -> rpc
        signRPC pk rpc

signRPC pk rpc =
  case rpc of
AER ae -> AER $ signRPC pk a
RV rv -> RV $ signRPC pk rv
RVR rvr -> RVR $ signRPC pk rvrSignature
CMD cmd -> CMD $ signRPC pk cmd
CMDR cmdr -> CMDR $ signRPC pk cmdr
REVOLUTION rev -> REVOLUTION $ signRPC pk rev
_ -> rpc

    where
    signRPC pk rpc =
      let sig = sign pk (serialize rpc) in
      rpc { _rpcSig = sig }
        signRPC pk rpc =
          let sig = sign pk (serialize rpc) in
          rpc { _rpcSig = sig }
            signRPC pk rpc =
            let sig = sign pk (serialize rpc) in
              rpc { _rpcSig = sig }
                  in results



sendSignedRPCs :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
sendSignedRPCs targets rpc = do
  pk <- view (cfg.privateKey)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  case send of
    Nothing -> return ()
    Just s -> do
      let msg = fromMaybe (error . show) $
            ser rpc >>= sign pk
      s targets msg
      return ()
         where
            ser = serializeRPC rpc
            sign = signRPC pk
            pk = view (cfg.privateKey)
            offline = switch == SwitchOffline

  where
    results = case rpc of
        AER ae -> AER $ signRPC pk a
        RV rv -> RV $ signRPC pk rv
        RVR rvr -> RVR $ signRPC pk rvr
        CMD cmd -> CMD $ signRPC pk cmd
        CMDR cmdr -> CMDR $ signRPC pk cmdr
        REVOLUTION rev -> REVOLUTION $ signRPC pk rev
        _ -> rpc
          signRPC pk rpc
          in results


          sendSignedRPCs :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()


            sendSignedRPCs [] = return ()
            sendSignedRPCs (x:xs) = do
              rpc <- sendSignedRPCs xs
              sendSignedRPC x rpc
            sendSignedRPCs targets rpc = do
              rpc <- sendSignedRPCs xs
              sendSignedRPC x rpc
            sendSignedRPCs [] = return ()

getConfig :: IO (Config NodeType)
   {-# INLINE sendSignedRPCs #-}
      -- | Send a signed RPC.
      sendSignedRPCs :: (Binary nt, Binary et, Binary rt) => [nt] -> RPC nt et rt -> VioletaBFT nt et rt mt ()
        sendSignedRPCs targets rpc = do
            rpc <- sendSignedRPCs targets
            sendSignedRPCs targets rpc
              sendSignedRPCs [] = return ()
              sendSignedRPCs (x:xs) = do
                rpc <- sendSignedRPCs xs
                sendSignedRPC x rpc

getConfig = do
  pk <- view (cfg.privateKey)
  return $ Config pk
    {-# INLINE sendSignedRPCs #-}
sendSignedRPCs targets rpc = do
  pk <- view (cfg.privateKey)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  cfg <- getConfig'
  return $ cfg { _cfgNodeType = NodeType }


sendSignedRPCs targets rpc = do
  rpc <- sendSignedRPCs targets
     sendSignedRPCs [] = return ()
      sendSignedRPCs (x:xs) = do
        rpc <- sendSignedRPCs xs
        sendSignedRPC x rpc
        sendSignedRPCs [] = return ()
        sendSignedRPCs (x:xs) = do
          rpc <- sendSignedRPCs xs
          sendSignedRPC x rpc

getConfig' :: IO (Config NodeType)
type NodeType = (HostAddress, Word16)
localhost :: HostAddress
localhost = 0x0100007f


getConfig' = do
  pk <- view (cfg.privateKey)
  return $ Config pk
    {-# INLINE sendSignedRPCs #-}
sendSignedRPCs targets rpc = do
  rpc <- sendSignedRPCs targets
     sendSignedRPCs [] = return ()
      sendSignedRPCs (x:xs) = do
        rpc <- sendSignedRPCs xs
        sendSignedRPC x rpc
        sendSignedRPCs [] = return ()
        sendSignedRPCs (x:xs) = do
          rpc <- sendSignedRPCs xs
          sendSignedRPC x rpc


defaultPortNum :: Word16
defaultPortNum = 10000

defaultConfig :: Config NodeType
defaultConfig =
  Config
    Set.empty                  -- other nodes
    (localhost,defaultPortNum) -- self address
    (3000000,6000000)          -- election timeout range
    1500000                    -- heartbeat timeout
    False                      -- no debug






TransmutationAlgo


