{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dessin.Violeta.Client
  ( runVioletaBftClient
  ) where

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
