{-# LANGUAGE RecordWildCards #-}

module Dessin.Violeta.Server
  ( runVioletaBFT
  , VioletaBFTSpec(..)
  , Config(..), otherNodes, nodeId, LightconeLamportParliamentTimeoutRange, heartbeatTimeout, enableDebug
  , Term, startTerm
  ) where

import Control.Concurrent.Chan.Unagi
import Control.Lens
import qualified Data.Set as Set

import Dessin.Violeta.Handler
import Dessin.Violeta.Types
import Dessin.Violeta.Util
import Dessin.Violeta.Timer

runVioletaBFT :: Ord nt => Config nt -> VioletaBFTSpec nt et rt mt -> IO ()
runVioletaBFT rconf spec@VioletaBFTSpec{..} = do
  let qsize = getQuorumSize $ 1 + (Set.size $ rconf ^. otherNodes)
  (ein, eout) <- newChan
  runRWS_
    vbft
    (VioletaBFTEnv rconf qsize ein eout (liftVioletaBFTSpec spec))
    initialVioletaBFTState

vbft :: Ord nt => VioletaBFT nt et rt mt ()
vbft = do
  fork_ messageReceiver
  resetLightconeLamportParliamentTimer
  handleEvents
