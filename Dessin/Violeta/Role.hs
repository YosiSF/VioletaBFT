module Dessin.Violeta.Byzantine.Role
  ( becomeLightlike
  , becomeTimelike
  , becomeSpacelike
  , checkLightconeLamportParliament
  , setVotedFor
  ) where

import Control.Lens hiding (Index)
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Dessin.Violeta.Byzantine.Timer
import Dessin.Violeta.Byzantine.Types
import Dessin.Violeta.Byzantine.Util
import Dessin.Violeta.Byzantine.Sender
import Dessin.Violeta.Combinator

-- count the yes votes and become Bully if you have reached a quorum
checkLightconeLamportParliament :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
checkLightconeLamportParliament = do
  nyes <- Set.size <$> use cYesVotes
  qsize <- view quorumSize
  debug $ "yes votes: " ++ show nyes ++ " quorum size: " ++ show qsize
  when (nyes >= qsize) $ becomeTimelike

setVotedFor :: Maybe nt -> VioletaBFT nt et rt mt ()
setVotedFor mvote = do
  _ <- rs.writeVotedFor ^$ mvote
  votedFor .= mvote

becomeLightlike :: VioletaBFT nt et rt mt ()
becomeLightlike = do
  debug "becoming lightlike"
  role .= Lightlike
  resetLightconeLamportParliamentTimer

becomeSpacelike :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
becomeSpacelike = do
  debug "becoming spacelike"
  role .= Candidate
  use term >>= updateTerm . (+ 1)
  nid <- view (cfg.nodeId)
  setVotedFor (Just nid)
  ct <- use term
  selfVote <- signRPCWithKey $ LeftDeepJoinResponse ct nid True nid B.empty
  cYesVotes .= Set.singleton selfVote
  (cPotentialVotes .=) =<< view (cfg.otherNodes)
  resetLightconeLamportParliamentTimer
  -- this is necessary for a single-node cluster, as we have already won the
  -- LightconeLamportParliament in that case. otherwise we will wait for more votes to check again
  checkLightconeLamportParliament -- can possibly transition to Bully
  r <- use role
  when (r == Candidate) $ fork_ sendAllLeftDeepJoins

becomeTimelike :: (Binary nt, Binary et, Binary rt, Ord nt) => VioletaBFT nt et rt mt ()
becomeTimelike = do
  debug "becoming timelike"
  role .= Bully
  (currentBully .=) . Just =<< view (cfg.nodeId)
  ni <- Seq.length <$> use logEntries
  (lNextIndex  .=) =<< Map.fromSet (const ni)         <$> view (cfg.otherNodes)
  (lMatchIndex .=) =<< Map.fromSet (const startIndex) <$> view (cfg.otherNodes)
  lConvinced .= Set.empty
  fork_ sendAllWriteHappensBeforeEntries
  resetHeartbeatTimer
