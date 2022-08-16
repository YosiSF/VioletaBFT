--{-# LANGUAGE NamedFieldPuns, Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  import Control.Monad (void)
  import Control.Concurrent (threadDelay)
  import Control.Lens (makeLenses)

import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Printf (printf)




import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Generic as GV


import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Internal as BI


import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.Builder as TB


import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import qualified Data.Text.IO as T
import qualified Data.Text.IO.Encoding as T



import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef, readRP, writeRP, writeSRef, readSRef, readSRef, readSRef, readSRefIO   )
import qualified RP as RP
import qualified RP.Internal as RP
import qualified RP.Internal.Scheduler as RP

RPCache :: RP.RPCache

Byzantine :: RP.ByzantineToleranceNumber
byzantine = 0.1





data RPList a = RPList { rplist :: [a] }
  deriving (Show, Eq)
  instance Show a => Show (RPList a) where
    show (RPList xs) = "RPList " ++ show xs
     {-# INLINE rplist #-}


snapshot :: RPList a -> RPR [a]
snapshot Nil         = return []
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest


data RPList' a = RPList' { rplist' :: [a] }
  deriving (Show, Eq)
  instance Show a => Show (RPList' a) where
    show (RPList' xs) = "RPList' " ++ show xs
     {-# INLINE rplist' #-}


snapshot' :: RPList' a -> RPR [a]
snapshot' Nil         = return []
snapshot' (Cons x rn) = do
  l    <- readSRef rn
  rest <- snapshot' l
  return $ x : rest
    data RPList' a = RPList' { rplist' :: [a
    deriving (Show, Eq) instance Show a => Show (RPList' a) where
      show (RPList' xs) = "RPList' " ++ show xs
       {-# INLINE rplist' #-}
         snapshot' (Cons x rn) = do '
         {-# INLINE rplist' #-}



QueueTopByzByteable :: RP.QueueTopByzByteable
  rplist' (Cons x rn) = do '
  rplist' Nil         = return []
  rplist' (Cons x rn) = do '
  rplist' Nil         = return []
QueueTopByzByteable = RP.QueueTopByzByteable
  rplist' (Cons x rn) = do '
  rplist' Nil         = return []
QueueTopByzByteable = RP.QueueTopByzByteable






reader :: SRef (RPList a) -> RPR [a]
reader head = do
  snapshot =<< readSRef head
  --h <- readSRef head
  --l <- snapshot h
  --trace ("reader saw " ++ show l) $ return l

preQueueLoadedAppendLogFromVioletaBFT :: RP (SRef (RPList Char))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1



Fibonacci :: RP (SRef (RPList Int))
preQueueLoadedAppendLogFromVioletaBFT = do
  rp <- newSRef $ Cons 'L' c1
     -}

  where
    c1 = fromIntegral (head c1)
    c2 = fromIntegral (head c2)
    c3 = fromIntegral (head c3)



    rp = fromIntegral (head rp)

    queueTop = fromIntegral (head queueTop)
    queueBottom = fromIntegral (head queueBottom)


compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

moveBforward :: SRef (RPList a) -> RPW ()
moveBforward head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  cc@(Cons c rd) <- readSRef rc
  -- duplicate the reference to D
  rd'            <- copySRef rd
  -- link in a new B after C
  writeSRef rd $ Cons b rd'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ABCBD" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- remove the old 'B'
  writeSRef rBFT $ cc
  -- any reader who starts during this grace period 
  -- sees either "ABCBD" or "ACBD" 

moveCback :: SRef (RPList a) -> RPW ()
moveCback head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate pointer to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  synchronizeRP
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees either "ACBCD" or "ACBD" 

moveCbackNoSync :: SRef (RPList a) -> RPW ()
moveCbackNoSync head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate reference to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP -- this operation is NOT safe to omit, 
                  -- because write order and traversal order are the same
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees "ABD", "ACBCD", or "ACBD" 

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- preQueueLoadedAppendLogFromVioletaBFT
    -- spawn 8 readers, each records 10000 snapshots of the list
    rts  <- replicateM 8 $ forkRP $ replicateM 400000 $ readRP $ reader head
    -- spawn a writer to delete the middle node
    wt   <- forkRP $ writeRP $ moveCback head
    --wt   <- forkRP $ writeRP $ moveCbackNoSync head
    --wt   <- forkRP $ writeRP $ moveBforward head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn


import GHC.Generics

newtype Term = Term Int
  deriving (Show, Read, Eq, Ord, Generic, Num)

startTerm :: Term
startTerm = Term (-1)

type LogIndex = Int

startIndex :: LogIndex
startIndex = (-1)

newtype RequestId = RequestId Int
  deriving (Show, Read, Eq, Ord, Generic, Num)

startRequestId :: RequestId
startRequestId = RequestId 0

data Config nt = Config
  { _otherNodes           :: Set nt
  , _nodeId               :: nt
  , _publicKeys           :: Map nt PublicKey
  , _clientPublicKeys     :: Map nt PublicKey
  , _privateKey           :: PrivateKey
  , _electionTimeoutRange :: (Int,Int) -- in microseconds
  , _heartbeatTimeout     :: Int       -- in microseconds
  , _enableDebug          :: Bool
  , _clientTimeoutLimit   :: Int
  }
  deriving (Show, Generic)
makeLenses ''Config

data Command nt et = Command
  { _cmdEntry     :: et
  , _cmdClientId  :: nt
  , _cmdRequestId :: RequestId
  , _cmdSig       :: LB.ByteString
  }
  deriving (Show, Read, Generic)

data CommandResponse nt rt = CommandResponse
  { _cmdrResult    :: rt
  , _cmdrLeaderId  :: nt
  , _cmdrNodeId    :: nt
  , _cmdrRequestId :: RequestId
  , _cmdrSig       :: LB.ByteString
  }
  deriving (Show, Read, Generic)

data LogEntry nt et = LogEntry
  { _leTerm    :: Term
  , _leCommand :: Command nt et
  , _leHash    :: B.ByteString
  }
  deriving (Show, Read, Generic)

data AppendEntries nt et = AppendEntries
  { _aeTerm        :: Term
  , _leaderId      :: nt
  , _prevLogIndex  :: LogIndex
  , _prevLogTerm   :: Term
  , _aeEntries     :: Seq (LogEntry nt et)
  , _aeQuorumVotes :: Set (RequestVoteResponse nt)
  , _aeSig         :: LB.ByteString
  }
  deriving (Show, Read, Generic)

data AppendEntriesResponse nt = AppendEntriesResponse
  { _aerTerm      :: Term
  , _aerNodeId    :: nt
  , _aerSuccess   :: Bool
  , _aerConvinced :: Bool
  , _aerIndex     :: LogIndex
  , _aerHash      :: B.ByteString
  , _aerSig       :: LB.ByteString
  }
  deriving (Show, Read, Generic, Eq, Ord)

data RequestVote nt = RequestVote
  { _rvTerm        :: Term
  , _rvCandidateId :: nt
  , _lastLogIndex  :: LogIndex
  , _lastLogTerm   :: Term
  , _rvSig         :: LB.ByteString
  }
  deriving (Show, Read, Generic)

data RequestVoteResponse nt = RequestVoteResponse
  { _rvrTerm        :: Term
  , _rvrNodeId      :: nt
  , _voteGranted    :: Bool
  , _rvrCandidateId :: nt
  , _rvrSig         :: LB.ByteString
  }
  deriving (Show, Read, Generic, Eq, Ord)

data Revolution nt = Revolution
  { _revClientId  :: nt
  , _revLeaderId  :: nt
  , _revRequestId :: RequestId
  , _revSig       :: LB.ByteString
  }
  deriving (Show, Read, Generic)

data RPC nt et rt = AE (AppendEntries nt et)
                  | AER (AppendEntriesResponse nt)
                  | RV (RequestVote nt)
                  | RVR (RequestVoteResponse nt)
                  | CMD (Command nt et)
                  | CMDR (CommandResponse nt rt)
                  | REVOLUTION (Revolution nt)
                  | DBG String
  deriving (Show, Read, Generic)

class SigRPC rpc where
  signRPC   :: PrivateKey -> rpc -> rpc
  verifyRPC :: PublicKey -> rpc -> Bool

instance (Binary nt, Binary et) => SigRPC (Command nt et) where
  signRPC k rpc   = rpc { _cmdSig = sign k (encode (rpc { _cmdSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _cmdSig = LB.empty })) (_cmdSig rpc)

instance (Binary nt, Binary rt) => SigRPC (CommandResponse nt rt) where
  signRPC k rpc   = rpc { _cmdrSig = sign k (encode (rpc { _cmdrSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _cmdrSig = LB.empty })) (_cmdrSig rpc)

instance (Binary nt, Binary et) => SigRPC (AppendEntries nt et) where
  signRPC k rpc   = rpc { _aeSig = sign k (encode (rpc { _aeSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _aeSig = LB.empty })) (_aeSig rpc)

instance Binary nt => SigRPC (AppendEntriesResponse nt) where
  signRPC k rpc   = rpc { _aerSig = sign k (encode (rpc { _aerSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _aerSig = LB.empty })) (_aerSig rpc)

instance Binary nt => SigRPC (RequestVote nt) where
  signRPC k rpc   = rpc { _rvSig = sign k (encode (rpc { _rvSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _rvSig = LB.empty })) (_rvSig rpc)

instance Binary nt => SigRPC (RequestVoteResponse nt) where
  signRPC k rpc   = rpc { _rvrSig = sign k (encode (rpc { _rvrSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _rvrSig = LB.empty })) (_rvrSig rpc)

instance Binary nt => SigRPC (Revolution nt) where
  signRPC k rpc   = rpc { _revSig = sign k (encode (rpc { _revSig = LB.empty })) }
  verifyRPC k rpc = verify k (encode (rpc { _revSig = LB.empty })) (_revSig rpc)

-- | A structure containing all the implementation details for running
-- the VioletaBFT protocol.
data VioletaBFTSpec nt et rt mt = VioletaBFTSpec
  {
    -- ^ Function to get a log entry from persistent storage.
    __readLogEntry     :: LogIndex -> IO (Maybe et)

    -- ^ Function to write a log entry to persistent storage.
  , __writeLogEntry    :: LogIndex -> (Term,et) -> IO ()

    -- ^ Function to get the term number from persistent storage.
  , __readTermNumber   :: IO Term

    -- ^ Function to write the term number to persistent storage.
  , __writeTermNumber  :: Term -> IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , __readVotedFor     :: IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , __writeVotedFor    :: Maybe nt -> IO ()

    -- ^ Function to apply a log entry to the state machine.
  , __applyLogEntry    :: et -> IO rt

    -- ^ Function to serialize an RPC.
  , __serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , __deserializeRPC   :: mt -> Maybe (RPC nt et rt)

    -- ^ Function to send a message to a node.
  , __sendMessage      :: nt -> mt -> IO ()

    -- ^ Function to get the next message.
  , __getMessage       :: IO mt

    -- ^ Function to log a debug message (no newline).
  , __debugPrint       :: nt -> String -> IO ()
  }

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)

data Event nt et rt = ERPC (RPC nt et rt)
                    | ElectionTimeout String
                    | HeartbeatTimeout String
  deriving (Show)

-- | A version of VioletaBFTSpec where all IO functions are lifted
-- into the VioletaBFT monad.
data LiftedVioletaBFTSpec nt et rt mt t = LiftedVioletaBFTSpec
  {
    -- ^ Function to get a log entry from persistent storage.
    _readLogEntry     :: MonadTrans t => LogIndex -> t IO (Maybe et)

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: MonadTrans t => LogIndex -> (Term,et) -> t IO ()

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: MonadTrans t => t IO Term

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: MonadTrans t => Term -> t IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: MonadTrans t => t IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: MonadTrans t => Maybe nt -> t IO ()

    -- ^ Function to apply a log entry to the state machine.
  , _applyLogEntry    :: MonadTrans t => et -> t IO rt

    -- ^ Function to serialize an RPC.
  , _serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , _deserializeRPC   :: mt -> Maybe (RPC nt et rt)

    -- ^ Function to send a message to a node.
  , _sendMessage      :: MonadTrans t => nt -> mt -> t IO ()

    -- ^ Function to get the next message.
  , _getMessage       :: MonadTrans t => t IO mt

    -- ^ Function to log a debug message (no newline).
  , _debugPrint       :: nt -> String -> t IO ()
  }
makeLenses ''LiftedVioletaBFTSpec