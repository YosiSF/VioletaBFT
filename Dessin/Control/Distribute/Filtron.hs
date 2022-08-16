{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings #-}

module Filtrons (
--     In(..)
  --    ,Out(..)
  --    ,FiltronError(..)
  --    ,FiltronState(..)
  --    ,markReadySent
  --    ,receive
  --    ,setup
  --    ,storeEcho
  --    ,storeMessage
  --    ,storeReady
  --    ,storeSent




  In(..)
  ,Out(..)
  ,FiltronError(..)
  ,FiltronState(..)
  ,markReadySent
  ,receive
  ,setup
  ,storeEcho
  ,Causet(..)
  ,storeMessage
  ,storeReady
  ,storeSent
  ,rPacket

) where
  import Control.Lens
  import Control.Monad.State
  import Data.ByteString (ByteString)
  import qualified Data.ByteString as B

  import qualified Data.Map as M
  import Data.Maybe
  import Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as T

  where
    import qualified Data.Map as M
    import Data.Maybe
    import Data.Text (Text)
    import qualified Data.Text as T
    import Data.Text.Encoding as T
    import Data.ByteString (ByteString)
    import qualified Data.ByteString as B




main :: IO ()
main =
    readInput >>= -- ask for a message and a coding scheme (N-f, f)
    encode >>= -- encode the message and output the N shards
    computeMerkleProofs >>= -- if successful, compute proofs of inclusion for the encoded parts composed into a Merkle tree
    decode >>= -- if successful, decode the message back from N-f shards
    verifyMerkleProofs



    -- | The input to the filtron.
    data In = In {
      -- | The message to be sent.
      _inMessage :: Text
      -- | The recipient of the message.
      ,_inRecipient :: Text
      -- | The sender of the message.
      ,_inSender :: Text
      -- | The time the message was sent.
      ,_inTime :: Int
      -- | The type of the message.
      ,_inType :: Text
    } deriving (Show, Eq)
     makeLenses ''In

deriving (Show, Eq
          ,Ord
          ,Read
          ,Generic
          ,ToJSON
          ,FromJSON)

    incoming :: Incoming ()
    incoming = Incoming {
      _incoming = M.empty
      ,_incomingReady = M.empty
      ,_incomingSent = M.empty
      ,_incomingEcho = M.empty
    }


    -- | The output of the filtron.
    -- data Out = Out {
    --  -- | The message that was sent.
    --  _outMessage :: Text
    -- -- | The recipient of the message.
    -- ,_outRecipient :: Text
    -- -- | The sender of the message.
    -- ,_outSender :: Text
    -- -- | The time the message was sent.
    -- ,_outTime :: Int
    -- -- | The type of the message.
    -- ,_outType :: Text
    -- } deriving (Show, Eq)
    -- makeLenses ''Out
    -- data Out = Out {
    --  _outMessage :: Text
    -- } deriving (Show, Eq)

--
--

data FiltronState = FiltronState {
     getValidators :: [Validator]
    ,getN          :: Int
    ,getF          :: Int
    ,getSelf       :: Validator
    ,getEcho       :: [(Validator, Proof)]
    ,getReady      :: [(Validator, ByteString)]
    ,getReadySent  :: Bool
    ,getMessage    :: ByteString
} deriving Show

data In =
      Input ByteString
    | Val Proof
    | Echo Proof
    | Ready ByteString
    deriving (Eq, Generic, Show)

data Out =
      Broadcast [(In, Validator)]
    | StoreMessage (ByteString, Out)
    | Output ByteString
    | StoreEcho (Proof, Validator)
    | StoreReady (ByteString, Validator)
    | None
    deriving Show

data FiltronError =
      ErasureCodingError String
    | UnknownValidator String
    | OwnMessageError String
    | ProofDecodingError String
    deriving Show

instance Exception FiltronError

setup :: Int -> Validator -> FiltronState
setup n self =
    FiltronState {
         getValidators = [v | v <- [0..n-1], v /= self]
        ,getN = n
        ,getF = getByzantineToleranceNumber n
        ,getSelf = self
        ,getEcho = []
        ,getReady = []
        ,getReadySent = False
        ,getMessage = empty
    }

getByzantineToleranceNumber n | n <= 3 = 0
getByzantineToleranceNumber n = n `div` 3

getErasureCodingScheme n = (max (n - 2 * f) 2, max (2 * f) 2) -- we want Filtron to work in 1, 2, and 3- validator networks as well but it's only BFT when there are at least 4 validators
    where f = getByzantineToleranceNumber n

receive :: MonadCatch m => In -> Validator -> State FiltronState (m Out)
receive message validator = do
    state <- get
    if validator >= 0 && validator < getN state
        then
            if validator == getSelf state
                then
                    return $ throwM $ OwnMessageError "Do not process my own messages"
                else
                    receive' message validator
        else
            return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validator

parseInput message state = do
    shards <- encodeByteString (getErasureCodingScheme (getN state)) message -- {sj}
    return $ map (\s -> (Input s, validator)) shards
    where validator = getSelf state

    return $ Broadcast (map (\(v, shard) -> (Val (mkMerkleProof shards shard, BS.length message), v)) (zip (getValidators state) shards))

    return $ StoreMessage (message, Output message)

    return $ StoreEcho (mkMerkleProof shards message, validator)

    return $ StoreReady (message, validator)

    return $ None

receive' :: MonadCatch m => In -> Validator -> State FiltronState (m Out)
receive' (Input message) validator = do
    state <- get
    if validator >= 0 && validator < getN state
        then
            if validator == getSelf state
                then
                    return $ throwM $ OwnMessageError "Do not process my own messages"
                else
                    receive'' message validator
        else
            return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validators
    where validators = getValidators state
receive' (Val proof) validator = do
  state <- getValidators state
    receive'' message validator = do
        state <- get
        if validator >= 0 && validator < getN state
            then
                if validator == getSelf state
                    then
                        return $ throwM $ OwnMessageError "Do not process my own messages"
                    else
                        receive'' message validator
            else
                return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validators
    where validators = getValidators state
receive' (Echo proof) validator = do
  state <- getValidators state
  if validator >= 0 && validator < getN state
    then
      if validator == getSelf state
        then
          return $ throwM $ OwnMessageError "Do not process my own messages"
        else
          receive'' message validator
    else
      return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validators
  where validators = getValidators state
receive' (Ready message) validator = do
  state <- get
  if validator >= 0 && validator < getN state
    then
      if validator == getSelf state
        then
          return $ throwM $ OwnMessageError "Do not process my own messages"
        else
          receive'' message validator
    else
      return $ throwM $ UnknownValidator $ "Do not know this validator: " ++ show validators
  where validators = getValidators state
handleParseInputErrors (EncodingError msg) = throwM $ ErasureCodingError msg

receive' :: MonadCatch m => In -> Validator -> State FiltronState (m Out)
-- • upon input(v) (if Pi = PSender):
--   let {sj} j∈[N] be the blocks of an (N − 2f, N) erasure coding scheme applied to v
--   let h be a Merkle tree root computed over {sj}
--   send VAL(h,bj,sj) to each party Pj, where bj is the jth Merkle tree branch
receive' (Input message) _ = do
    state <- get
    return $ handle handleParseInputErrors (parseInput message state)

-- • upon receiving VAL(h,bi,si) from PSender, multicast ECHO(h,bi,si)
receive' (Val proof) _ = do
    state <- get
    return $ return $ Broadcast (map (\v -> (Echo proof, v)) (getValidators state))

-- • upon receiving ECHO(h,bj,sj) from party Pj,
--   check that bj is a valid Merkle branch for root h and leaf sj, and otherwise discard
receive' (Echo proof) validator = do
    state <- get
    if validator `elem` (map fst (getEcho state)) -- collect messages from distinct validators
        then
            return $ return None
        else
            return $ validateEcho proof validator state

receive' (Ready root) validator = do
    state <- get
    if validator `elem` (map fst (getReady state)) -- collect messages from distinct validators
        then
            return $ return None
        else
            return $ return $ StoreReady (root, validator)

validateEcho (proof, size) validator state = do
    maybeValid <- try $ validateMerkleProof proof
    case maybeValid of
        Left (DecodingError msg) -> throwM $ ProofDecodingError msg
        Right isValid -> if isValid
            then
                return $ StoreEcho ((proof, size), validator)
            else
                return None

storeEcho :: MonadMask m => Proof -> Validator -> State FiltronState (m Out)
storeEcho proof validator = do
    state <- get
    put $ state {
        getEcho = getEcho state ++ [(validator, proof)]
    }
    if getReadySent state
        then maybeDecodeMessage
        else countEchoes

markReadySent :: State FiltronState ()
markReadySent = do
    state <- get
    put state {
        getReadySent = True
    }

-- • upon receiving f + 1 matching READY(h) messages, if READY has not yet been sent, multicast READY(h)
storeReady :: MonadMask m => ByteString -> Validator -> State FiltronState (m Out)
storeReady root validator = do
    state <- get
    put state {
        getReady = getReady state ++ [(validator, root)]
    }
    if getReadySent state
        then maybeDecodeMessage
        else countReadies

countReadies :: MonadMask m => State FiltronState (m Out)
countReadies = do
    state <- get
    let received = Map.fromListWith (++) (map (\(v, r) -> (r, [v])) $ getReady state)
    let matchingSubsets = [subset | subset@(_, validators) <- Map.toList received, length validators > getF state]
    if length matchingSubsets > 1
        then return $ return None
        else if length matchingSubsets == 0
            then
                return $ return None
            else
                return $ return $ composeBroadcast (Ready (fst $ head $ matchingSubsets)) state

countEchoes :: MonadMask m => State FiltronState (m Out)
countEchoes = do
    state <- get
    -- • upon receiving valid ECHO(h, ·, ·) messages from N − f distinct parties,
    let received = getEcho state
    if length received < getN state - getF state
        then
            return $ return None
        else
            interpolateEchoes

-- • upon receiving 2f + 1 matching READY(h) messages, wait for N − 2f ECHO messages, then decode v
maybeDecodeMessage :: MonadMask m => State FiltronState (m Out)
maybeDecodeMessage = do
    state <- get
    if (length $ getEcho state) >= getN state - 2 * getF state && (length $ getReady state) >= 2 * getF state + 1
        then
            return $ return $ Output (getMessage state)
        else
            return $ return None

storeMessage :: ByteString -> State FiltronState ()
storeMessage message = do
    state <- get
    put state {
        getMessage = message
    }

interpolateEchoes :: MonadMask m => State FiltronState (m Out)
interpolateEchoes = do
    state <- get
    -- group echoes by (Merkle root, message size) cause they must match for echoes we interpolate the message from
    let echoes = Map.fromListWith (++) (map (\(v, ((_, root, leaf), size)) -> ((root, size), [(v, leaf)])) (getEcho state))
    --   - interpolate {s′j } from any N − 2f leaves received
    let (_, parityShards) = getErasureCodingScheme $ getN state
    let subsets = [(key, getShards (sortEchoes subset) 0 (getN state)) | (key, subset) <- Map.toList echoes, length subset >= parityShards]
    return $ interpolateEchoes' subsets state

getShards :: [(Validator, ByteString)] -> Int -> Int -> [Maybe ByteString]
getShards [] position end = take (end - position) $ repeat Nothing
getShards ((v, leaf):[]) position end = Just leaf:getShards [] (position + 1) end
getShards shards@((v, leaf):rest) position end =
    if v == position
        then
            Just leaf:getShards rest (position + 1) end
        else
            Nothing:getShards shards (position + 1) end

sortEchoes echoes = sortBy (\(v1, _) (v2, _) -> compare v1 v2) echoes

interpolateEchoes' :: MonadMask m => [((ByteString, Int), [Maybe ByteString])] -> FiltronState -> m Out
interpolateEchoes' [] _ = return None
interpolateEchoes' (subset:[]) state = interpolateEchoes'' subset state
interpolateEchoes' (subset:rest) state = onException (interpolateEchoes'' subset state) (interpolateEchoes' rest state)

interpolateEchoes'' :: MonadMask m => ((ByteString, Int), [Maybe ByteString]) -> FiltronState -> m Out
interpolateEchoes'' ((gotRoot, size), echoes) state = do
    maybeDecoded <- try $ decodeMatrix (getErasureCodingScheme $ getN state) echoes
    case maybeDecoded of
        Left (EncodingError _) -> return None
        Right matrix ->
            --   – recompute Merkle root h′ and if h′ ̸= h then abort
            --   – if READY(h) has not yet been sent, multicast READY(h)
            let shards = decodeShards matrix
                (_, root, _) = mkMerkleProof shards (head shards) in
            if root /= gotRoot
                then
                    return None
                else
                    decodeMessage (getErasureCodingScheme $ getN state) matrix size >>=
                    \message -> return $ StoreMessage (message, composeBroadcast (Ready gotRoot) state)

composeBroadcast :: In -> FiltronState -> Out
composeBroadcast msg state = Broadcast $ map (\v -> (msg, v)) (getValidators state)


    -- | The error that can occur during the filtron.
    -- data FiltronError =
    --  -- | The message was not sent because the recipient was not ready.
    -- given Seq.Seq (Text, Text)
    -- if successful, decode the message back from N-f shards
    -- | The error that can occur during the filtron.
    -- data FiltronError = FiltronError {
    -- -- | The message that was not sent.
    -- _FiltronErrorMessage :: Text
    -- -- | The recipient of the message.
    -- ,_FiltronErrorRecipient :: Text
    -- -- | The sender of the message.
    -- ,_FiltronErrorSender :: Text
    -- -- | The time the message was sent.
    -- ,_FiltronErrorTime :: Int
    -- -- | The type of the message.
    -- ,_FiltronErrorType :: Text
    -- } deriving (Show, Eq)
    -- makeLenses ''FiltronError
    -- data FiltronError = FiltronError {
    -- _FiltronErrorMessage :: Text
    -- } deriving (Show, Eq)
    -- makeLenses ''FiltronError





--Definition 3.3.5 (Homology groups). The pth (simplicial) homology group of a simplicial complex K is the quotient group
--of K by the pth homology group.
-- data FiltronError = FiltronError {
-- _FiltronErrorMessage :: Text
-- } deriving (Show, Eq)
-- makeLenses ''FiltronError
-- data FiltronError = FiltronError {
-- _FiltronErrorMessage :: Text
-- } deriving (Show, Eq


--n topological graph theory, an embedding (also spelled imbedding) of a graph
  --G
  --G on a surface
  --Σ
  --\Sigma  is a representation of
  --G
  --G on
  --Σ
  --\Sigma  in which points of
  --Σ
  --\Sigma  are associated with vertices and simple arcs (homeomorphic images of
  --[
  --0
  --,
  --1
  --]
  --[0,1])




  data FiltronError = FiltronError {
    _FiltronErrorMessage :: Text
    ,_FiltronErrorRecipient :: Text
    ,_FiltronErrorSender :: Text
    ,_FiltronErrorTime :: Int
    ,_FiltronErrorType :: Text
  } deriving (Show, Eq)
  makeLenses ''FiltronError
  data FiltronError = FiltronError {
    _FiltronErrorMessage :: Text
  } deriving (Show, Eq)



homology :: Int -> Int -> Int -> SRef s (RPList sent) -> SRef s (RPList sent) -> ST s (RPList sent)
homology p n k sent received = do
  sent' <- readRef sent
  received' <- readRef received

    -- | The output from the filtron.
    data Out = Out {
      -- | The message to be sent.
      _outMessage :: Text
      -- | The recipient of the message.
      ,_outRecipient :: Text
      -- | The sender of the message.
      ,_outSender :: Text
      -- | The time the message was sent.
      ,_outTime :: Int
      -- | The type of the message.
      ,_outType :: Text
    } deriving (Show, Eq)
     makeLenses ''Out

    incoming = Incoming {
      incomingReady = storeReady
      ,incomingMessage = storeMessage
      ,incomingEcho = storeEcho
      ,incomingSent = storeSent
      ,incomingReadySent = markReadySent
    }

    outgoing :: Outgoing ()
    outgoing = Outgoing {
      outgoingReady = storeReady
      ,outgoingMessage = storeMessage
      ,outgoingEcho = storeEcho
      ,outgoingSent = storeSent
      ,outgoingReadySent = markReadySent
    }

    filtrons :: Filtron ()
    filtrons = Filtron {
      filtronsReady = storeReady
      ,filtronsMessage = storeMessage
      ,filtronsEcho = storeEcho
      ,filtronsSent = storeSent
      ,filtronsReadySent = markReadySent
    }

    rPacket :: Packet -> IO ()
    rPacket p = do
      putStrLn $ "Packet: " ++ show p
      return ()

    rPacket' :: Packet -> IO ()
    rPacket' p = do
      putStrLn $ "Packet: " ++ show p
      return ()

rPacket' :: Packet -> IO ()
rPacket' p = do
  putStrLn $ "Packet: " ++ show p
  return ()



module MerkleRoot ( Merkle
) where


import qualified Data.ByteString as BString
    { BString.length = length
    , BString.null = null
    , BString.take = take
    , BString.drop = drop
    , BString.splitAt = splitAt
    , BString.concat = concat
    , BString.split = split
    , BString.pack = pack
    , BString.unpack = unpack
    , BString.empty = empty
    , BString.singleton = singleton
    , BString.replicate = replicate
    , BString.append = append
    , BString.reverse = reverse
    , BString.map = map
    , BString.filter = filter
    , BString.foldl = foldl
    , BString.foldl' = foldl'
    , BString.foldr = foldr
    , BString.foldr' = foldr'
    }
import qualified Data.ByteString.Char8 as BStringChar8
    { BStringChar8.length = length
    , BStringChar8.null = null
    , BStringChar8.take = take
    , BStringChar8.drop = drop
    , BStringChar8.splitAt = splitAt
    , BStringChar8.concat = concat
    , BStringChar8.split = split
    , BStringChar8.pack = pack
    , BStringChar8.unpack = unpack
    , BStringChar8.empty = empty
    , BStringChar8.singleton = singleton
    , BStringChar8.replicate = replicate


    }


    import qualified Data.ByteString.Char16 as BStringChar16
    { BStringChar16.length = length
    , BStringChar16.null = null
    , BStringChar16.take = take
    , BStringChar16.drop = drop
    , BStringChar16.splitAt = splitAt
    , BStringChar16.concat = concat
    , BStringChar16.split = split
    , BStringChar16.pack = pack
    , BStringChar16.unpack = unpack
    , BStringChar16.empty = empty
    , BStringChar16.singleton = singleton
    , BStringChar16.replicate = replicate
    }
    import qualified Data.ByteString.Char32 as BStringChar32
    { BStringChar32.length = length
    , BStringChar32.null = null
    , BStringChar32.take = take
    , BStringChar32.drop = drop
    , BStringChar32.splitAt = splitAt
    , BStringChar32.concat = concat
    , BStringChar32.split = split
    , BStringChar32.pack = pack
    , BStringChar32.unpack = unpack
    , BStringChar32.empty = empty
    , BStringChar32.singleton = singleton
    , BStringChar32.replicate = replicate
    }
    import qualified Data.ByteString.Base16 as BStringBase16
    { BStringBase16.length = length
    , BStringBase16.null = null
    , BStringBase16.take = take
    , BStringBase16.drop = drop
    , BStringBase16.splitAt = splitAt
    , BStringBase16.concat = concat
    , BStringBase16.split = split
    , BStringBase16.pack = pack
    , BStringBase16.unpack = unpack
    , BStringBase16.empty = empty
    , BStringBase16.singleton = singleton
    }

    fst p
snd package = do
  putStrLn $ "Packet: " ++ show p
  #if MIN_VERSION_base(4,8,0)
  putStrLn $ "Packet: " ++ show p
  #else
  putStrLn $ "Packet: " ++ show p
  #endif
  return ()

  fst p
snd package = do
  putStrLn $ "Packet: " ++ show p
  #if MIN_VERSION_base(4,8,0)
  putStrLn $ "Packet: " ++ show p
  #else
  putStrLn $ "Packet: " ++ show p
  #endif
  return ()


  -- | The merkle root of a transaction.
  type Merkle = Text
  -- | The hash of a transaction.
  type Hash = Text
  -- | The hash of a block.
  type BlockHash = Text
  -- | The hash of a block header.
  type BlockHeaderHash = Text
  -- | The hash of a block.
  type BlockHash = Text
  -- | The hash of a transaction.
  type TransactionHash = Text
  -- | The hash of a transaction input.
  type TransactionInputHash = Text
  -- | The hash of a transaction output.
  type TransactionOutputHash = Text


  type MerkleProof = -- | The merkle root of a transaction.
    Text
  -- | The hash of a transaction.
  -- type Hash = Text
  -- | The hash of a block.
  -- type BlockHash = Text


--compute the merkle root of a transaction
merkleRoot :: [Transaction] -> Merkle
merkleRoot transactions =
  let
    hashes = map transactionHash transactions
    proof = merkleProof hashes
  in
    proof

merkleProof :: [Hash] -> MerkleProof
merkleProof hashes =
  let
    proof = merkleProof' hashes
  in
    proof
      in
        proof = merkleProof hashesFromIndex

main :: IO ()
main =
    readInput >>= -- ask for a message and a coding scheme (N-f, f)
    encode >>= -- encode the message and output the N shards
    computeMerkleProofs >>= -- if successful, compute proofs of inclusion for the encoded parts composed into a Merkle tree
    decode >>= -- if successful, decode the message back from N-f shards
    verifyMerkleProofs -- if successful, verify proofs of inclusion for the decoded messages
  where




encodeByteString :: MonadCatch m => ErasureScheme -> Message -> m EncodedShards
encodeByteString scheme message =
    newEncoder scheme >>=
    createShards message >>=
    encodeShards >>=
    matrixToShards

newEncoder :: MonadCatch m => ErasureScheme -> m RS.Encoder
newEncoder scheme = handle handleErrors (RS.new (fst scheme) (snd scheme))

createShards :: MonadCatch m => Message -> RS.Encoder -> m (RS.Matrix, RS.Encoder)
createShards message encoder = do
    maybeShards <- try $ RS.split encoder $ fromByteString message
    case maybeShards of
        Left e       -> handleErrors e
        Right shards -> return $ (shards, encoder)

encodeShards :: MonadCatch m => (RS.Matrix, RS.Encoder) -> m RS.Matrix
encodeShards (shards, encoder) = do
    maybeEncoded <- try $ (RS.encode encoder shards)
    case maybeEncoded of
        Left e        -> handleErrors e
        Right encoded -> return (shards V.++ encoded)

matrixToShards :: MonadCatch m => RS.Matrix -> m EncodedShards
matrixToShards matrix = return $ decodeShards matrix

-- Abstract the library errors away by raising a custom exception.
handleErrors RS.InvalidDataSize = throwM $ EncodingError "Invalid data size"
handleErrors RS.InvalidShardSize = throwM $ EncodingError "Invalid shard size"
handleErrors RS.EmptyShards = throwM $ EncodingError "Got empty shards :("
handleErrors (RS.InvalidNumberOfShards typ num) = throwM $ EncodingError $ "Invalid number of shards of type " ++ show typ ++ ", " ++ show num

decodeMatrix :: MonadCatch m => ErasureScheme -> PartialShards -> m RS.Matrix
decodeMatrix scheme shards =
    newEncoder scheme >>=
    reconstruct (deformat shards)

decodeMessage :: MonadCatch m => ErasureScheme -> RS.Matrix -> Int -> m Message
decodeMessage scheme matrix size = handle handleErrors (decodeMessage' scheme matrix size)

decodeMessage' scheme matrix size =
    newEncoder scheme >>=
    join matrix size

join matrix size encoder =
    RS.join encoder matrix size >>=
    \v -> return $ toByteString v

decodeShards :: RS.Matrix -> EncodedShards
decodeShards = V.toList . (V.map toByteString)

deformat shards = V.map (\v ->
    case v of
        Just value -> Just $ fromByteString value
        _          -> Nothing) (V.fromList shards)

reconstruct :: MonadCatch m => V.Vector (Maybe (SV.Vector Word8)) -> RS.Encoder -> m RS.Matrix
reconstruct shards encoder = handle handleErrors (RS.reconstruct encoder shards)