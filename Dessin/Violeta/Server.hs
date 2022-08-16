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






import Dessin.Violeta.State
import Dessin.Violeta.Lightcone
import Dessin.Violeta.LamportParliament


import qualified Dessin.Violeta.Server.Debug as Debug
import qualified Dessin.Violeta.Server.Handler as Handler
import qualified Dessin.Violeta.Server.Timer as Timer


import qualified Dessin.Violeta.Server.Lightcone as Lightcone
import qualified Dessin.Violeta.Server.LamportParliament as LamportParliament
import qualified Dessin.Violeta.Server.State as State




data Config = Config
  { otherNodes :: [NodeId]
  , nodeId :: NodeId
  , LightconeLamportParliamentTimeoutRange :: (Int, Int)
  , heartbeatTimeout :: Int
  , enableDebug :: Bool
  }

data VioletaBFTSpec = VioletaBFTSpec
  { config :: Config
  , state :: State.State
  , lightcone :: Lightcone.Lightcone
  , lamportParliament :: LamportParliament.LamportParliament
  , timer :: Timer.Timer
  , handler :: Handler.Handler
  , debug :: Debug.Debug
  }

runVioletaBFT :: VioletaBFTSpec -> IO ()
makeLenses ''VioletaBFTSpec
runVioletaBFT spec = do
  let config = view config spec
  let state = view state spec
  let lightcone = view lightcone spec
  let lamportParliament = view lamportParliament spec
  let timer = view timer spec
  let handler = view handler spec
  let debug = view debug spec
  let nodeId = view nodeId config
  let otherNodes = view otherNodes config
  let LightconeLamportParliamentTimeoutRange = view LightconeLamportParliamentTimeoutRange config
  let heartbeatTimeout = view heartbeatTimeout config
  let enableDebug = view enableDebug config
  let term = startTerm state
  let initialState = State.initialState nodeId otherNodes term
  let initialLightcone = Lightcone.initialLightcone nodeId otherNodes
  let initialLamportParliament = LamportParliament.initialLamportParliament nodeId otherNodes LightconeLamportParliamentTimeoutRange
  let initialTimer = Timer.initialTimer nodeId otherNodes heartbeatTimeout
  let initialHandler = Handler.initialHandler nodeId otherNodes
  let initialDebug = Debug.initialDebug nodeId otherNodes enableDebug
  let initialSpec = VioletaBFTSpec
        { config = config
        , state = initialState
        , lightcone = initialLightcone
        , lamportParliament = initialLamportParliament
        , timer = initialTimer
        , handler = initialHandler
        , debug = initialDebug
        }
  VioletaBFTSpec
    { config = config
    , state = state
    , lightcone = lightcone
    , lamportParliament = lamportParliament
    , timer = timer
    , handler = handler
    , debug = debug
    } <- runVioletaBFT' initialSpec
  return ()

runVioletaBFT' :: VioletaBFTSpec -> IO VioletaBFTSpec
runVioletaBFT' (VioletaBFTSpec spec) = do
  config <- getConfig
  let state = view state spec
  let lightcone = view lightcone spec
  let lamportParliament = view lamportParliament spec
  let timer = view timer spec
  let handler = view handler spec
  let debug = view debug spec
  let nodeId = view nodeId config
  let otherNodes = view otherNodes config
  let LightconeLamportParliamentTimeoutRange = view LightconeLamportParliamentTimeoutRange config
  let heartbeatTimeout = view heartbeatTimeout config
  let enableDebug = view enableDebug config
  let term = view term state
  let initialState = State.initialState nodeId otherNodes term
  let initialLightcone = Lightcone.initialLightcone nodeId otherNodes
  let initialLamportParliament = LamportParliament.initialLamportParliament nodeId otherNodes LightconeLamportParliamentTimeoutRange
  let initialTimer = Timer.initialTimer nodeId otherNodes heartbeatTimeout
  let initialHandler = Handler.initialHandler nodeId otherNodes

  let initialDebug = Debug.initialDebug nodeId otherNodes enableDebug
let initialSpec = VioletaBFTSpec
        { config = config
        , state = initialState
        , lightcone = initialLightcone
        , lamportParliament = initialLamportParliament
        , timer = initialTimer
        , handler = initialHandler
        , debug = initialDebug
        }
  VioletaBFTSpec
    { config = config
    , state = state
    , lightcone = lightcone
    , lamportParliament = lamportParliament
    , timer = timer
    , handler = handler
    , debug = debug
    } <- runVioletaBFT' initialSpec
  return ()
    return (lightcone, lamportParliament)
  where
    config = config { lightcone = lightcone
                    , lamportParliament = lamportParliament
                    , timer = timer
                    , handler = handler
                    , debug = debug
                    }
    state = state { lightcone = lightcone
                  , lamportParliament = lamportParliament
                  , timer = timer
                  , handler = handler
                  , debug = debug
                  }
    lightcone = lightcone { lightcone = lightcone
                          , lamportParliament = lamportParliament
                          , timer = timer
                          , handler = handler
                          , debug = debug
                          }
    lamportParliament = lamportParliament { lamportParliament = lamportParliament
                                          , timer = timer
                                          , handler = handler
                                          , debug = debug
                                          }
    timer = timer { timer = timerInterval
                  , handler = handler
                  , debug = debug
                  }
    handler = handler { handler = handler
                      , debug = debug
                      }
    debug = debug { debug = debug
                    }
    nodeId = nodeId { lightcone = lightcone
    }
    otherNodes = otherNodes { lightcone = lightcone
    }
    LightconeLamportParliamentTimeoutRange = LightconeLamportParliamentTimeoutRange { lightcone = lightcone
    }

    heartbeatTimeout = heartbeatTimeout { lightcone = lightcone
    }
    enableDebug = enableDebug { lightcone = lightcone
    }
    disableDebug = disable
    enable = enable
    disable = disable
    startTerm = startTerm { lightcone = lightcone
    }
    initialState = State.initialState nodeId otherNodes term
    initialLightcone = Lightcone.initialLightcone nodeId otherNodes
    initialLamportParliament = LamportParliament.initialLamportParliament nodeId otherNodes LightconeLamportParliamentTimeoutRange
    initialTimer = Timer.initialTimer nodeId otherNodes heartbeatTimeout
    initialHandler = Handler.initialHandler nodeId otherNodes
    initialDebug = Debug.initialDebug nodeId otherNodes enableDebug
    state = State.state nodeId otherNodes term
    lightcone = Lightcone.lightcone nodeId otherNodes
    lamportParliament = LamportParliament.lamportParliament nodeId otherNodes LightconeLamportParliamentTimeoutRange
    timer = Timer.timer nodeId otherNodes heartbeatTimeout
    handler = Handler.handler nodeId otherNodes
    debug = Debug.debug nodeId otherNodes enableDebug
    nodeId = nodeId { lightcone = lightcone
    }
    otherNodes = otherNodes { lightcone = lightcone
    }
    LightconeLamportParliamentTimeoutRange = LightconeLamportParliamentTimeoutRange { lightcone = lightcone
    }
    heartbeatTimeout = heartbeatTimeout { lightcone = lightcone
    }
    enableDebug = enableDebug { lightcone = lightcone
    }
    disableDebug = disable
    enable = enable
    disable = disable
    startTerm = startTerm { lightcone = lightcone
    }
    initialState = State.initialState nodeId otherNodes term
    initialLightcone = Lightcone.initialLightcone nodeId otherNodes
    initialLamportParliament = LamportParliament.initialLamportParliament nodeId otherNodes LightconeLamportParliamentTimeoutRange * 2






     ) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import qualified Data.Vector as Vector
  import qualified Data.Vector.Mutable as MVector
  import qualified Data.Vector.Generic as VG




  #ifdef DEBUG
  import qualified Debug.Trace as Trace

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
resetLightconeLamportParliamentTimer :: VioletaBFT nt et rt mt ()
resetLightconeLamportParliamentTimer = do
  modify (\s -> s { _lightconeLamportParliamentTimer = Nothing })
  resetLightconeLamportParliamentTimer'            <- liftIO $ newTimer
  modify (\s -> s { _lightconeLamportParliamentTimer = Just resetLightconeLamportParliamentTimer' })
  liftIO $ startTimer resetLightconeLamportParliamentTimer' (lightconeLamportParliamentTimeoutRange * 2)  (lightconeLamportParliamentTimeoutRange * 2)
  return ()
resetLightconeLamportParliamentTimer' :: Timer
resetLightconeLamportParliamentTimer' = Timer
  { _timerId = 0
  , _timerCallback = resetLightconeLamportParliamentTimer
  , _timerInterval = lightconeLamportParliamentTimeoutRange * 2
  , _timerActive = True
  }
handleEvents :: VioletaBFT nt et rt mt ()
resetLightconeLamportParliamentTimer' = modify (\s -> s { _timer
  , _timerCallback = Nothing
  , _timerInterval = Nothing  * 2})'
  return ()
handleEvents = do
  _lightconeLamportParliamentTimer' <- liftIO $ newTimer
  modify (\s -> s { _lightconeLamportParliamentTimer = Just _lightconeLamportParliamentTimer' })
  liftIO $ startTimer _lightconeLamportParliamentTimer' (lightconeLamportParliamentTimeoutRange * 2)  (lightconeLamportParliamentTimeoutRange * 2)
  return ()
messageReceiver :: VioletaBFT nt et rt mt ()
messageReceiver = do
  e <- liftIO $ readChan ein
  case e of
    MessageReceived m -> do
      handleMessage m
      messageReceiver
    _ -> return ()
handleMessage :: Message nt et rt mt -> VioletaBFT nt et rt mt ()
     resetLightconeLamportParliamentTimer' = modify (\s -> s { _timerInterval = Nothing  * 2})'
resetLightconeLamportParliamentTimer' = modify (\s -> s { _timerInterval = Nothing  * 2})'














