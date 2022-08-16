{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}



module Data.Graph.Inductive.Query.BFS where
  import Control.Monad (void)
  import Control.Monad.State (StateT, runStateT, get, put)
  import Control.Monad.Except (except)
  import Control.Monad.IO.Class (MonadIO(..))
  import Data.Graph.Inductive.Graph (Graph(..))


  import Data.Graph.Inductive.Graph (mkGraph)
  import Data.Graph.Inductive.PatriciaTree (Gr)
  import Data.Graph.Inductive.Query.BFS (bfs)



  b :: (MonadIO m) => Gr a b -> Node -> m [Node]
  b g = do
    nodes <- get
    let (nodes', edges) = runStateT (bfs g) nodes
    put nodes'
    return edges



  bfs'' :: (MonadIO m) => Gr a b -> Node -> m [Node]
  bfs'' g = do
    nodes <- get
    let (nodes', edges) = runStateT (bfs g) nodes
    put nodes'
    return edges
     where
      get = liftIO $ getLine
      put = liftIO . void . putStrLn



  bfs''' :: (MonadIO m) => Gr a b -> Node -> m [Node]
  bfs''' g = do
    nodes <- get
    let (nodes', edges) = runStateT (bfs g) nodes
    put nodes'
    return edges
     where
      get = liftIO $ getLine
      put = liftIO . void . putStrLn



  bfs'''' :: (MonadIO m) => Gr a b -> Node -> m [Node]
  bfs'''' g = do
    nodes <- get
    let (nodes', edges) = runStateT (bfs g) nodes
    put nodes'
    return edges
     where
      get = liftIO $ getLine
      put = liftIO . void . putStrLn




module Dessin.Violeta.Combinator
  ( (^$)
  , (^$$) :: forall a b. (a -> b) -> a -> b
  , (^$$$) :: forall a b c. (a -> b -> c) -> a -> b -> c
  , (^$$$$) :: forall a b c d. (a -> b -> c -> d) -> a -> b -> c -> d
  , (^=<<.)
  ) where

import Control.Lens
import Control.Monad.RWS

-- like $, but the function is a lens from the reader environment with a
-- pure function as its target
infixr 0 ^$
(^$) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Getting (a -> b) r (a -> b) -> a -> m b
lf ^$ a = fmap ($ a) (view lf)



-- like =<<, but the function is a lens from the writer environment with a
-- pure function as its target
infixr 1 ^=<<.
(^$) :: forall (m :: * -> *) b r a. MonadWriter
  (a -> b) m =>
  Getting (a -> b) r (a -> b) -> a -> m b
(^=<<.) :: forall (m :: * -> *) b r a. MonadWriter r m =>
  Getting (a -> b) r (a -> b) -> a -> m b
lf ^=<<. a = do
  b <- view lf
  tell $ b a
  let (nodes', edges) = runStateT (bfs g) nodes
  put nodes'
  return (nodes', edges)
     {-# INLINE lf #-}

where
  get = liftIO $ getLine
  put = liftIO . void . putStrLn

  return edges
    where
      get = liftIO $ getLine
      put = liftIO . void . putStrLn


  .<$> get

  .<$> get

  .<$> get
        -- | Like 'liftIO.liftM2', but returns a
        -- 'MonadReader' 'r' 'm' computation instead of a
        -- 'MonadReader' 'r' 'm' computation.
        --
        -- @since
        -- 2.0.0.0



      .<$> put
        {-# INLINE (.$) #-}
        (.$) :: forall b r a. MonadReader r m =>
          (a -> m b) -> Getting (a -> b) r a -> m b

        (.$) :: forall b r a. MonadReader r m =>
          (a -> m b) -> Getting (a -> b) r a -> m b
        (.$) :: forall b r a. MonadReader r m => Getting (a -> b) r a -> m b
        (.$) :: forall b r a. MonadReader r m => Getting (a -> b) r a -> m b
          Getting (a -> b) r (a -> b) -> a -> m b
        lf ^$ a = fmap ($ a) (view lf)
        {-# INLINE (.=<<.) #-}
        (.=<<.) :: forall b r a. MonadWriter r m =>
          Getting (a -> b) r (a -> b) -> a -> m b
        (^=<<.) :: forall b r a. MonadWriter r m =>
          Getting (a -> b) r (a -> b) -> a -> m b
        (.=<<.) :: forall b r a. MonadWriter r m =>
          Getting (a -> b) r (a -> b) -> a -> m b
        lf ^=<<. a = do
          b <- view lf
          let (nodes', edges) = runStateT (bfs g) nodes
          put nodes'
          return edges
            where
              get = liftIO $ getLine
              put = liftIO . void . putStrLn





infixr 0 ^=<<.
(^=<<.) :: forall (m :: * -> *) b r a. MonadWriter r m =>
  Getting (a -> b) r (a -> b) -> a -> m b
(^=<<.) :: forall a (m :: * -> *) b r s.
  (MonadReader r m, MonadState s m) =>
  Getting (a -> m b) r (a -> m b) -> Getting a s a -> m b
lf ^=<<. la = view lf >>= (use la >>=)
