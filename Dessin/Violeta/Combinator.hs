{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

--We begin by partitioning the indices of nonzero columns into those that are leftmost columns (i.e. that will not be added to) and those that are neighbours of some leftmost column (i.e. those that will be added to).
  --let ( neighbour_idxs , leftmost_idxs ) =
  --(iota n) |> filter (λj → s.lows[j] != −1)
  --|> partition (λj → s.arglows[j]] != j)
  --where n is the number of columns and s is the current state.
  --The leftmost_idxs are the indices of the columns that will not be added to.
  --The neighbour_idxs are the indices of the columns that will be added to.
  --The leftmost_idxs are the indices of the columns that will not be added to.
  --The neighbour_idxs are the indices of the columns that will be added to.
  --The leftmost_idxs are the indices of the columns that will not be added to.
  --The neighbour_idxs are the indices of the columns that will be added to.






  import Data.List
  import Data.Maybe
  import Data.Array
  import Data.Array.ST
  import Control.Monad.ST
  import Control.Monad
  import Control.Monad.ST.Safe
  import Control.Monad.ST.Safe.Coerce
  import Control.Monad.ST.Safe.Unsafe


  import qualified Data.Vector.Unboxed as V
  import qualified Data.Vector.Unboxed.Mutable as MV


  import qualified Data.Vector.Unboxed.Mutable as MV
  import qualified Data.Vector.Unboxed as V


module Dessin.Violeta.Combinator
  ( (^$)
  , (^$$)
  , (^$$$)
  , (^$$$$)

  , (^=<<.)
  , (^=<<$)
  , (^=<<$$)
  , (^=<<$$$)
  , (^=<<$$$$) :: forall
    ( a
    , b -> c
    , d -> environment
    , environment -> environment
    , environment -> environment
  ) where



   instance (Applicative f, Applicative g) =>   Applicative (Dessin.Violeta.Combinator f g) where
       Applicative (f :. g) where
           f <*> (g :. h) =
    f <*> g <*> h
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> (g <*> h) #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f :. g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f
      g h h = f <*> g <*> h #-}
    {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall
        f g h h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f g h. f <*> g <*> h = f <*> g <*> h #-}
      {-# RULES "f <*> g <*> h" forall f
        g h h h h h h h h h h h h h h h h h
        {-# RULES "f <*> g <*> h" forall f
          g h h h h h h h h h h h h h h
            g h h h h h h h h h h h h h h h h h h
            h h h h h h h h h h h h h h h h h h h
            h h h h h h h h h h h h h h h h h h h h
            h h h h h h h h h h h h h h h h h h h h
            h h h h h h h h h h h h h h h h h h h h
            h h h h h h h h h h h h h h h h h h h h

            ----------------------------------------------------------------
            -- Optimized version of the above rules
            --
            -- This is a bit faster than the above rules, but not as fast as the
            -- above rules.
            -- The above rules are optimized for the case where the first argument
            -- is a function that is applied to the second argument.
            -- The optimized version is optimized for the case where the second
            -- argument is a function that is applied to the third argument.
            -- The optimized version is optimized for the case where the third
            -- argument is a function that is applied to the fourth argument.

            ----------------------------------------------------------------
            -- Optimized version of the above rules

            ----------------------------------------------------------------


            type family F a b c d e f g h i j k l m n o p q r s t u v w x y zip :: * where






    pure x = pure x :. pure x
    (f :. g) <*> (x :. y) = (f <*> x) :. (g <*> y)

    threeChannelTransducers :: (Applicative f, Applicative g) => (f :. g) -> (f :. g) -> (f :. g) -> (f :. g)
    threeChannelTransducers f g h = f :. g :. head h :. tail head h :. tail tail h

    infixr 0 ^$
    (^$) :: (Applicative f, Applicative g) => (f :. g) -> (f :. g) -> (f :. g)
    (^$) = threeChannelTransducers




  import Dessin.Violeta.Environment
  import Dessin.Violeta.Monad
  import Dessin.Violeta.MonadTrans
import Control.Lens
import Control.Monad.RWS

-- like $, but the function is a lens from the reader environment with a
-- pure function as its target
infixr 0 ^$
(^$) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Getting (a -> b) r (a -> b) -> a -> m b
lf ^$ a = fmap ($ a) (view lf)

infixr 0 ^=<<.
(^=<<.) :: forall (m :: * -> *) b r a. MonadReader r m =>
  (a -> m b) -> a -> m b
(^=<<.) :: forall a (m :: * -> *) b r s.
  (MonadReader r m, MonadState s m) =>
  Getting (a -> m b) r (a -> m b) -> Getting a s a -> m b
lf ^=<<. la = view lf >>= (use la >>=)
 infixr 0 ^$$.
(^$$.) :: forall (m :: * -> *) b r a. MonadReader r m =>
  (a -> m b) -> a -> m b
(^$$.) :: forall a (m :: * -> *) b r s.

infixr 0 ^=<<$.
(^=<<$) :: forall a (m :: * -> *) b r s.

  (MonadReader r m, MonadState s m) =>
  Getting (a -> m b) r (a -> m b) -> a -> m b
lf ^=<<$ a = view lf >>= ($ a)

infixr 0 ^=<<$.
(^=<<$) :: forall a (m :: * -> *) b r s.
  (MonadReader r m, MonadState s m) =>
  Getting (a -> m b) r (a -> m b) -> a -> m b
lf ^=<<$ a = view lf >>= ($ a)
   {-# INLINABLE lf #-}


-- like $, but the function is a memristive lens from the reader environment with a pure function as its target
infixr 0 ^$$
(^$$) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Lens' r (a -> b) -> a -> m b
   infixr 0 ^$$ = \case
     lf -> a -> fmap ($ a) (view lf)

lf ^$$ a = fmap ($ a) (view lf)
infixr 0 ^$$ = \case
--we need a transducing layer which is eponymous in VioletaBFT as Desssins in the Categorical monad


infixr 0 ^$$$.
(^$$$) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Lens' r (a -> b) -> a -> m b
lf ^$$$ a = fmap ($ a) (view lf)
infixr 0 ^$$$ = \case
lf ^$$$ a = fmap ($ a) (view lf)

-- VioletaBFT implements a Stochastic Foraging function in the VioletaBFTBFT invariant Merkle Signature scheme
-- the Stochastic Foraging function is a function which takes a list of nodes and a random number and returns a node ranked
-- by the random number and the Causet index from EinsteinDB. The Causet index is the index of the node in the list of nodes.
-- With FIDel you can later query your Blocks with IPFS and get the Causet index.


infixr 0 ^$$$$.
(^$$$$) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Lens' r (a -> b) -> a -> m b
lf ^$$$$ a = fmap ($ a) (view lf)
infixr 0 ^$$$$ = \case
lf ^$$$$ a = fmap ($ a) (view lf)

