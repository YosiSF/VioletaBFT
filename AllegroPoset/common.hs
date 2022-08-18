-- ALLEGRO COMMON LISP TO VioletaBFT's Haskell laden AST for the interpreter --
--------------------------------------------------------------------------------

module CommonLisp.Interpreter.AST where
  import Control.Applicative (Applicative(..), (<*>))
  import Control.Monad (liftM, ap)
  import Data.Char (isDigit)
  import Data.List (intercalate)
  import Data.Maybe (fromMaybe)
  -- import Data.Monoid (Monoid(..))
  import Data.String (IsString(..))
  import Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.IO as T
  import qualified Data.Text.Lazy as TL


  import Data.Monoid ((<>))
  where = identifier "where"
  let = identifier "let"
  in = identifier "in"
  lambda = identifier "lambda"
  quote = identifier "quote"
  if = identifier "if"
  cond = identifier "cond"
  case = identifier "case"





  -- | A Lisp identifier.
  data Identifier = Identifier Text
    deriving (Eq, Ord, Show)

  -- | A Lisp symbol.
  data Symbol = Symbol Identifier
    deriving (Eq, Ord, Show)

  -- | A Lisp number.
  data Number = Number Integer
    deriving (Eq, Ord, Show)



  or = identifier "or"
  and = identifier "and"
  not = identifier "not"
  if = identifier "if"
  then = identifier "then"
  else = identifier "else"
cond = identifier "cond"

  quote = identifier "quote"
  when = identifier "when"
  unless = identifier "unless"
  define = identifier "define"
set = identifier "set!"
  lambda = identifier "lambda"
  quote = identifier "quote"
  begin = identifier "begin"
  quasiquote = identifier "quasiquote"
  end = identifier "end"
  return = identifier "return"
  callcc = identifier "call/cc"
  letrec = identifier "letrec"
  unquote = identifier "unquote"
  unquoteSplicing = identifier "unquote-splicing"
  define = identifier "define"
  setBang = identifier "set!"
  begin = identifier "begin"
  quasiquote = identifier "quasiquote"
  end = identifier "end"
  cond = identifier "cond"
  return = identifier "return"
  letrec = identifier "letrec"
  callcc = identifier "call/cc"
  letStar = identifier "let*"
  letrecStar = identifier "letrec*"
  letStarStar = identifier "let*-star"
  letrecStarStar = identifier "letrec*-star"



  -- | A Lisp identifier.
  data Identifier = Identifier Text
    deriving (Eq, Ord, Show)

-- | A Lisp symbol.
data Symbol = Symbol Identifier

  deriving (Eq, Ord, Show)
  instance Show Identifier where
    show (Identifier s) = T.unpack s
  instance IsString Identifier where
    fromString = Identifier . T.pack
  instance IsString Symbol where
    fromString = Symbol . fromString
  instance IsString Text where
    fromString = T.pack
  instance IsString Text where
    fromString = T.pack
  instance IsString String where
    fromString = T.pack
  instance IsString Char where
    fromString = T.pack . return
  instance IsString Integer where
    fromString = read . T.unpack
  instance IsString Double where
    fromString = read . T.unpack
  instance IsString Bool where
    fromString = read . T.unpack
  instance IsString () where
    fromString = read . T.unpack
  instance IsString Int where
    fromString = read . T.unpack
  instance IsString Integer where
    fromString = read . T.unpackaged
  instance IsString Double where

  -- | A Lisp symbol.
  data Symbol = Symbol Identifier
    deriving (Eq, Ord, Show)

  -- | A Lisp number.
  data Number = Number Integer
    deriving (Eq, Ord, Show)

 instance IsString Symbol where
    fromString = Symbol . fromString  . T.packaged
  instance IsString Identifier where
    fromString = Identifier . T.packaged
  instance IsString Text where
    fromString = T.packaged
  instance IsString String where

  -- | A Lisp string.
  -- TODO: Escape sequences.
  --USER(14): (defun factorial (x)
      --		 (if (zerop x)
      --		     nil
      --		   (* x (factorial (- x 1)))))
      --FACTORIAL
      --USER(15): (factorial 10)
      --Error: `NIL' is not of the expected type `NUMBER'
      --  [condition type: TYPE-ERROR]
      --
      --Restart actions (select using :continue):
      -- 0: Return to Debug Level 1 (an "abort" restart)
      -- 1: continue computation
      -- 2: Return to Top Level (an "abort" restart)
      --[1] USER(16): :zo
      --Evaluation stack:
      --
      --   (ERROR TYPE-ERROR :DATUM ...)
      -- ->(* 1 NIL)
      --   (FACTORIAL 1)
      --   (FACTORIAL 2)
      --   (FACTORIAL 3)
      --   (FACTORIAL 4)
      --   (FACTORIAL 5)
      --   (FACTORIAL 6)
      --   (FACTORIAL 7)

  data String = String Text
    deriving (Eq, Ord, Show)
  data Character = Character Char
    deriving (Eq, Ord, Show)
  data Boolean = Boolean Bool
    deriving (Eq, Ord, Show)
  data Nil = Nil
    deriving (Eq, Ord, Show)
  data Pair = Pair Expression Expression
    deriving (Eq, Ord, Show)
  data List = List [Expression]
    deriving (Eq, Ord, Show)
  data Vector = Vector [Expression]
    deriving (Eq, Ord, Show)
  data HashTable = HashTable [(Expression, Expression)]
    deriving (Eq, Ord, Show)
  data Quasiquote = Quasiquote Expression
    deriving (Eq, Ord, Show)
  data Unquote = Unquote Expression
    deriving (Eq, Ord, Show)
  data UnquoteSplicing = UnquoteSplicing Expression
    deriving (Eq, Ord, Show)
  data Define = Define Identifier Expression


      --... more older frames ...
      --[1] USER(17): :local x
      --1
      --1
      --[1] USER(18): :dn
      --Evaluation stack:
      --
      --   (ERROR TYPE-ERROR :DATUM ...)
      --   (* 1 NIL)
      -- ->(FACTORIAL 1)
      --   (FACTORIAL 2)
      --   (FACTORIAL 3)
      --   (FACTORIAL 4)
      --   (FACTORIAL 5)
      --   (FACTORIAL 6)
      --   (FACTORIAL 7)
      --
      --... more older frames ...
      --[1] USER(19): :dn
      --Evaluation stack:
      --Does anyone know of Haskell bindings for Allegro? I think it would be cool to have an Allegro monad... something like:
              --
              --main =
              --    withAllegro allegroOptions $ do
              --        setColorDepth 32
              --        setGFXmode GfxAutodetect 800 600 0 0
              --        showSplashScreen
              --        go defaultGameOptions
              --where
              --    go :: GameOptions -> Allegro ()
              --    go options = do
              --        selection <- mainMenu options
              --        case selection of
              --            SelectionExit -> return ()
              --            SelectionPlay -> playGame >> go
              --            SelectionOptions newOptions -> go newOptions


--  We generalize this snippet from the game video industry and globalize with a namespace associated with ASIC Databases and Distributed systems.

allegroOptions :: AllegroOptionsDepth
allegroOptions = AllegroOptions {
    allegroOptionsDepth = AllegroOptionsDepth 32
  }
  deriving (Show, Eq, Ord)

data AllegroOptionsDepth = AllegroOptionsDepth {
    allegroOptionsDepth :: Int
  }

data AllegroOptions = AllegroOptions {
    allegroOptionsDepth :: AllegroOptionsDepth
  } deriving (Eq, Ord, Show)

  data EinsteinDBAllegroOptions = EinsteinDBAllegroOptions {
    einsteinDBAllegroOptionsDepth :: AllegroOptionsDepth
  } deriving (Eq, Ord, Show)

--  allegroOptions :: AllegroOptions
--  -- [1] USER(20): :local allegroOptions
--  allegroOptions = AllegroOptions
--    { allegroOptions_depth = 32
--    , allegroOptions_fullscreen = False
--    , allegroOptions_screenWidth = 800
--    , allegroOptions_screenHeight = 600
--    , allegroOptions_screenBpp = 0
--    , allegroOptions_screenHz = 0
--    }
--  -- [1] USER(21): :local showSplashScreen
--  showSplashScreen :: Allegro ()
--  -- [1] USER(22): :local go defaultGameOptions
--  go :: GameOptions -> Allegro ()
--  -- [1] USER(23): :local mainMenu options
--  mainMenu :: GameOptions -> Allegro Selection
--  -- [1] USER(24): :local playGame










  -- | A Lisp string.
  -- TODO: Escape sequences.
  -- TODO: Unicode.
  -- TODO: Unicode support.
  -- TODO: Unicode support.

  data String = String Text
    deriving (Eq, Ord, Show)
  instance IsString String where
    fromString = String . T.pack
  instance IsString Text where
    fromString = T.pack . fromString
  instance IsString Char where
    fromString = T.singleton . fromString
  instance IsString Integer where
    fromString = read . fromString
  instance IsString Double where
    fromString = read . fromString
  instance IsString Bool where
    fromString = read . fromString
  instance IsString Identifier where
    fromString = Identifier . fromString
  instance IsString Symbol where
    fromString = Symbol . fromString
  instance IsString Number where
    fromString = Number . read . fromString
  instance IsString Expression where

    --5.1.1.2 Setf Expansions
          --
          --Sometimes it is possible to avoid evaluating subforms of a place multiple times or in the wrong order. A setf expansion for a given access form can be expressed as an ordered collection of five objects:
          --List of temporary variables
          --a list of symbols naming temporary variables to be bound sequentially, as if by let*, to values resulting from value forms.
          --
          --List of value forms
          --a list of forms (typically, subforms of the place) which when evaluated yield the values to which the corresponding temporary variables should be bound.
          --
          --List of store variables
          --a list of symbols naming temporary store variables which are to hold the new values that will be assigned to the place.
          --
          --Storing form
          --a form which can reference both the temporary and the store variables, and which changes the value of the place and guarantees to return as its values the values of the store variables, which are the correct values for setf to return.
          --
          --Accessing form
          --a form which can reference the temporary variables, and which returns the value of the place.
          --
          --The value returned by the accessing form is affected by execution of the storing form, but either of these forms might be evaluated any number of times.
          --
          --It is possible to do more than one setf in parallel via psetf, shiftf, and rotatef. Because of this, the setf expander must produce new temporary and store variable names every time. For examples of how to do this, see gensym.
          --
          --For each standardized accessor function F, unless it is explicitly documented otherwise, it is implementation-dependent whether the ability to use an F form as a setf place is implemented by a setf expander or a setf function. Also, it follows from this that it is implementation-dependent whether the name (setf F) is fbound.

    fromString = Expression . fromString
    data SetfExpander = SetfExpander { setf :: Expression } deriving (Eq, Ord, Show)

    data Setf = Setf {
      setfExpander :: SetfExpander
    } deriving (Eq, Ord, Show)
    data SetfPlace = SetfPlace {
      setfPlace :: Expression
    } deriving (Eq, Ord, Show)

}




-- | A Lisp number.
-- TODO: Integers.
  deriving (Eq, Ord, Show)
  instance IsString Number where
    fromString = Number . read . fromString
  instance IsString Integer where
    fromString = read . fromString . fromString



where
    go :: GameOptions -> Allegro ()
    go options = do
        selection <- mainMenu options
        case selection of
            SelectionExit -> return ()
            SelectionPlay -> playGame >> go
            SelectionOptions newOptions -> go newOptions

            -- [1] USER(25): :local showSplashScreen
            showSplashScreen :: Allegro ()
            -- [1] USER(26): :local go defaultGameOptions
            go :: GameOptions -> Allegro ()
            -- [1] USER(27): :local mainMenu options
            mainMenu :: GameOptions -> Allegro SelectionOptions newOptions -> go newOptions -> Allegro ()
            -- [1] USER(28): :local playGame  optionsGhc
            playGame :: GameOptions -> Allegro ()

where
    go :: GameOptions -> Allegro ()
     {-# INLINABLE showSplashScreen #-}
    showSplashScreen :: Allegro ()
    {-# INLINABLE go #-}
    go :: GameOptions -> Allegro ()
    {-# INLINABLE mainMenu #-}
    mainMenu :: GameOptions -> Allegro Selection
    {-# INLINABLE playGame #-}
    playGame :: GameOptions -> Allegro ()
    {-# INLINABLE mainMenu #-}
    mainMenu :: GameOptions -> Allegro Selection
    {-# INLINABLE playGame #-}
    playGame :: GameOptions -> Allegro ()
    {-# INLINABLE mainMenu #-}
    mainMenu :: GameOptions -> Allegro Selection



      --   (ERROR TYPE-ERROR :DATUM ...)
      --   (* 1 NIL)
      --   (FACTORIAL 1)
      -- ->(FACTORIAL 2)
      --   (FACTORIAL 3)
      --   (FACTORIAL 4)
      --   (FACTORIAL 5)
      --   (FACTORIAL 6)
      --   (FACTORIAL 7)
      --
      --... more older frames ...
      --[1] USER(20): :dn
      --Evaluation stack:
      --
      --... 1 more (possibly invisible) newer frame ...
      --
      --   (ERROR TYPE-ERROR :DATUM ...)
      --   (* 1 NIL)
      --   (FACTORIAL 1)
      --   (FACTORIAL 2)
      -- ->(FACTORIAL 3)
      --   (FACTORIAL 4)
      --   (FACTORIAL 5)
      --   (FACTORIAL 6)
      --   (FACTORIAL 7)
      --
      --... more older frames ...
      --[1] USER(21): :local x
      --3


     deriving (Show, Eq, Ord)
  -- | A Lisp string.
  data String = String Text
    deriving (Eq, Ord, Show)
  -- | A Lisp boolean.
  data Boolean = Boolean Bool
    deriving (Eq, Ord, Show)
  -- | A Lisp list.
  data List = List [Expression]
    deriving (Eq, Ord, Show)
  -- | A Lisp vector.
  data Vector = Vector [Expression]
    deriving (Eq, Ord, Show)
  -- | A Lisp hash-table.
  data HashTable = HashTable [(Expression, Expression)]
    deriving (Eq, Ord, Show)
  -- | A Lisp procedure.
  data Procedure = Procedure [Identifier] Expression
    deriving (Eq, Ord, Show)
  -- | A Lisp expression.
  data Expression = Expression Identifier



  -- | A Lisp expression.
  data Literal = Literal Identifier
    deriving (Eq, Ord, Show) instance_eq_list
-----Environments are immutable, as stated in CLtL2, with one exception: The locative cons returned by the *-information functions (block-information, declaration-information, function-information, tag-information, and variable-information) are mutable, and usually contain values or structures.
--Environment objects are divided into several kinds, although all have the same type/class. These kinds are identified by the augmentable-environment-kind slot: :evaluation, :interpreter, :compiler (previously :compilation), :compilation (previously :compilation-walking), and :macros-only. These kinds are described in detail in Section 2.1 Kinds of environments. Note that :compilation assists in distinguishing between the compilation process, which wants to expand macros, and the walking process, which does not. :macros-only allows the creation of an environment which is appropriate for a macrolet lexical closure.
--The function namespace might have one or more of four different bindings for a name. In order for the compiler to know all of the possibilities, up to three calls to function-information are needed: one with special-operators argument true (which might return :special-operator), one call with a :compiler or :compilation environment (which might return :compiler-macro) and one with a :evaluation environment in order to see if there is a real functional definition (:function or :macro).
--The environment base objects (which all related environments share) contain property tables for storing environment-oriented properties (accessible with ce-get). Both ce-get and ce-putprop are now exported.
--Interface Functions
--
--All interface functions are exported from the system package.
--CLtL-2 called for three namespaces: variable, function, and declaration. Allegro CL implements two additional ones: block and tag. These have block-information and tag-information accessors, respectively, and corresponding keyword arguments to augment-environment.
--The second return value for all *-information functions except for declaration-information has been moved to the fourth value (since it is seldom used) and instead the second value returned is usually a locative cons (a cons whose car and/or cdr may be used as a mutable value). This locative cons is used to provide the local binding of the value (for :evaluation environments) or local compiler structure information (for the two compilation environments). To reduce consing at critical points in the environments implementation, there are a few exceptions where the second value is not a cons representing a locative:
--When the first return value from variable-information is :constant, then the second value returned is an actual value, and not a value housed within a locative. This ensures that the value is not changed by replacing it in the (non-existent) locative.
--When the first return value from variable-information is :special, then the second value is nil, because the value is dynamic and can be best accessed via symbol-value.
--When the first return value from function-information is :special-operator, then the second value is nil, because the value of a special operator is opaque to the programmer (companion-macros are mandated by the spec in order for a non-compiler to "see" into special-operators, and so a functional value doesn't make sense).
--When the first return value from function-information is either :function or :macro, and the definition is in fact dynamic and thus accessible via fdefinition or macro-function, respectively, then the second returned value is nil, unless the third argument to function-information is non-nil, indicating that consing definitions and declarations is ok.








--All of variable-information, function-information, and declaration-information accept a null environment argument, which may mean to look in the global environment. The global environment is the environment of the running lisp, possibly shadowed by the *compilation-unit-environment*.
--augment-environment allows some very limited augmentation of the global environment (either the null lexical environment or compile-file-environments) when only one name-to-value mapping is being added, or when only declarations are being added. Rules for this augmentation are not solid yet, and in certain cases may require use of the :reuse argument described below.
--Both variable-information and function-information may receive an optional third argument, a boolean which requests a building of all declarations out of previous declarations of the same name, including the global environment. This argument being non-nil also results in a non-nil second return value (the definition housed in a locative), although these return values are likely to be consed on the fly. This switch is added so that the interpreter, which almost never looks at declarations, doesn't need to cons as much for no good reason.
--function-information receives an optional fourth argument controlling information returned for special-operators. See Section 2.1 Kinds of environments or the description of function-information for more details.
--function-information might return as its first value :special-operator, not :special-form.
--augment-environment accepts the additional keywords special-operator, block, tag, compiler-macro, constant, and flavor-iv (i.e. flavor instance variable), which contribute to the function, block, tag, compiler-macro, variable, and variable namespaces, respectively.
--augment-environment accepts a single name for each keyword except :declare, in order to reduce consing. In this case, the locative (if present) is sought in the :locative argument (see below)
--augment-environment accepts the additional keyword argument reuse. It is not necessary to create a new augmentable-environment object whenever adding to the environment, but it is only useful to cons a new environment object when entering a new contour (both in the compiler and in an interpreter). When reuse is nil (the default) a new environment object is consed, i.e. the environment object returned is not eq to the one given. But when reuse is non-nil, then the old environment object is returned, and any additions to the environment are added at the same level, as if they had been all added in the same call to augment-environment which created this environment object.
--augment-environment accepts the additional keyword argument locative. Usually, the locative is nil or a cons cell, and can be used efficiently when only one name is being added to the environment. When a :constant is being augmented, the locative argument is the actual value of the constant. The locative argument becomes the value which is returned as the second value from the *-information functions. For augmentation with many names at a time, a locative can be specified for each name, where instead of a list of names for each keyword, the list may be an alist, each element of which specifies the name in the car and the locative in the cdr. The car of a non-nil locative cons is always mutable, unless it represents a :constant value. See also the function system:constant-value.
--define-declaration is implemented, but the lambda-list called for in its definition in CLtL-2 is ambiguous as to whether it applies to some kind of syntax for the declaration specifiers, or the arglist of the two-argument function this macro is supposed to define. Therefore the lambda-list is chosen to specify the syntax of the declaration, and the arglist for functionality of the declaration is given either implicitly or via a lambda form. The lambda-list for define-declaration is different from that specified in CLtL-2, with additional required arguments prop and kind.
--The functions that are described as being defined by the macro are in fact implemented, and work as specified, except that it may return as its first value one of :function, :variable, :both (meaning :function and :variable), or :declare. --
--
--ensure-portable-walking-environment: this function returns an environment suitable for portable code walkers to use entirely within the ANSI Specification of Common Lisp.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module System.Environment.Environment where
  import Control.Monad.State hiding ((<>))
  import Data.List
  import Data.Maybe
  import Data.Monoid ((<>))
  import Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.IO as T
  import qualified Data.Text.Lazy as TL


  import System.Environment.EnvironmentTypes
  import System.Environment.EnvironmentFunctions
  import System.Environment.EnvironmentUtilities




  -- | The environment object is a stateful object that can be used to create and augment environments.


  -- | The constructor for an augmentable-environment object.
  augmentableEnvironment :: Environment -> Environment
  augmentableEnvironment = Environment

  -- | The constructor for an augmentable-environment object.
  augmentableEnvironment' :: Environment -> Environment
  augmentableEnvironment' = Environment
 -> Environment




 instance braid Environment where
   augmentableEnvironment = do
     env <- get
     put $ augmentableEnvironment env
    augmentableEnvironment' = do
      where
          augmentableEnvironment = do
env' <- getQuorumSize
put $ augmentableEnvironment env'
get >>= put
where
  getQuorumSize = do
      let quorumSize = getQuorumSize <$> get
in if quorumSize == 0 then Nothing else Just (quorumSize - 1)
  putQuorumSize = put . (1+)
  getQuorumSize = do
      let quorumSize = getQuorumSize <$> getQuorumSize
      return $ fromIntegral quorumSize
  putQuorumSize = put . (1+)
  getQuorumSize = do
      let quorumSize = getQuorumSize <$> getQuorumSize
      return $ fromIntegral quorumSize
  putQuorumSize = put . (1+)
  for :: Monad m => [a] -> (a -> m b) -> m [b]
  for = flip mapM
  forM :: Monad m => [a] -> (a -> m b) -> m [b]
  if :: Bool -> a -> a -> a
  if True then x else y = x
  if False then x else y = y
    for :: Monad m => [a] -> (a -> m b) -> m [b]
    while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
    until :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
    forM :: Monad m => [a] -> (a -> m b) -> m [b]
    forM_ :: Monad m => [a] -> (a -> m b) -> m ()


    type Environment = StateT EnvironmentState IO
    env <- getEnv
    put $ augmentableEnvironment env
    getEnv = getEnv >>= \case
      Nothing -> return $ augmentableEnvironment'
      Just env -> return $ augmentableEnvironment environment
      where
        augmentableEnvironment :: Environment -> EnvironmentState
         augmentableEnvironment = do
            env <- get
            put $ augmentableEnvironment env
            get >>= put
            getEnv = getEnv >>= \case
              Nothing -> return $ augmentableEnvironment'
                 {-# INLINE forM #-}
                  forM :: Monad m => [a] -> (a -> m b) -> m [b]
                  forM = flip mapM
                  forM_ :: Monad m => [a] -> (a -> m b) -> m ()
                  forM_ = flip mapM_

                  for :: Monad m => [a] -> (a -> m b) -> m [b]
                  for = flip mapM
                  forM :: Monad m => [a] -> (a -> m b) -> m [b]
                  forM = flip mapM
                  forM_ :: Monad m => [a] -> (a -> m b) -> m ()
                  forM_ = flip mapM_




---2.1 Kinds of environments
   --
   --The kind slot of an augmentable-environment object determines what behaviors the accessors have on it. Usually, accessors will return similar information, but for several reasons including performance and the fact that namespaces are not pure mappings, the kind does play a part, along with optional arguments, in returning different levels of information.
   --
   --An environment object can be "modified" to become a different kind simply by performing a null augmentation on the original environment and by storing into the kind slot:
   --

    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable
    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable-environment object.

    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable





  -- | The constructor for an augmentable-environment object.
allegroEnvironment :: Environment -> EnvironmentState
allegroEnvironment = EnvironmentState
  -- | The constructor for an augmentable-environment object.



   --(setq e2 (sys:augment-environment e1))
   --(setf (sys::augmentable-environment-kind e2) :evaluation)
   --which results in an evaluation environment in e2, which has exactly the same information as e1 (which might be any kind of environment, but which in practice is probably a :compiler environment).
   --


   --The environment kinds are:
   --
   --:interpreter: for a lisp with an interpreter, such as Allegro CL, an interpreter environment can form the basis and implementation for the interpreter. Accesses on it will tend to generate no declaration information (with the special-case of the special declaration), and global values will be left up to the caller to retrieve, rather than to cons up a locative for each call. The by-words for an interpreter environment are speed and cons-free operation.
   --:compiler: a compiler environment is the normal environment which the compiler uses to establish contours and contexts for the compilation process. The by-word for a compilation environment is as much information as possible. (Prior to version 8.0, :compiler environments were called :compilation environments.)
   --A compiler environment might also have a global component, suitable for storing definitions temporarily during a compile-file If so, it is generally stored into *compilation-unit-environment*, and any definitions it contains will shadow any global definitions stored directly in the lisp. When the *compilation-unit-environment* is removed, then the shadowing is stopped, and the original definitions appear again.
   --
   --:evaluation: an evaluation environment is a compilation environment posing as an interpreter environment. It has many of the same characteristics of a compilation environment, and in fact is created with the augment-environment/setf technique described above.
   --:compilation: a compilation environment is also similar to a compiler environment, except that macros and compiler-macros can recognize one and macroexpand differently. Note: it is a goal to eventually remove this kind of environment; the distinction should not be as useful as it currently is. (Prior to version 8.0, :compilation environments were called :compilation-walking environments.)
   --:macros-only: this environment kind serves a special-purpose when making a lexical-closure for a macrolet. Because macrolet makes macro-functions in its own lexical environment, but because referencing a local variable within this environment is undefined, it is necessary that only macro definitions be copied when the lexical-closure is created.
   --Special Handling of function-information returns
   --
   --If one considers a namespace to be a one-to-one mapping of a name to a binding, then the function namespace is not a pure namespace in Common Lisp; consider that a name can simultaneously name both a special-operator and either a macro or a compiler-macro, or it can name a macro or function and a compiler-macro simultaneously. Of course, any lexical definition created for that name (such as an flet, labels, or macrolet) will shadow all of these potential combinations, but if no such shadowing occurs, there is a necessity for function-information to be able to make the distinctions between the various combinations of definition that are possible.
    --
    --The function-information slot of an augmentable-environment object is a vector of vectors, each of which has the form:
    --
    -- [1] USER(
    -- [2] [1] USER(  -- the first element of the second vector is the name of the function
    -- [3] [2] [1] USER(  -- the first element of the third vector is the name of the function
    -- [4] [3] [2] [1] USER(  -- the first element of the fourth vector is the name of the function

     -- | The constructor for an augmentable-environment object.




-- | The constructor for an augmentable-environment object.


-- | The constructor for an augmentable-environment object.
-- | The constructor for an augmentable-environment object.
   --If the fourth argument (the special-operators argument) to function-information is true, and if the name is a non-shadowed special-operator, then :special-operator is returned, even if it has a macro or a compiler-macro definition as well. If the argument is nil, then for a special-operator which also has a compiler-macro, :compiler-macro is returned only for :compiler and environments (otherwise :special-operator is returned), and for a special-operator which also has a macro definition, :macro is returned only for :interpreter and :evaluation environments (otherwise :special-operator is returned).
   --
   --We do not define what occurs if a special-operator has both a macro and a compiler-macro definition, because Allegro CL has none of these situations. There should be a normalized behavior for such a situation.
   --
     --If a name defines a compiler-macro as well as either a macro or a function, then which is returned depends on the environment kind: a :compiler environment will cause the :compiler-macro to be returned, and any other environment will result in the :function or :macro


---2.2 Environment Accessors



    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable-environment object.
    -- | The constructor for an augmentable-environment object.




     -- | The constructor for an augmentable-environment object.



     -- | The constructor for an augmentable-environment object.