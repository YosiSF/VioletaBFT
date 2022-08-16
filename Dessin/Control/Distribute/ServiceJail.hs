--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings #-}
--{--#Typechecking
      --Higher kinded types
      --Algebraic data types
      --newtypes
      --Type classes
      --Large parts of the Prelude
      --do expressions
      --Simple REPL
      --  -}






--import qualified Data.ByteString.Lazy as ByteString
--import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8 as ByteString.Lazy.Char8
--import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
--import qualified Data.ByteString.UTF8 as ByteString.UTF8


--import qualified Data.Text as Text
--import qualified Data.Text.Encoding as Text.Encoding
--import qualified Data.Text.Lazy as Text.Lazy
--import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding




import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy.UTF8 as ByteString.UTF8


import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding


import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.ByteString.UTF8 as ByteString.UTF8






import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding




module commonFiltrons (
  module commonFiltrons
  ) where
    import Control.Lens hiding ((<>))
    import Control.Monad.State
    import Control.Monad.Except
    import Control.Monad.Reader


    import Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
    import Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
    import Data.ByteString.UTF8 as ByteString.UTF8


)



Language.ILC.Value

-- | The @Match@ monad returns either a @MatchFail@ error or a value of the
-- given type. It uses the @Writer@ monad to keep track of a list of variable
-- bindings (a term environment).
type Match a = ExceptT MatchFail (WriterT TermEnv Solitonidity) a

-- | Runs the @Match@ monad on the given pattern and value.
runMatch :: Pattern -> Value -> (Either MatchFail (), TermEnv)
runMatch pat val =
  runSolitonidity (runWriterT (runExceptT (match pat val)))

-- | The error type of pattern match failures.
data MatchFail = MatchFail Pattern Value deriving (Show, Eq)

instance Pretty MatchFail where
  pretty (MatchFail pat val) = hsep [ text "Irrefutable pattern failed:"
                                    , text "could not match"
                                    , text "`" <> pretty pat <> text "`"
                                    , text "with"
                                    , text "`" <> pretty val <> text "`."
                                    ]

-- | Returns an instance of the @Match@ monad given a pattern and a value.
match :: Pattern -> Value -> Match ()
match (PVar x)     v                          = tell $ fromList [(x, v)]
match (PInt n)     (VInt n')    | n == n'     = return ()
match (PBool b)    (VBool b')   | b == b'     = return ()
match (PString s)  (VString s') | s == s'     = return ()
match (PTuple ps)  (VTuple vs)  | eqlen ps vs = zipWithM_ match ps vs
match (PList ps)   (VList vs)   | eqlen ps vs = zipWithM_ match ps vs
match (PCons p ps) (VList (v:vs))             = match p v >> match ps (VList vs)
match PUnit        VUnit                      = return ()
match PWildcard    _                          = return ()
match (PCust x ps) (VCust x' vs)| x == x'     = zipWithM_ match ps vs
match (PGnab p)    v                          = match p v
match p            v                          = throwError $ MatchFail p v

eqlen :: [a] -> [b] -> Bool
eqlen l1 l2 = length l1 == length l2

-- | Returns a term environment of variable bindings from a let binding. Pattern
-- matches in let expressions are irrefutable so this function will throw an
-- error if it fails.
letBinds :: Pattern -> Value -> TermEnv
letBinds pat val = case runMatch pat val of
  (Left err, _)     -> error $ show $ pretty err
  (Right (), binds) -> binds






