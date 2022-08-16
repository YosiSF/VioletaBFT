{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Dessin.Violeta.Byzantine
  (
    ByzantineToleranceNumber(..)
  , ByzantineTolerance(..)
  , ByzantineToleranceType(..)

  ) where
      import Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, real
                    , fmap, Show, Read, Eq, Ord, Maybe(..))
      import qualified Prelude as P
      import qualified Data.List as L
      import qualified Data.Text as T

      import qualified Data.Map as M
      import qualified Data.Set as S

      import qualified Data.ByteString.Lazy as BL
      import qualified Data.ByteString.Lazy.Char8 as C



      import qualified Data.Aeson as A
      import qualified Data.Aeson.Types as AT
      import qualified Data.Aeson.Encode as AE
      import qualified Data.Aeson.Parser as AP

      import qualified Data.Vector as V
      import qualified Data.Vector.Mutable as VM
      import qualified Data.Vector.Generic as VG
      import qualified Data.Vector.Generic.Mutable as VGM


      import qualified Data.HashMap.Strict as HM
      import qualified Data.HashSet as HS

      import qualified Data.Text.Encoding as TE
      import qualified Data.Text.Encoding.Error as TEE


      import qualified Data.ByteString.Lazy.UTF8 as UTF8
      import qualified Data.ByteString.Lazy.Char8 as C8

      import qualified Data.ByteString.Lazy.Builder as B
      import qualified Data.ByteString.Lazy.Builder.ASCII as BASCII


      import qualified Data.ByteString.Lazy.Builder.BINARY as BINARY
      import qualified Data.ByteString.Lazy.Builder.Base64 as BASE64
      import qualified Data.ByteString.Lazy.Builder.Hex as HEX
      import qualified Data.ByteString.Lazy.Builder.Int as INT
      import qualified Data.ByteString.Lazy.Builder.RealFloat as REALFLOAT
      import qualified Data.ByteString.Lazy.Builder.Scientific as SCIENTIFIC




      type JSON = A.Value
      type JSONPath = [Int]
      type JSONPathElement = Int
      type JSONPathElementString = T.Text
      type JSONPathElementStringList = [T.Text]


      data ByzantineToleranceNumber = ByzantineToleranceNumber {
          byzantineToleranceNumber :: Double
        } deriving (Show, Read, Eq, Ord)

deriving (Show, Read, Eq, Ord)
data ByzantineTolerance = ByzantineTolerance {
          byzantineToleranceType :: ByzantineToleranceType
        , byzantineToleranceNumber :: ByzantineToleranceNumber
        } deriving (Show, Read, Eq, Ord)




      List
        :: [JSON]
        -> JSON
      List = A.Array . V.fromList
      ListNil
        :: JSON
      ListNil = A.Array V.empty
      ListCons
        :: JSON
        -> [JSON]
        -> JSON
      ListCons x xs = A.Array $ V.fromList $ x : xs
      ListNilCons
        :: JSON
        -> JSON -> JSON
        -> JSONPathElementStringList
        -> JSON
      ListNilCons x xs y ys = A.Array $ V.fromList $ x : xs : y : ys
        List = map (\(A.Number n) -> T.pack $ show n)
        ListNil = []
      List _ = []
      ListCons _ _ = []
       instance Show JSON where
        show = C8.unpack . AE.encode
        showList = C8.unpack . AE.encode . List
        showListNil = C8.unpack . AE.encode . ListNil
        showListCons = C8.unpack . AE.encode . ListCons
        show = C8.unpack . B.toLazyByteString . A.encode
        --show = C8.unpack . B.toLazyByteString . A.encodePretty
      instance Show JSONPathElementString where
        show = T.unpack
         {-# INLINE show #-}
         showList = C8.unpack. B.toLazyByteString. A.en
      instance Show JSONPathElementStringList where
        show = C8.unpack. B.toLazyByteString. A.encode
        show = T.unpack . T.intercalate "."
      instance Show JSONPath where
        show = C8.unpack. B.toLazyByteString. A.encode
        show = T.unpack . T.intercalate "." . map show
      instance Show JSONPathElement where
        show = show . fromIntegral
      instance Show JSONPathElementList where


      data JSONPathElementList = JSONPathElementList {
        jsonPathElementList :: [JSONPathElement]
      } deriving (Show, Eq)
      instance JSONPathElementList JSONPathElement where
        jsonPathElementList = jsonPathElementList
      instance JSONPathElementList JSONPath where
        jsonPathElementList = map fromIntegral
      instance JSONPathElementList JSONPathElementString where
        jsonPathElementList = map T.pack . jsonPathElementList
      instance JSONPathElementList JSONPathElementStringList where
        jsonPathElementList = map T.pack. jsonPathElementList
      instance JSONPathElementList JSONPathElementList where
        jsonPathElementList = jsonPathElementList { lightcone = lightcone }
      instance JSONPathElementList JSONPathElementList where
        jsonPathElementList = jsonPathElementList { lightcone = lightcone }
      instance JSONPathElementList JSONPathElementList where
        jsonPathElementList = jsonPathElementList { lightcone = lightcone }


--|encapsulation of the type of the tolerance
data ByzantineToleranceType = ByzantineToleranceType {
  byzantineToleranceType :: T.Text
} deriving (Show, Eq)


-- | Inference monad
type Infer a = ReaderT TypeEnv (StateT InferState (Except TypeError)) a

ToleranceType :: T.Text -> Infer ByzantineToleranceType
ToleranceType t = do
  return $ ByzantineToleranceType t
    {-# INLINABLE tolerant #-}

where
  tolerant :: Infer ()
  tolerant = do
    t <- asks typeEnv
    lift $ modify $ \s -> s { typeEnv = t }
    return ()
    {-# INLINABLE tolerant #-}
      tolerant = do
          t <- asks typeEnv
          lift $ modify $ \s -> s { typeEnv = t }
          return ()
          {-# INLINABLE tolerant #-}

-- | Inference state
newtype InferState = InferState { count :: Int }

  deriving (Show, Eq)
  {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (ByzantineToleranceType)
tolerations = M.fromList [("tolerant", ByzantineToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
{-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
  ByzantineToleranceType)
tolerations = M.fromList [("tolerant", ByzantineToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
{-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType
tolerant t = do
  case M.lookup t tolerations of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Unknown tolerance type: " <> t
    {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
      ByzantineToleranceType)
    )
tolerations = M.fromList [("tolerant", ByzantineToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
{-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType
tolerant t = do
  case M.lookup t tolerations of
    Just t -> return t
    Nothing -> throwError $ TypeError $ "Unknown tolerance type: " <> t
    {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
      ByzantineToleranceType)
    ) where
      tolerations = M.fromList [("tolerant", ByzantineToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
      {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
        ByzantineToleranceType)
      ) where
        tolerations = M.fromList [("tolerant",ktopToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
        {-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType
     {-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType
     tolerant t = do
       case M.lookup t tolerations of
           Just t' -> return t'
       case M.lookup t tolerations of
         Just t -> return t
          Nothing -> throwError $ TypeError $ "Unknown tolerance type: " <> t
          {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
            ByzantineToleranceType)
          )
        tolerations = M.fromList [("tolerant", ByzantineToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
        {-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType
          {-# INLINABLE tolerant #-} tolerations :: M.Map T.Text (
            ByzantineToleranceType)
tolerations = M.fromList [("tolerant",ktopToleranceType "tolerant"), ("strict", ByzantineToleranceType "strict")]
          )
          }
          {-# INLINABLE tolerant #-} tolerant :: T.Text -> Infer ByzantineToleranceType

)

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

-- | Run the inference monad
runInfer :: TypeEnv
         -> Infer (Type, [Constraint], TypeEnv)
         -> Either TypeError (Type, [Constraint], TypeEnv)
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer


-- | Run the inference monad
--
--  The result is a pair of the inferred type and the constraints.
-- The type environment is the resulting environment after type inference.
--
-- The result is a pair of the inferred type and the constraints.

runInfer' :: TypeEnv
  -> Infer (Type, [Constraint], TypeEnv)
  -> (Type, [Constraint], TypeEnv)
runInfer' env m = case runInfer env m offline
  of Left err -> error $ show err
      Right (t, cs, env') -> (t, cs, env')
  where offline = Offline
    runInfer' env m = runExceptT (evalStateT (runReaderT  m env) initInfer) >>= \case_expr
      Left err -> error $ show err
      Right (t, cs, env') -> (t, cs, env')))'
    where offline = Offline) $ trace ("reader
      {-# INLINABLE runInfer' #-} runInfer' :: TypeEnvironment
      -> Infer (Type, [Constraint], TypeEnvironment)
      -> (Type, [Constraint], TypeEnvironment)
          -> Infer (Type, [Constraint], TypeEnv)
runInfer' env m = case runInfer env m of
  Left err -> error $ show err
  Right (t, cs, env') -> (t, cs, env')
  where
    (t, cs, env') = runInfer' env m
    {-# INLINABLE runInfer' #-}




-- | Run the inference monad
-- The result is a pair of the inferred type and the constraints.
-- The type environment is the resulting environment after type inference.
-- The result is a pair of the inferred type and the constraints.
--





-- | Solve for type of toplevel expression in a given environment
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err          -> Left err
  Right (ty, cs, _) -> case runSolve cs of
    Left err    -> Left err
    Right subst -> Right (closeOver $ apply subst ty)

-- | Return internal constraints used in solving for type of expression
--constraintsExpr :: TypeEnv
--                -> Expr
--                -> Either TypeError ([Constraint], Subst, Type, Scheme)
--constraintsExpr env ex = case runInfer env (infer ex) of
--  Left       err -> Left err
--  Right (ty, cs, _) -> case runSolve cs of
--    Left err    -> Left err
--    Right subst -> Right (cs, subst, ty, sc)
--      where sc = closeOver $ apply subst ty

localc :: TypeEnv -> Infer a -> Infer a
localc env = local (const env)


-- | Solve for type of toplevel expression in a given environment
-- and return internal constraints used in solving for type of expression
-- constraintsExpr :: TypeEnv
--               -> Expr
--              -> Either TypeError ([Constraint], Subst, Type, Scheme)
-- constraintsExpr env ex = case runInfer env (infer ex) of
--  Left err -> Left err
-- Right (ty, cs, _) -> case runSolve cs of
-- Left err    -> Left err
-- Right subst -> Right (cs, subst, ty, sc)
-- where sc = closeOver $ apply subst ty
-- constraintsExpr env ex = case runInfer env (infer ex) of
-- Left err -> Left err
-- Right (ty, cs, _) -> case runSolve cs of
-- Left err    -> Left err
-- Right subst -> Right (cs, subst, ty, sc)
-- where sc = closeOver $ apply subst ty
-- constraintsExpr env ex = case runInfer env (infer ex) of
-- Left err -> Left err
-- Right (ty, cs, _) -> case runSolve cs of
-- Left err    -> Left err

--
--



-- | Set up interpreter in IO monad (for filling and taking from mvars).
type Interpreter t = IO t

-- | Evaluating EError throws EvalError
newtype EvalError = EvalError String deriving (Typeable)

instance Exception EvalError

instance Show EvalError where
  show (EvalError s) = "Exception: " ++ s
  instance Exception EvalError
  instance Show EvalError where
  show (EvalError s) = "Exception: " ++ s


-- | Evaluates an expression to a value and puts it into the given MVar.
evalPut :: TermEnv -> MVar Value -> Expr -> Interpreter ()
evalPut env mv ex = do
  val <- eval env ex
  putMVar mv val
  return ()

-- | Evaluates an expression to a value and puts it into the given MVar.
evalPut' :: TermEnv -> MVar Value -> Expr -> Interpreter ()
evalPut' env mv ex = do
  val <- eval env ex
  putMVar mv val
  return ()


-- | Evaluates an expression to a value and puts it into the given MVar.
evalPut'' :: TermEnv -> MVar Value -> Expr -> Interpreter ()
evalPut'' env mv ex = do
  val <- eval env ex
  putMVar mv val
  return ()

evalPut env m expr = case expr of
  EVar x -> do
    let v = case Map.lookup x env of
              Just val -> val
              Nothing  -> error $ "evalPut: EVar: Unbound variable " ++ x
    putMVar m v

  EConstant c -> putMVar m c
  _ -> error $ "evalPut: EVar: Unbound variable " ++ x
  putMVar m v


evalPut' env m expr = case expr of
  EVar x -> do
    let v = case Map.lookup x env of
              Just val -> val
              Nothing  -> error $ "evalPut': EVar: Unbound variable " ++ x
    putMVar m v

  EConstant c -> putMVar m c
  _ -> error $ "evalPut': EVar: Unbound variable " ++ x
  putMVar m v

  ELit lit -> putMVar m little
  EApp e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case v1 of
      VFun f -> f v2
      _ -> error $ "evalPut: EApp: Not a function: " ++ show v1
  ELit (LInt n)    -> putMVar m $ VInt n
  ELit (LBool b)   -> putMVar m $ VBool b
  ELit (LChar c)   -> putMVar m $ VChar c
  ELit (LString s) -> putMVar m $ VString s
  ELit (LFloat f)  -> putMVar m $ VFloat f
  ELit (LDouble d) -> putMVar m $ VDouble d
  ELit (LUnit)     -> putMVar m $ VUnit
  ELit (LString s) -> putMVar m $ VString s
  ELit LUnit       -> putMVar m VUnit
  ELit (LInt n)    -> putMVar m $ VInt n
  ELit (LLong n) -> putMVar m $ VLong n
  ELit (LFloat f)  -> putMVar m $ VFloat f
  ELit (LDouble d) -> putMVar m $ VDouble d
  ELit (LChar c)   -> putMVar m $ VChar c
  ELit (LString s) -> putMVar m $ VString s


  ETuple es -> do
    res <- evalList env es
    putMVar m $ VTuple res

  ERecord fs -> do
    res <- evalRecord env fs
    putMVar m $ VRecord res
    putMVar m $ VRecord fs
    return res
  EGet e1 e2 -> do
    res <- evalRecord env e1
    putMVar m $ res
    putMVar m $ res
    return res
  ESet e1 e2 e3 -> do
    res <- evalRecord env e1
    putMVar m $ res
    putMVar m $ res
    return res
  EApp e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case v1 of
      VFun f -> f v2
      _ -> error $ "evalPut': EApp: Not a function: " ++ show v1
  EIf e1 e2 e3 -> do
    v1 <- eval env e1
    case v1 of
      VBool True -> eval env e2
      VBool False -> eval env e3
      _ -> error $ "evalPut': EIf: Not a boolean: " ++ show v1
  ELet x e1 e2 -> do
    v1 <- eval env e1
    eval (Map.insert x v1 env) e2
  ELetRec x e1 e2 -> do
    v1 <- eval env e1
    eval (Map.insert x v1 env) e2
  ELetTuple xs e1 e2 -> do
    v1 <- eval env e1
    eval (Map.union (Map.fromList (zip xs v1)) env) e2


closeOver :: Type -> Scheme
closeOver = normalize . generalize emptyTyEnv

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = removeTyEnv e x `extendTyEnv` (x, sc)
  local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  TypeEnv env <- ask
  case Map.lookup x env of
    Nothing  -> throwError $ UnboundVariable x
    Just ty  -> instantiate ty

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: (String -> a) -> Infer a
fresh f = do
  s <- get
  put s{count = count s + 1}
  return $ f (letters !! count s)

freshi :: Infer Type
freshi = fresh (IType . IVar . TV)

fresha :: Infer Type
fresha = fresh (AType . AVar . TV)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const freshi) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type-> Scheme -- ^ T-Gen
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

binops :: Binop -> Infer Type
binops op = case op of
  Add -> tyArithOp
  Sub -> tyArithOp
  Mul -> tyArithOp
  Div -> tyArithOp
  Mod -> tyArithOp
  And -> tyLogOp
  Or  -> tyLogOp
  Lt  -> tyRelOp
  Gt  -> tyRelOp
  Leq -> tyRelOp
  Geq -> tyRelOp
  Eql -> eqBinOp
  Neq -> eqBinOp
  _   -> error "Infer.binops"
  where
    tyArithOp = return $ IType (IArr tyInt (IType (IArr tyInt (IType tyInt))))
    tyLogOp = return $ IType (IArr tyBool (IType (IArr tyBool (IType tyBool))))
    tyRelOp = return $ IType (IArr tyInt (IType (IArr tyInt (IType tyBool))))
    eqBinOp = do
      t1 <- fresh (IVar . TV)
      t2 <- fresh (IVar . TV)
      return $ IType (IArr t1 (IType (IArr t2 (IType tyBool))))
.PrettyPrint.ANSI.Leijen

-- | Pretty prints a comma separated list
-- TODO: Fix doc with to 80
prettyTuple :: [Doc] -> Doc
prettyTuple xs = encloseSep lparen rparen comma xs

-- | No line breaks
_prettyTuple :: Pretty a => [a] -> Doc
_prettyTuple xs = parens $ hcat $ intersperse comma $ map pretty xs

-- | Pretty prints a comma separated list
prettySet :: [Doc] -> Doc
prettySet xs = encloseSep lbrace rbrace comma xs

-- | Pretty prints a space separated list
prettySpace :: [Doc] -> Doc
prettySpace xs = encloseSep empty empty space xs

-- | Enclose a 'Doc' in parens if the flag is 'True'
maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id
Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
