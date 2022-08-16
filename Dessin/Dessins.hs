{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where
  import Control.Monad (void)
  import Control.Concurrent (threadDelay)
  import Control.Concurrent.Async (async, wait)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
  import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)


  import Data.Aeson (FromJSON, ToJSON)
  import Data.Aeson.Types (parseEither)
  import Data.ByteString.Lazy (ByteString)


  import qualified Data.ByteString.Lazy as BL
  import qualified Data.ByteString.Lazy.Char8 as BLC
  import qualified Data.ByteString.Lazy.UTF8 as BLU


  import qualified Data.Text as T
  import qualified Data.Text.Encoding as T
  import qualified Data.Text.IO as T
  import qualified Data.Time.Clock as TM



  import qualified Network.HTTP.Client as HC
  import qualified Network.HTTP.Client.TLS as HC
  import qualified Network.HTTP.Types as HT
  import qualified Network.HTTP.Types.Header as HT
  import qualified Network.HTTP.Types.Status as HT


  import qualified System.IO as IO
  import qualified System.IO.Error as IO
  import qualified System.IO.Unsafe as IO

  import qualified System.Random as R

  import qualified System.Process as P






  --We implement a mock HLC for testing purposes.
  --We use a TMVar to store the current time.
  --We use a TVar to store the current random number.

data MockHLC = MockHLC {
    mockHLCTime :: TM.UTCTime,
    mockHLCRandom :: TVar Integer
  }

  instance MonadIO MockHLC where
    liftIO = MockHLC <$> liftIO TM.getCurrentTime <*> liftIO (newTVarIO =<< R.randomIO)
    {-# INLINE mockHLCTime #-}
    {-# INLINE mockHLCRandom #-}


  --We use a TMVar to store the current time.

  mockHLCTime :: MockHLC -> TM.UTCTime
  mockHLCTime = mockHLCTime
  {-# INLINE mockHLCTime #-}

  mockHLCRandom :: MockHLC -> TVar Integer
  mockHLCRandom = mockHLCRandom
  {-# INLINE mockHLCRandom #-}




ser TopDecl
{-# INLINE mockHLCRandom #-}
dExpr = Decl "it" <$> expr
{-# INLINE mockHLCRandom #-}




ser TopDecl
{-# INLINE mockHLCRandom #-}

parseLet :: Parser (Name, [Pattern], Expr)
parseLet = do
  reserved "let"
  name <- identifier
  patterns <- many pattern
  reservedOp "="
  expr <- expr
  return (name, patterns, expr)
{-# INLINE parseLet #-}
 {-# INLINE parseLet #-}
  {-# INLINE parseLet #-}
   {-# INLINE parseLet #-}
    {-# INLINE parseLet #-}
     {-# INLINE parseLet #-}



parseLet = do
  x <- identifier
  ps <- many pat
  reserved "="
  e <- expr
  return (x, ps, e)

dDeclLetRec :: Parser TopDecl
dDeclLetRec = do
  reserved "letrec"
  (x, ps, e) <- parseLet
  return $ Decl x (EFix x $ foldr ELam e ps)


{-# INLINE parseLet #-}


dDeclFun :: Parser TopDecl
dDeclFun = do
  reserved "let"
  (x, ps, e) <- parseLet
  return $ Decl x (foldr ELam e ps)

-}


dDeclCon :: Parser TopDecl
dDeclCon = do
  reserved "data"
  tyCon <- constructor
  _ <- many identifier
  reservedOp "="
  valCons <- sepBy1 (parseValCon tyCon) (reservedOp "|")
  return $ TyCon tyCon valCons


dDecl :: Parser TopDecl
dDecl = dDeclCon <|> dDeclFun <|> dDeclLetRec



parseValCon :: Name -> Parser ValCon
parseValCon tyCon = do
  con <- constructor
  args <- many pattern
  return $ ValCon con args


parseTopDecl :: Parser TopDecl
parseTopDecl = dDecl <|> dExpr


parseTopDecls :: Parser [TopDecl]
parseTopDecls = many parseTopDecl


parseFile :: FilePath -> IO [TopDecl]
parseFile path = do
  input <- BL.readFile path
  case parse parseTopDecls path input of
    Left err -> fail $ show err
    Right topDecls -> return topDecls

  valCon <- constructor
  params <- sepBy ty whitespace
  return $ TyCon valCon params

  let ps = map stripi params
    ps' = filter (/= '\n') ps
    ps'' = filter (/= '\r') ps'
  return (valCon, foldr (\a b -> IType (IArr a b)) (IType (ISend (SCon tyCon))) ps)
    in if null ps'
        parseTopDecls :: FilePath -> IO [TopDecl]
          return $ ValCon valCon ps'
              parseTopDecls :: FilePath -> IO [TopDecl]
                return $ ValCon valCon ps'
                    parseTopDecls :: FilePath -> IO [TopDecl]
                       parseTopDecls file = do
  input <- BL.readFile file

  case parse parseTopDecls file input of
    Left err -> fail $ show err
    Right topDecls -> return topDecls

  valCon <- constructor
  params <- sepBy ty whitespace

  return $ TyCon valCon params


  let ps = map stripi params
    ps' = filter (/= '\n') ps
    ps'' = filter (/= '\r') ps'


  return (valCon, foldr (\a b -> IType (IArr a b)) (IType (ISend (SCon tyCon))) ps)
    in if null ps'
        parseTopDecls :: FilePath -> IO [TopDecl]
          return $ ValCon valCon ps'
              parseTopDecls :: FilePath -> IO [TopDecl]
                return $ ValCon valCon ps'
                    parseTopDecls :: FilePath -> IO [TopDecl]
                       parseTopDecls file = do



  input <- BL.readFile file
  case parse parseTopDecls file input of
    Left err -> fail $ show err
    Right topDecls -> return topDecls

  valCon <- constructor
  params <- sepBy ty whitespace

  return $ TyCon valCon params










  let ps = map stripi params
    ps' = filter (/= '\n') ps
    ps'' = filter (/= '\r') ps'




      return (valCon, foldr (\a b -> IType (IArr a b)) (IType (ISend (SCon tyCon))) ps)
    in if null ps'
        parseTopDecls :: FilePath -> IO [TopDecl]
          return $ ValCon valCon ps'
              parseTopDecls :: FilePath -> IO [TopDecl]
                return $ ValCon valCon ps'
                    parseTopDecls :: FilePath -> IO [TopDecl]
                       parseTopDecls file = do



  causet :: Parser TopDecl
causet = do
  reserved "causet"
  (x, ps, e) <- parseLet
  return $ Decl x (EFix x $ foldr ELam e ps)
    where

  | not (null e) = fail $ show e
  (x, ps, e) = maybe (fail $ show e) (const True) $ parseLet

    (x, ps, e) <- parseLet
    parseLet :: Parser TopDecl
    parseLet = do
      e <- parseExpr
       parseExpr :: Parser Expr


  parseExpr :: Parser Expr
  parseExpr = parseExpr' <|> parseExpr''
  where
    parseExpr' = do
      e <- parseExpr''
      return $ EFix x e1 e2 -> do
        e1 <- parseExpr''
        e2 <- parseExpr''
        return $ EFix x e1 e2
in for all xs . xs -> xs




parseExpr :: Parser Expressions -> Parser Expressions
parseExpr = parseExpr' <|> parseExpr''
  where
    parseExpr' = do
      e <- parseExpr''
      return $ EFix x e1 e2 -> do
        e1 <- parseExpr''
        e2 <- parseExpr''
        return $ EFix x e1 e2 -> do
    return $ Decl x (EFix x $ foldr ELam e ps)
    parseExpr = do
      e' <- parseExpr
    (x, ps, e) <- parseLet
    parseLet :: Parser TopDecl
    return $ Decl x (EFix x $ foldr ELam e ps) (<|> causet <|> causet <|> causet )
    parseExpr = do
      e' <- parseExpr
    (x, ps, e) <- parseLet
    parseLet :: Parser TopDecl
          where

            e = case e of  Just e -> e
                          Nothing -> fail $ show error "seqIndex: index out of bounds"

              ELam x e -> ELam x e
              ELet x e1 e2 -> ELet x e1 e2
              EFix x e -> EFix x e1 e2 -> do
                e1 <- parseExpr
                e2 <- parseExpr
                return $ EFix x e1 e2

                parseExpr :: Parser Expr
                parseExpr = do
                  e <- parseExpr' <|> parseExpr''
                  case e of
                    Just e -> e
                    Nothing -> fail $ show error "seqIndex: index out of bounds"
                    ELam x e -> ELam x e
                    ELet x e1 e2 -> ELet x e1 e2

                  e' <- parseExpr
                (x, ps, e) <- parseLet
                parseLet :: Parser TopDecl
                return $ Decl x (EFix x $ foldr ELam e ps)
                parseExpr = do
                  e' <- parseExpr
                (x, ps, e) <- parseLet
                case e of
                  Just e -> e
                  Nothing -> fail $ show error "seqIndex: index out of bounds"
                  ELam x e -> ELam x e
                  ELet x e1 e2 -> ELet x e1 e2
                  _ -> fail $ show error "seqIndex: index out of bounds"

                parseLet :: Parser TopDecl
                return $ Decl x (EFix x $ foldr ELam e ps)
                parseExpr = do
                  e' <- parseExpr

              _ -> return ()
            ca = case ca of
              CLam x c -> CLam x c
              _ -> return ()


              (x, ps, e) <- parseLet
              parseLet :: Parser TopDecl
              return $ Decl x (EFix x $ foldr ELam e ps)
              parseExpr = do
                e' <- parseExpr





_ -> return ()



decl :: Parser TopDecl
decl = dDeclCon <|> dDeclLetRec <|> try dExpr <|> dDeclFun


pVar :: Parser Pattern
pVar = mklexer PVar identifier

pInt :: Parser Pattern
pInt = mklexer PInt integer



pCon :: Parser Pattern
pCon = mklexer PCon constructor


pWild :: Parser Pattern
pWild = mklexer PWild (reserved "wild")


pTuple :: Parser Pattern
pTuple = do
  reserved "tuple"
  ps <- parens $ sepBy pPattern comma
  return $ PTuple ps

pPattern :: Parser Pattern
pPattern = pVar <|> pInt <|> pCon <|> pWild <|> pTuple


pExpr :: Parser Expr
pExpr = do
  reserved "expr"
  e <- parens pExpr
  return $ EExpr e

pLet :: Parser Expr
 parseLit = do
     reserved "lit"
      lit <- parens pLit
      return $ ELit lit



pBool :: Parser Pattern
pBool = pTrue <|> pFalse
  where
    pTrue = reserved "true" >> return (PBool True)
    pFalse = reserved "false" >> return (PBool False)

pString :: Parser Pattern
pString = mklexer PString stringLit

pUnit :: Parser Pattern
pUnit = reserved "()" >> return PUnit


pLit :: Parser Lit
pLit = pBool <|> pString <|> pUnit


pExpr :: Parser Expr
pExpr = do
  e <- parens pExpr
  return $ EExpr expressions
    <|> parseLiterals

pExpr :: Parser Expr
pExpr = do
  e <- parens pExpr
  return $ EExpr expressions
    <|> parseLiterals



pExpr :: Parser Expr
pExpr = do
  e <- parens pExpr
  return $ EExpr expressions
    <|> parseLiterals







pWildcard :: Parser Pattern
pWildcard = reserved "_" >> return PWildcard

pTuple :: Parser Pattern
pTuple = mklexer PTuple $ parens $ commaSep2 pat

pList :: Parser Pattern
pList = mklexer PList $ brackets $ commaSep pat

pCons :: Parser Pattern
pCons = do
  hd <- pat'
  _  <- colon
  PCons hd <$> pat

pCust :: Parser Pattern
pCust = do
  con <- constructor
  ps <- many pat
  return $ PCust con ps

pGnab :: Parser Pattern
pGnab = do
  reservedOp "!"
  PGnab <$> pat

pat' :: Parser Pattern
pat' = pVar
  <|> pInt
  <|> pBool
  <|> pString
  <|> try pUnit
  <|> pWildcard
  <|> try pTuple
  <|> pList
  <|> pCust
  <|> pGnab
  <|> parens pat

pat :: Parser Pattern
pat = try pCons <|> pat'





pExpr :: Parser Expr
pExpr = do
  e <- parens pExpr
  return $ EExpr expressions
    <|> parseLiterals

pExpr :: Parser Expr
pExpr = do
  e <- parens pExpr
  return $ EExpr expressions
    <|> parseLiterals


