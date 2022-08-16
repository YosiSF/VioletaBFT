{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


--The above pseudocode is useful for comparing our algorithm with other boundary matrix algorithms in the literature, but before we give an implementation in Futhark we present (an example of) algorithm 5 in conceptual Futhark code. Assume we have a type matrix. Then we may do the clearing with a function clear of type matrix → matrix that sets the appropriate columns to zero.
  --For representing L, we may define a lookup table arglows such that arglows[i] = j iff j is the minimum column index such that low∂(j) = i, otherwise arglows[i] = −1; we may build arglows with a function leftmost_lookup of type matrix → []i64. This way, assuming ∂j ̸= 0, if we have arglows[low∂(j)] = j, then we know that j ∈ L; otherwise, arglows[low∂(j)] < j and j ∈ N(arglows[low∂(j)]). With this in mind, convergence can be checked by verifying that every column is either zero or in L.
  --For representing U, we may define a lookup table argups such that argups[i] = j iff j is the minimum column index such that up∂(j) = i, otherwise argups[i] = −1; we may build argups with a function rightmost_lookup of type matrix → []i64. This way, assuming ∂j ̸= 0, if we have argups[up∂(j)] = j, then we know that j ∈ U; otherwise, argups[up∂(j)] > j and j ∈ N(argups[up∂(j)]). With this in mind, convergence can be checked by verifying that every column is either zero or in U.
  --If there are no more columns to look at, then it is an error to call this function again.
  --We may use the above functions to build the boundary matrices L and U.
  --We may use the above functions to build the boundary matrices L and U.




import Violet.MatrixElement
import Violet.Matrix

import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.,
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool{-,
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool-}
  ) where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool) where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(

  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool) where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(




  );{ -# LANGUAGE OverloadedStrings #-}
  import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool) where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(_,_) -> FibTree  where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  )}
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool) where
import qualified Data.Vector as VectorSpace (Vector, fromList, length, (//)) ( view :: VectorSpace.Vector a -> a),(
  view :: VectorSpace.Vector a -> a -> VectorSpace.Vector a -> VectorSpace.Vector a, Eq (Element a) => Matrix a -> Matrix a -> Bool) where












import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
where
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVectorSpace


import qualified Data.Vector.Unboxed as VectorSpace
import qualified Data.Vector.Unboxed.Mutable as MutableVectorSpace



type Matrix = VectorSpace.Vector MatrixElement
type MatrixElement = Int -> Int ^ 2 {
type Matrix = VectorSpace.Vector MatrixElement
  clear :: Matrix -> Matrix,
  leftmost_lookup :: Matrix -> [Int],
  rightmost_lookup :: Matrix -> [Int],
  build_L :: Matrix -> Matrix,
  build_U :: Matrix -> Matrix,
  check_convergence :: Matrix -> Matrix -> Bool,
  build_L :: Matrix -> Matrix,
  build_U :: Matrix -> Matrix,
  check_convergence :: Matrix -> Matrix -> Bool,
  build_L :: Matrix -> Matrix,
  build_U :: Matrix -> Matrix,
  check_convergence :: Matrix -> Matrix -> Bool,

  rightmost_lookup :: Matrix -> [Int],
  build_boundary_matrices :: Matrix -> (Matrix, Matrix),
  verify_convergence :: Matrix -> Bool
}




-- Circle Circle


CircleJson :: String
CircleJson = "{\"type\":\"circle\",\"center\":{\"x\":0,\"y\":0},\"radius\":1}"
CircleJsonToCephRook :: StringLite -> CircleJson -> CircleJsonToCephRook
CircleJsonToCephRook = CircleJsonToCephRook
CircleJsonToCephRook :: StringLite -> CircleJson -> CircleJsonToCephRook



{ --The above pseudocode is useful for comparing our algorithm with other boundary matrix algorithms in the literature, but before we give an implementation in Futhark we present (an example of) algorithm 5 in conceptual Futhark code. Assume we have a type matrix. Then we may do the clearing with a function clear of type matrix → matrix that sets the appropriate columns to zero.
  --For representing L, we may define a lookup table arglows such that arglows[i] = j iff j is the minimum column index such that low∂(j) = i, otherwise arglows[i] = −1; we may build arglows with a function leftmost_lookup of type matrix → []i64. This way, assuming ∂j ̸= 0, if we have arglows[low∂(j)] = j, then we know that j ∈ L; otherwise, arglows[low∂(j)] < j and j ∈ N(arglows[low∂(j)]). With this in mind, convergence can be checked by verifying that every column is either zero or in L.
  --For representing U, we may define a lookup table argups such that argups[i] = j iff j is the minimum column index such that up∂(j) = i, otherwise argups[i] = −1; we may build argups with a function rightmost_lookup of type matrix → []i64. This way, assuming ∂j ̸= 0, if we have argups[up∂(j)] = j, then we know that j ∈ U; otherwise, argups[up∂(j)] > j and j ∈ N(argups[up∂(j)]). With this in mind, convergence can be checked by verifying that every column is either zero or in U.
  --If there are no more columns to look at, then it is an error to call this function again.
  --We may use the above functions to build the boundary matrices L and U.
  --We may use the above functions to build the boundary matrices L and U. --} deriving
  --(Eq, Show) instance_eq_list
  --instance_eq_list :: (Eq a) => [a] -> [a] -> Bool
  --instance_eq_list [] [] = True
  --instance_eq_list (x:xs) (y:ys) = (x == y) && instance_eq_list xs ys

  type MatrixT = g matrix where
    matrix :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> MatrixToShards
    {-# INLINE matrix #-}
    matrix n (i, j) (k, l) (m, n) = MatrixToShards n (i, j) (k, l) (m, n)
    {-# INLINE matrix #-}
    g :: Matrix -> Matrix
    g matrix = matrix
    {-# INLINE g #-}

    --Now do a homology baaed off of the antisymmetric diagonal matrix.
    --We can do this by computing the antisymmetric diagonal matrix and then computing the homology of the antisymmetric diagonal matrix.
    --We can do this by computing the antisymmetric diagonal matrix and then computing the homology of the antisymmetric diagonal matrix.  -# INLINE g #-}


    --Let's begin with a transpose of a matrix. Assumed to be a square matrix.
    --For Non Square Matrices we should establish a Young Tableaux

    youngTableaux :: Matrix -> MatrixToShards
    youngTableaux matrix = matrixToShards matrix
    {-# INLINE youngTableaux #-}




module Main where
  import Control.Monad (void)

  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO

  import qualified Data.ByteString.Lazy as BSL
  import qualified Data.ByteString.Lazy.Char8 as BSL8


  import qualified Data.Aeson as Aeson
  import qualified Data.Aeson.Types as Aeson

  import qualified Data.HashMap.Strict as HM
  import qualified Data.Vector as V

  import qualified Data.Scientific as Sci


  import qualified Data.Time.Clock as Clock
  import qualified Data.Time.Calendar as Calendar


  {-# LANGUAGE OverloadedStrings #-}
  import qualified Data.Text.Encoding as TEnc
  import qualified Data.ByteString.Base64 as Base64

  import qualified Data.ByteString.Lazy.Char8 as BSL8








  -- import qualified Data.ByteString.Lazy.Char8 as BSL8
  import qualified Data.ByteString.Lazy as BSL
  import qualified Data.ByteString.Lazy.UTF8 as UTF8
  import qualified Data.ByteString.Lazy.Builder as BSB




  -- Now we can use the BSL8.putStrLn function to print out the JSON data.
  -- BSL8.putStrLn $ Aeson.encode $ Aeson.object [("name", Aeson.String "John Doe"), ("age", Aeson.Number $ Sci.scientific 10 0)]
  -- BSL8.putStrLn $ Aeson.encode $ Aeson.object [("name", Aeson.String "John Doe"), ("age", Aeson.Number $ Sci.scientific 10 0), ("address", Aeson.object [("street", Aeson.String "123 Main St"), ("city", Aeson.String "Anytown"), ("state", Aeson.String "CA")])]



  -- import qualified Data.ByteString.Lazy.Char8 as BSL8
  import qualified Data.ByteString.Lazy as BSL
  import qualified Data.ByteString.Lazy.UTF8 as UTF8
  import qualified Data.ByteString.Lazy.Builder as BSB


  import qualified Data.ByteString.Lazy.Char8 as BSL8
  import qualified Data.ByteString.Lazy as BSL


  import qualified Data.ByteString.Lazy.Char8 as BSL8


-- | Names in ILC.
type Name = String




-- | A type for a person.
-- data Person = Person {-# LANGUAGE
--  OverloadedStrings #-}
-- { name :: Name
-- , age :: Int
-- , address :: Address
-- }
-- deriving (Show)
--
--  dim :: Integer
-- dim = 2022
--
--
--


--
data Person = Person {
  name :: Name
, age :: Int
, address :: Address
}
deriving (Show)


-- | A type for an address.
-- data Address = Address {
--  street :: String
-- , city :: String
-- , state :: String
-- }
-- deriving (Show)
-- data Address = Address {
-- street :: String
-- , city :: String
-- , state :: String
-- }


data Address = Address {
  street :: String
, city :: String
, state :: String
}
deriving (Show)




-- | A type for a person.

-- | Expressions in ILC.
data Expr = EVar Name

          | notElem
          | elem
          | notEqual
          | equal
          | greaterThan
          | lessThan
          | greaterThanEqual
          | lessThanEqual
          | add
          | sub
          | mul
          | div
          | mod
          | and
          | or
          | ifThenElse
          | letIn
          | letRec
          | letTuple
          | letArray
          | letRecord
          | letFunction
          | letFunctionRec
          | letFunctionTuple
          | letFunctionArray
          | letFunctionRecord
          | letFunctionFunction
          | letFunctionFunctionRec


          | letFunctionFunctionTuple
          | letFunctionFunctionArray
          | ETuple [Expr]                        -- ^ Tuple
          | EList [Expr]                         -- ^ List
          | ELam Pattern Expr                    -- ^ Unrestricted abstraction
          | ELamw Pattern Expr                   -- ^ Write abstraction
          | ELam1 Pattern Expr                   -- ^ Affine abstraction
          | EApp Expr Expr                       -- ^ Function application
          | EFix Name Expr                       -- ^ Fixpoint
          | ELet Pattern Expr Expr               -- ^ Let binding
          | ELetRd Pattern Expr Expr             -- ^ Read operation
          | EBang Expr                           -- ^ Bang
          | EIf Expr Expr Expr                   -- ^ Conditional
          | EMatch Expr [(Pattern, Expr, Expr)]  -- ^ Pattern match
          | ENu (Name, Name) Expr                -- ^ Channel allocation
          | ERd Expr                             -- ^ Read from channel
          | EWr Expr Expr                        -- ^ Write operation
          | EFork Expr Expr                      -- ^ Fork operation
          | EChoice Expr Expr                    -- ^ External choice
          | EPrint Expr                          -- ^ Print
          | EError Expr                          -- ^ Throw error
          | EBin Binop Expr Expr                 -- ^ Binary expression
          | EUn Unop Expr                        -- ^ Unary expression
          | ECustom Name [Expr]
          | StoreMessage
                 -- ^ Custom data type
          deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False


-- | Binary operators.
-- data Binop = Add | Sub | Mul | Div | Mod | And | Or
-- deriving (Eq, Show)
-- data Binop = Add | Sub | Mul | Div | Mod | And | Or
-- deriving (Eq, Show)
-- data Binop = Add | Sub | Mul | Div | Mod | And | Or
-- deriving (Eq, Show) instance_eq_list
--
-- instance_eq_list :: Eq a => [a] -> [a] -> Bool
--
--

--
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
-- instance_eq_list :: Eq a => [a] -> [a] -> Bool


-- | Unary operators.
-- data Unop = Not | Neg
-- deriving (Eq, Show)
-- data
-- Unop = Not | Neg
-- deriving (Eq, Show) instance_eq_list
-- instance_eq_list :: Eq a => [a] -> [a] -> Bool
-- instance_eq_list :: Eq a => [a] -> [a] -> Bool
data Unop = Not | Neg
deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False






type Pattern = String
type Binop = String
type Unop = String
type Name = String


( pattern_eq_list :: Pattern -> Pattern -> Bool)
pattern_eq_list [] [] = True
pattern_eq_list (x:xs) (y:ys) = x == y && pattern_eq_list xs ys
pattern_eq_list _ _ = False

( binop_eq_list :: Binop -> Binop -> Bool)

-- | Literals in ILC.
data Lit = LInt Integer    -- ^ Integer literal
  | LChar Char -- ^ Character literal
  | LString String -- ^ String literal
  | LFloat Double -- ^ Floating point literal
  | LBool Bool -- ^ Boolean literal
  | LUnit -- ^ Unit literal
  | LArray [Lit] -- ^ Array literal
  | LRecord [(Name, Lit)] -- ^ Record literal
  | LError String -- ^ Error literal
  deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False


-- | A type for a program.
data Program = Program {
  imports :: [Import]
, decls :: [Decl]
 } deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
-- | A type for a declaration.
data Decl = Decl {
  name :: Name
, type_ :: Type
, expr :: Expr
} deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True


}         | LBool Bool      -- ^ Boolean literal
         | LString String  -- ^ String literal
         | LUnit           -- ^ Unit literal
          | LArray [Lit]    -- ^ Array literal
         deriving (Eq, Show)

-- | Built-in binary operators in ILC.
data Binop = Add     -- ^ Addition
           | Sub     -- ^ Subtraction
           | Mul     -- ^ Multiplication
           | Div     -- ^ Integer division
           | Mod     -- ^ Remainder
           | And     -- ^ Logical and
           | Or      -- ^ Logical or
           | Lt      -- ^ Less than
           | Gt      -- ^ Greater than
           | Leq     -- ^ Less than or equal to
           | Geq     -- ^ Greater than or equal to
           | Eql     -- ^ Equal to
           | Neq     -- ^ Not equal to
           | Cons    -- ^ Cons element to list
           | Concat  -- ^ List concatenation
           deriving (Eq, Show)

-- | Built-in unary operator in ILC.
data Unop = Not  -- ^ Logical not
  deriving (Eq, Show)

-- | Built-in types in ILC.
-- data Type = TInt     -- ^ Integer type
--          | TChar    -- ^ Character type
--         | TString  -- ^ String type
--         | TFloat   -- ^ Floating point type
data Type = TInt     -- ^ Integer type
          | TChar    -- ^ Character type
          | TString  -- ^ String type
          | TFloat   -- ^ Floating point type
          | TBool    -- ^ Boolean type
          | TUnit    -- ^ Unit type
          | TArray Type -- ^ Array type
          | TRecord [(Name, Type)] -- ^ Record type
          | TError String -- ^ Error type
          deriving (Eq, Show) instance_eq_list

instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False {
  unopType = UnopNot,
  binopType = BinopAdd,
  litType = LitInt,
  typeType = TypeInt,
}

-- | Patterns in ILC.
data Pattern = PVar Name              -- ^ Variable pattern
             | PInt Integer           -- ^ Integer literal pattern
             | PBool Bool             -- ^ Boolean literal pattern
             | PString String         -- ^ String literal pattern
             | PUnit                  -- ^ Unit literal pattern
             | PWildcard              -- ^ Wildcard pattern
             | PTuple [Pattern]       -- ^ Tuple pattern
             | PList [Pattern]        -- ^ List pattern
             | PCons Pattern Pattern  -- ^ Cons pattern
             | PCust Name [Pattern]   -- ^ Custom data type pattern
             | PGnab Pattern          -- ^ Gnab
             deriving (Eq, Show)



             Show (Expr)
              deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True


-- | Expressions in ILC.
instance_eq_list (x:xs) (y:ys) = x == y &&
instance_eq_list _ _ = False



 ++
 instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
  instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Bool
instance_eq_list [] [] = True


instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False

isPrimeCauset :: Integer -> Boolean
isPrimeCauset n = isPrimeCauset' (n - 1) n
isPrimeCauset' :: Integer -> Integer -> Boolean
isPrimeCauset' 1 1 = True


isPrimeCauset' n 1 = True
isPrimeCauset' n 1 = False

isPrimeCauset' n m = isPrimeCauset' (n - 1) m
isPrimeCauset' n m = isPrimeCauset' (n - 1) map (\x -> x + 1) esn = isPrimeCauset' (n - 1) map (\x -> x + 1) esn



M
instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True instance_eq_list :: Eq a => [a] -> [a] -> Boolean
--For every node that is a leaf, we check if the value is the same.
--For Merkle Tree
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False


instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True

instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Boolean


instance_eq_list :: Eq a => [a] -> [a] -> Boolean

instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys


  ++
  instance Show (Expr) where
    show (PVar n) = show n
    show (PLit l) = show l
    show (PUnop u e) = show u ++ "(" ++ show e ++ ")"
    show (PBinop b e1 e2) = show e1 ++ " " ++ show b ++ " " ++ show e2
    show (PTuple es) = "(" ++ intercalate ", " (map show es) ++ ")"
    show (PList es) = "[" ++ intercalate ", " (map show es) ++ "]"



     () => deriveJSONOptions'' opts =
  case opts of
    {-# OPTIONS #-}
    {-# OPTIONS_GHC -fno-warn-unused-imports #-}



  deriving (Eq, Show) instance_eq_list
instance_eq_list :: Eq a => [a] -> [a] -> Bool{
instance_eq_list [] [] = True{
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys,()=> [a] ->
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys instance_eq_list :: Eq a => [a] -> [a] -> Boolean instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Boolean
}
}
instance_eq_list [] [] = True


 type instance_eq_list :: Eq a => [a] -> [a] -> Boolean
instance_eq_list [] [] = True
instance_eq_list (x:xs) (y:ys) = instance_eq_list xs ys instance_eq_list :: Eq a =>
instance_eq_list (x:xs) (y:ys) = x == y && instance_eq_list xs ys
instance_eq_list _ _ = False
instance_eq_list :: Eq a => [a] -> [a] -> Bool
