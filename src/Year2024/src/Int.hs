{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ViewPatterns #-}

module Int where

import GHC.Real
import Data.List
import Data.Maybe
import Control.Applicative

import Types
import Utilities
import Examples

import Data.Bifunctor

--
-- Universal assumptions/preconditions:
-- 1. All polynomials are in standard form with decreasing
--    powers of x
-- 2. 0 is represented by P [(0, 0)]; P [] is undefined for
--    the purposes of the exercise.
-- 3. All constants will be polynomials of the form
--    [(c, 0)], e.g. logarithms of constants and constant
--    powers will not appear.
-- 4. All computed integrals omit the constant of integration.
--

-------------------------------------------------
-- Part I (13 marks)

addP :: Polynomial -> Polynomial -> Polynomial
addP p [] = p
addP [] p = p
addP (t1@(c1, x1):p1) (t2@(c2, x2):p2)
    | x1 == x2  = (c1 + c2, x1):addP p1 p2
    | x1 > x2   = t1:addP p1 (t2:p2)
    | otherwise = t2:addP (t1:p1) p2

mulP :: Polynomial -> Polynomial -> Polynomial
mulP [] p = []
mulP p [] = []
mulP (t1:p1) p2 = addP (mulT t1 <$> p2) (mulP p1 p2)

mulT :: Term -> Term -> Term
mulT (c1, x1) (c2, x2) = (c1*c2, x1 + x2)


sumP :: [Polynomial] -> Polynomial
sumP = foldl' addP []

prodP :: [Polynomial] -> Polynomial
prodP = foldl' mulP [(1, 0)]

diffT :: Term -> Term
diffT (_, 0) = (0, 0)
diffT (c, x) = (c * toRational x, x - 1)

-- > The speç should specify the constant term to be zero!
intT :: Term -> Term
intT (c, x) = (c / toRational (x+1), x + 1)

diffP :: Polynomial -> Polynomial
diffP = map diffT

intP :: Polynomial -> Polynomial
intP = map intT

-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr
diffE (P p)       = P (diffP p)
diffE (Add e1 e2) = Add (diffE e1) (diffE e2)
diffE (Mul e1 e2) = Add (Mul e1 (diffE e2)) (Mul (diffE e1) e2)
diffE (Pow e n)   = Mul (Mul (toExpr n) (Pow e (n - 1))) (diffE e)
diffE (Log e)     = Mul (Pow e -1) (diffE e)


--
-- Given
--
toExpr :: Rational -> Expr
toExpr n = P [(n, 0)]

isConstant :: Expr -> Bool
isConstant (P [(_, 0)]) = True
isConstant _ = False

simplifiedDiff :: Expr -> Expr
simplifiedDiff = simplify . diffE

printDiff :: Expr -> IO ()
printDiff = prettyPrint . simplifiedDiff

-------------------------------------------------
-- Part III (10 marks)

intE :: Expr -> Maybe Expr
intE (P p)          = Just $ P (intP p)
intE (Add e1 e2)    = liftA2 Add (intE e1) (intE e2)
intE (Mul e1 e2)    = applyICR e1 e2 <|> applyICR e2 e1
intE e              = applyICR (toExpr 1) e


applyICR :: Expr -> Expr -> Maybe Expr
applyICR e1 (Pow e2 n)
    | n == -1                 = Just (Log e2)
    | e1 == simplifiedDiff e2 = Just $ Mul (toExpr (1/(n+1))) (Pow e2 (n+1))
applyICR e1 (Log e2)
    | e1 == simplifiedDiff e2 = Just $ Mul e2 (Add (Log e2) (toExpr -1))
applyICR e1 e2
    | isConstant e1           = liftA2 Mul (Just e1) (intE e2)
    | e1 == simplifiedDiff e2 = Just $ Mul (toExpr (1/2)) (Pow e2 2)
    | e2 <- Mul e3 e4         = applyICR e3 (Mul e1 e4) <|> applyICR e4 (Mul e1 e3)



--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)
