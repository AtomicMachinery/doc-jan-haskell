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


mulP :: Polynomial -> Polynomial -> Polynomial


sumP :: [Polynomial] -> Polynomial


prodP :: [Polynomial] -> Polynomial


diffT :: Term -> Term


-- > The speç should specify the constant term to be zero!
intT :: Term -> Term


diffP :: Polynomial -> Polynomial


intP :: Polynomial -> Polynomial


-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr




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


applyICR :: Expr -> Expr -> Maybe Expr




--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)
