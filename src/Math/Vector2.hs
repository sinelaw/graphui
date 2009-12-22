
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Vector2
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003, (c) Noam Lewis, 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 2D vector abstraction (R^2).
--
-- ToDo: Deriving Show, or provide dedicated show instance?
-----------------------------------------------------------------------------------------

module Math.Vector2 where

import Prelude hiding (negate)

import Data.Monoid(Monoid(..))
import Data.Foldable
import Control.Applicative(Applicative(..), liftA2)

------------------------------------------------------------------------------
-- 2D vector, constructors and selectors.
------------------------------------------------------------------------------

-- Restrict coefficient space to RealFloat (rather than Floating) for now.
-- While unclear if a complex coefficient space would be useful (and if the
-- result really would be a 2d vector), the only thing causing trouble is the
-- use of atan2 in vector2Theta. Maybe atan2 can be generalized?

data Vector2 a = Vector2 !a !a deriving (Eq,Show,Ord)

instance Functor Vector2 where
    fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Applicative Vector2 where
    pure x = Vector2 x x
    Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)

vector2 :: RealFloat a => a -> a -> Vector2 a
vector2 = Vector2 

getX :: RealFloat a => Vector2 a -> a
getX (Vector2 x _) = x

getY :: RealFloat a => Vector2 a -> a
getY (Vector2 _ y) = y

getXY :: RealFloat a => Vector2 a -> (a, a)
getXY (Vector2 x y) = (x, y)

fromPolar :: RealFloat a => a -> a -> Vector2 a
fromPolar rho theta = Vector2 (rho * cos theta) (rho * sin theta) 

getRho :: RealFloat a => Vector2 a -> a
getRho (Vector2 x y) = sqrt (x * x + y * y)

getManhatten :: RealFloat a => Vector2 a -> a
getManhatten (Vector2 x y) = x + y

getTheta :: RealFloat a => Vector2 a -> a
getTheta (Vector2 x y) = atan2 y x

getRhoTheta :: RealFloat a => Vector2 a -> (a, a)
getRhoTheta v = (getRho v, getTheta v)

------------------------------------------------------------------------------
-- Semantic Editor Combinators, see: http://conal.net/blog/posts/semantic-editor-combinators/
------------------------------------------------------------------------------

type Endo a = a -> a

atX :: Endo a -> Endo (Vector2 a)
atX f (Vector2 x y) = Vector2 (f x) y

atY :: Endo a -> Endo (Vector2 a)
atY f (Vector2 x y) = Vector2 x (f y)

atXY :: Endo (a, a) -> Endo (Vector2 a)
atXY f (Vector2 x y) = uncurry Vector2 (f (x, y))

------------------------------------------------------------------------------
-- Vector space instance
------------------------------------------------------------------------------

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

zeroVector :: (RealFloat a) => Vector2 a
zeroVector = pure 0

(*^) :: (RealFloat a) => a -> Vector2 a -> Vector2 a
a *^ v = fmap (a*) v

(^/) :: (RealFloat a) => Vector2 a -> a -> Vector2 a
v ^/ a = fmap (/a) v

negate :: (RealFloat a) => Vector2 a -> Vector2 a
negate = fmap (0-)

(^+^) :: (RealFloat a) => Vector2 a -> Vector2 a -> Vector2 a
(^+^) = liftA2 (+)

(^-^) :: (RealFloat a) => Vector2 a -> Vector2 a -> Vector2 a
(^-^) = liftA2 (-)

dot :: (RealFloat a) => Vector2 a -> Vector2 a -> a
v1 `dot` v2 = getManhatten $ liftA2 (*) v1 v2

vsum :: RealFloat a => [Vector2 a] -> Vector2 a
vsum = foldl' (^+^) zeroVector

------------------------------------------------------------------------------
-- Additional operations
------------------------------------------------------------------------------

rotate :: RealFloat a => a -> Vector2 a -> Vector2 a
rotate theta' v = fromPolar (getRho v) (getTheta v + theta')

------------------------------------------------------------------------------
-- Monoids
------------------------------------------------------------------------------

newtype VSum a = VSum { unVSum :: Vector2 a }
    deriving (Eq,Show,Ord)

inVSum :: (Vector2 a -> Vector2 b) -> VSum a -> VSum b
inVSum f = VSum . f . unVSum

inVSum2 :: (Vector2 a -> Vector2 b -> Vector2 c)
        -> VSum a -> VSum b -> VSum c
inVSum2 f = inVSum . f . unVSum

instance RealFloat a => Monoid (VSum a) where
    mempty = VSum zeroVector
    mappend = inVSum2 (^+^)
