
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

import Data.Monoid(Monoid(..))
import Data.Foldable

------------------------------------------------------------------------------
-- 2D vector, constructors and selectors.
------------------------------------------------------------------------------

-- Restrict coefficient space to RealFloat (rather than Floating) for now.
-- While unclear if a complex coefficient space would be useful (and if the
-- result really would be a 2d vector), the only thing causing trouble is the
-- use of atan2 in vector2Theta. Maybe atan2 can be generalized?

data RealFloat a => Vector2 a = Vector2 !a !a deriving (Eq,Show,Ord)

vector2 :: RealFloat a => a -> a -> Vector2 a
vector2 = Vector2 

vector2X :: RealFloat a => Vector2 a -> a
vector2X (Vector2 x _) = x

vector2Y :: RealFloat a => Vector2 a -> a
vector2Y (Vector2 _ y) = y

vector2XY :: RealFloat a => Vector2 a -> (a, a)
vector2XY (Vector2 x y) = (x, y)

vector2Polar :: RealFloat a => a -> a -> Vector2 a
vector2Polar rho theta = Vector2 (rho * cos theta) (rho * sin theta) 

vector2Rho :: RealFloat a => Vector2 a -> a
vector2Rho (Vector2 x y) = sqrt (x * x + y * y)

vector2Theta :: RealFloat a => Vector2 a -> a
vector2Theta (Vector2 x y) = atan2 y x

vector2RhoTheta :: RealFloat a => Vector2 a -> (a, a)
vector2RhoTheta v = (vector2Rho v, vector2Theta v)

------------------------------------------------------------------------------
-- Vector space instance
------------------------------------------------------------------------------

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

zeroVector :: (RealFloat a) => Vector2 a
zeroVector = Vector2 0 0

(*^) :: (RealFloat a) => a -> Vector2 a -> Vector2 a
a *^ (Vector2 x y) = Vector2 (a * x) (a * y)

(^/) :: (RealFloat a) => Vector2 a -> a -> Vector2 a
(Vector2 x y) ^/ a = Vector2 (x / a) (y / a)

negateVector :: (RealFloat a) => Vector2 a -> Vector2 a
negateVector (Vector2 x y) = Vector2 (-x) (-y)

(^+^) :: (RealFloat a) => Vector2 a -> Vector2 a -> Vector2 a
(Vector2 x1 y1) ^+^ (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

(^-^) :: (RealFloat a) => Vector2 a -> Vector2 a -> Vector2 a
(Vector2 x1 y1) ^-^ (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

dot :: (RealFloat a) => Vector2 a -> Vector2 a -> a
(Vector2 x1 y1) `dot` (Vector2 x2 y2) = x1 * x2 + y1 * y2


------------------------------------------------------------------------------
-- Additional operations
------------------------------------------------------------------------------

vector2Rotate :: RealFloat a => a -> Vector2 a -> Vector2 a
vector2Rotate theta' v = vector2Polar (vector2Rho v) (vector2Theta v + theta')

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

vsum :: RealFloat a => [Vector2 a] -> Vector2 a
vsum = foldl' (^+^) zeroVector
