module Data.Vector.Polymorphic
  ( makeRect
  , length
  , diagonal
  , inside
  , outside
  , putInsideMod
  , area
  , perimeter
  , ratio
  , midPos
  , dot
  , perpendicular
  , parallel
  , toRectangleWith
  , toRectangle
  
  , module Types
  ) where

import Prelude

import Control.Apply (lift3)
import Graphics.Canvas (Rectangle)
import Data.Vector.Polymorphic.Types (Rect(..), (><))
import Data.Vector.Polymorphic.Types (Rect(..), Vector2(..), (><)) as Types
import Data.Vector.Polymorphic.Class (class AsPosEndo, class FromPos, class ToPos, class ToRegion, class ToSize, asPosEndo, fromPos, toPos, toRegion, toSize)
import Math (sqrt)


-- | Constructs a `Rect` from four values
makeRect :: forall a. a -> a -> a -> a -> Rect a
makeRect x y w h = Rect (x >< y) (w >< h)

-- | Get the length of a position vector
length :: forall p. ToPos Number p => p -> Number
length = toPos >>> \(x >< y) -> sqrt (x * x + y * y)

-- | Get the diagonal of a size
diagonal :: forall s. ToSize Number s => s -> Number
diagonal = toSize >>> \(w >< h) -> sqrt (w * w + h * h)

-- | Checks if a position is inside a region. Size of the region should be
-- | positive. Inclusive on the lower bound, exclusive on the higher bound.
inside
  :: forall r p n
   . ToRegion n r => ToPos n p => Ord n => Semiring n
  => r -> p -> Boolean
inside region pos = case toRegion region, toPos pos of
  Rect (rx >< ry) (rw >< rh), (x >< y)
    | x < rx       -> false
    | x >= rx + rw -> false
    | y < ry       -> false
    | y >= ry + rh -> false
    | otherwise    -> true

-- | `inside`, but with its result negated.
outside
  :: forall r p n
  . ToRegion n r => ToPos n p => Ord n => Semiring n
  => r -> p -> Boolean
outside = not <$> inside

-- | Put a position inside a region by using the modulus operator
putInsideMod
  :: forall r p n
   . ToRegion n r => AsPosEndo n p => EuclideanRing n
  => r -> p -> p
putInsideMod = toRegion >>> \(Rect rectpos rectsize) ->
  asPosEndo $ lift3 (\rpos rsize pos -> ((pos - rpos) `mod` rsize) + rpos)
    rectpos
    rectsize

-- | Get the area of a size
area :: forall s n. ToSize n s => Semiring n => s -> n
area = toSize >>> \(w >< h) -> w * h

-- | Get the perimeter of a size
perimeter :: forall s n. ToSize n s => Semiring n => s -> n
perimeter = toSize >>> \(w >< h) -> (w + w + h + h)

-- | Get the ratio of a size by dividing the width by the height
ratio :: forall s n. ToSize n s => EuclideanRing n => s -> n
ratio = toSize >>> \(w >< h) -> w / h

-- | Get the center position of a region
midPos :: forall s p n. ToRegion n s => FromPos n p => EuclideanRing n => s -> p
midPos region = fromPos $ (\p s -> p + half s) <$> pos <*> size
  where
    (Rect pos size) = toRegion region
    half n = n / (one + one)

-- | Get the dot product of two vectors
dot :: forall p n. ToPos n p => Semiring n => p -> p -> n
dot p1 p2 = x1 * x2 + y1 * y2
  where
    (x1 >< y1) = toPos p1
    (x2 >< y2) = toPos p2

-- | Check if two vectors are perpendicular
perpendicular
  :: forall p n. ToPos n p => Semiring n => Eq n => p -> p -> Boolean
perpendicular p1 p2 = dot p1 p2 == zero

-- | Check if two vectors are parallel
parallel
  :: forall p n. ToPos n p => EuclideanRing n => Eq n => p -> p -> Boolean
parallel p1 p2 = case toPos p1, toPos p2 of
  (x1 >< y1), (x2 >< y2)
    | y1 == zero || y2 == zero -> y1 == y2
    | otherwise                -> x1 / y1 == x2 / y2

-- | Turn a region into a `Rectangle`
toRectangleWith
  :: forall n r
   . ToRegion n r => Semiring n
   => (n -> Number) -> r -> Rectangle
toRectangleWith f = toRegion >>> map f >>>
  \(Rect (x >< y) (width >< height)) -> { x, y, width, height }

-- | Turn a rection represented with `Number`s into a `Rectangle`
toRectangle :: forall r. ToRegion Number r => r -> Rectangle
toRectangle = toRectangleWith identity