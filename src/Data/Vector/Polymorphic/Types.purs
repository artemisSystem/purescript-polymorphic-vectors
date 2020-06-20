module Data.Vector.Polymorphic.Types where

import Prelude

import Control.Apply (lift2)
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Semigroup.Traversable (class Traversable1, traverse1Default, traverse1, sequence1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1)

data Vector2 a = Vector2 a a

infix 2 Vector2 as ><

-- | Get the x value from a `Vector2`
getX ∷  ∀ a. Vector2 a → a
getX (x >< _) = x

-- | Get the y value from a `Vector2`
getY ∷  ∀ a. Vector2 a → a
getY (_ >< y) = y

derive instance eqVector2 ∷ Eq a ⇒ Eq (Vector2 a)
derive instance ordVector2 ∷ Ord a ⇒ Ord (Vector2 a)

instance showVector2 ∷ (Show a) ⇒ Show (Vector2 a) where
  show (a >< b) = "(" <> show a <> " >< " <> show b <> ")"

instance functorVector2 ∷ Functor Vector2 where
  map f (a >< b) = f a >< f b

instance applyVector2 ∷ Apply Vector2 where
  apply (f >< g) (x >< y) = f x >< g y

instance applicativeVector2 ∷ Applicative Vector2 where
  pure x = x >< x

instance bindVector2 ∷ Bind Vector2 where
  bind vec f = distribute f <*> vec

instance monadVector2 ∷ Monad Vector2

instance semigroupVector2 ∷ Semigroup a ⇒ Semigroup (Vector2 a) where
  append (a >< b) (a' >< b') = (a <> a') >< (b <> b')

instance monoidVector2 ∷ Monoid a ⇒ Monoid (Vector2 a) where
  mempty = mempty >< mempty

instance semiringVector2 ∷ Semiring a ⇒ Semiring (Vector2 a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance foldable1Vector2 ∷ Foldable1 Vector2 where
  fold1 (x >< y) = x <> y
  foldMap1 f (x >< y) = f x <> f y

instance foldableVector2 ∷ Foldable Vector2 where
  foldr f z (x >< y) = x `f` (y `f` z)
  foldl f z (x >< y) = (z `f` x) `f` y
  foldMap = foldMap1

instance traversable1Vector2 ∷ Traversable1 Vector2 where
  sequence1 (fx >< fy) = (><) <$> fx <*> fy
  traverse1 = traverse1Default

instance traversableVector2 ∷ Traversable Vector2 where
  sequence = sequence1
  traverse = traverse1

instance distributiveVector2 ∷ Distributive Vector2 where
  distribute fvec = (><)
    do getX <$> fvec
    do getY <$> fvec
  collect = collectDefault


data Rect a = Rect (Vector2 a) (Vector2 a)

-- | Get the topleft position of a `Rect`
getPos ∷  ∀ a. Rect a → Vector2 a
getPos (Rect pos _) = pos

-- | Get the size of a `Rect`
getSize ∷  ∀ a. Rect a → Vector2 a
getSize (Rect _ size) = size

-- | Constructs a `Rect` from four values
makeRect ∷  ∀ a. a → a → a → a → Rect a
makeRect x y w h = Rect (x >< y) (w >< h)

derive instance eqRect ∷ Eq a ⇒ Eq (Rect a)
derive instance ordRect ∷ Ord a ⇒ Ord (Rect a)

instance showRect ∷ Show a ⇒ Show (Rect a) where
  show (Rect pos size) = "(Rect " <> show pos <> " " <> show size <> ")"

instance functorRect ∷ Functor Rect where
  map f (Rect xy wh) = Rect (map f xy) (map f wh)

instance applyRect ∷ Apply Rect where
  apply (Rect ab cd) (Rect xy wh) = Rect (apply ab xy) (apply cd wh)

instance applicativeRect ∷ Applicative Rect where
  pure x = Rect (pure x) (pure x)

instance bindRect ∷ Bind Rect where
  bind rect f = distribute f <*> rect

instance monadRect ∷ Monad Rect

instance semigroupRect ∷ Semigroup a ⇒ Semigroup (Rect a) where
  append (Rect a b) (Rect a' b') = Rect (a <> a') (b <> b')

instance monoidRect ∷ Monoid a ⇒ Monoid (Rect a) where
  mempty = Rect mempty mempty

instance semiringRect ∷ Semiring a ⇒ Semiring (Rect a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance foldable1Rect ∷ Foldable1 Rect where
  fold1 (Rect (x >< y) (w >< h)) = x <> y <> w <> h
  foldMap1 f (Rect (x >< y) (w >< h)) = f x <> f y <> f w <> f h

instance foldableRect ∷ Foldable Rect where
  foldr f z (Rect (x >< y) (w >< h)) = x `f` (y `f` (w `f` (h `f` z)))
  foldl f z (Rect (x >< y) (w >< h)) = (((z `f` x) `f` y) `f` w) `f` h
  foldMap = foldMap1

instance traversable1Rect ∷ Traversable1 Rect where
  sequence1 (Rect (fx >< fy) (fw >< fh)) = makeRect <$> fx <*> fy <*> fw <*> fh
  traverse1 = traverse1Default

instance traversableRect ∷ Traversable Rect where
  sequence = sequence1
  traverse = traverse1

instance distributiveRect ∷ Distributive Rect where
  distribute frect = makeRect
    do getX <<< getPos  <$> frect
    do getY <<< getPos  <$> frect
    do getX <<< getSize <$> frect
    do getY <<< getSize <$> frect
  collect = collectDefault