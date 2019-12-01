module Data.Vector.Polymorphic.Types where

import Prelude

import Control.Apply (lift2)


data Vector2 a = Vector2 a a

infix 2 Vector2 as ><

derive instance eqVector2 :: Eq a => Eq (Vector2 a)
derive instance ordVector2 :: Ord a => Ord (Vector2 a)

instance showVector2 :: (Show a) => Show (Vector2 a) where
  show (a >< b) = "(" <> show a <> " >< " <> show b <> ")"

instance functorVector2 :: Functor Vector2 where
  map f (a >< b) = (f a >< f b)

instance applyVector2 :: Apply Vector2 where
  apply (f >< g) (x >< y) = (f x >< g y)

instance applicativeVector2 :: Applicative Vector2 where
  pure x = (x >< x)

instance semigroupVector2 :: Semigroup a => Semigroup (Vector2 a) where
  append (a >< b) (a' >< b') = (a <> a') >< (b <> b')

instance monoidVector2 :: Monoid a => Monoid (Vector2 a) where
  mempty = (mempty >< mempty)

instance semiringVector2 :: Semiring a => Semiring (Vector2 a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one


data Rect a = Rect (Vector2 a) (Vector2 a)

derive instance eqRect :: Eq a => Eq (Rect a)
derive instance ordRect :: Ord a => Ord (Rect a)

instance showRect :: Show a => Show (Rect a) where
  show (Rect pos size) = "(Rect " <> show pos <> " " <> show size <> ")"

instance functorRect :: Functor Rect where
  map f (Rect xy wh) = Rect (map f xy) (map f wh)

instance applyRect :: Apply Rect where
  apply (Rect ab cd) (Rect xy wh) = Rect (apply ab xy) (apply cd wh)

instance applicativeRect :: Applicative Rect where
  pure x = Rect (pure x) (pure x)

instance semigroupRect :: Semigroup a => Semigroup (Rect a) where
  append (Rect a b) (Rect a' b') = Rect (a <> a') (b <> b')

instance monoidRect :: Monoid a => Monoid (Rect a) where
  mempty = Rect mempty mempty

instance semiringRect :: Semiring a => Semiring (Rect a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one