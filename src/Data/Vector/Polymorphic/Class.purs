module Data.Vector.Polymorphic.Class where

import Prelude

import Data.Vector.Polymorphic.Types (Rect(..), Vector2)


-- | Class describing types which represent a position on a 2D plane and can be
-- | turned into a `Vector2 a`.
class ToPos a pa | pa → a where
  toPos ∷ pa → Vector2 a

instance toPosVector2 ∷ ToPos a (Vector2 a) where
  toPos = identity

instance toPosRect ∷ ToPos a (Rect a) where
  toPos (Rect pos _) = pos


-- | Class describing types which represent a position on a 2D plane and can be
-- | constructed from a `Vector2 a`.
class FromPos a pa | pa → a where
  fromPos ∷ Vector2 a → pa

instance fromPosVector2 ∷ FromPos a (Vector2 a) where
  fromPos = identity

instance fromPosRect ∷ Semiring a ⇒ FromPos a (Rect a) where
  fromPos pos = Rect pos zero


-- | Class describing types which represent a position on a 2D plane and can be
-- | modified by any function of type `Vector2 a → Vector2 a`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asPosEndo identity = identity`
class AsPosEndo a pa | pa → a where
  asPosEndo ∷ (Vector2 a → Vector2 a) → pa → pa

instance asPosEndoVector2 ∷ AsPosEndo a (Vector2 a) where
  asPosEndo = identity

instance asPosEndoRect ∷ AsPosEndo a (Rect a) where
  asPosEndo f (Rect pos size) = Rect (f pos) size


-- | Class describing types which represent a position on a 2D plane and can be
-- | modified by any function of type `Vector2 a → Vector2 b`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asPos identity = identity`
class AsPos a b pa pb | pa → a, pb → b where
  asPos ∷ (Vector2 a → Vector2 b) → pa → pb

instance asPosVector2 ∷ AsPos a b (Vector2 a) (Vector2 b) where
  asPos = identity


-- | Class describing types which represent a size on a 2D plane and can be
-- | turned into a `Vector2 a`.
class ToSize a sa | sa → a where
  toSize ∷ sa → Vector2 a

instance toSizeVector2 ∷ ToSize a (Vector2 a) where
  toSize = identity

instance toSizeRect ∷ ToSize a (Rect a) where
  toSize (Rect _ size) = size


-- | Class describing types which represent a size on a 2D plane and can be
-- | constructed from a `Vector2 a`.
class FromSize a sa | sa → a where
  fromSize ∷ Vector2 a → sa

instance fromSizeVector2 ∷ FromSize a (Vector2 a) where
  fromSize = identity

instance fromSizeRect ∷ Semiring a ⇒ FromSize a (Rect a) where
  fromSize size = Rect zero size


-- | Class describing types which represent a size on a 2D plane and can be
-- | modified by any function of type `Vector2 a → Vector2 a`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asSizeEndo identity = identity`
class AsSizeEndo a sa | sa → a where
  asSizeEndo ∷ (Vector2 a → Vector2 a) → sa → sa

instance asSizeEndoVector2 ∷ AsSizeEndo a (Vector2 a) where
  asSizeEndo = identity

instance asSizeEndoRect ∷ AsSizeEndo a (Rect a) where
  asSizeEndo f (Rect pos size) = Rect pos (f size)


-- | Class describing types which represent a size on a 2D plane and can be
-- | modified by any function of type `Vector2 a → Vector2 b`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asSize identity = identity`
class AsSize a b sa sb | sa → a, sb → b where
  asSize ∷ (Vector2 a → Vector2 b) → sa → sb

instance asSizeVector2 ∷ AsSize a b (Vector2 a) (Vector2 b) where
  asSize = identity


-- | Class describing types which represent a rectangular region on a 2D plane
-- | and can be turned into a `Rect a`.
class ToRegion a ra | ra → a where
  toRegion ∷ ra → Rect a

instance toRegionVector2 ∷ Semiring a ⇒ ToRegion a (Vector2 a) where
  toRegion size = Rect zero size

instance toRegionRect ∷ ToRegion a (Rect a) where
  toRegion = identity


-- | Class describing types which represent a rectangular region on a 2D plane
-- | and can be constructed from a `Rect a`.
class FromRegion a ra | ra → a where
  fromRegion ∷ Rect a → ra

instance fromRegionVector2 ∷ FromRegion a (Vector2 a) where
  fromRegion (Rect _ size) = size

instance fromRegionRect ∷ FromRegion a (Rect a) where
  fromRegion = identity


-- | Class describing types which represent a rectangular region on a 2D plane
-- | and can be modified by any function of type `Rect a → Rect a`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asRegionEndo identity = identity`
class AsRegionEndo a ra | ra → a where
  asRegionEndo ∷ (Rect a → Rect a) → ra → ra

instance asRegionEndoVector2 ∷ Semiring a ⇒ AsRegionEndo a (Vector2 a) where
  asRegionEndo f = fromRegion <<< f <<< toRegion

instance asRegionEndoRect ∷ AsRegionEndo a (Rect a) where
  asRegionEndo = identity


-- | Class describing types which represent a rectangular region on a 2D plane
-- | and can be modified by any function of type `Rect a → Rect b`.
-- |
-- | Instances must satisfy the following law:
-- |
-- | - Identity: `asRegion identity = identity`
class AsRegion a b ra rb | ra → a, rb → b where
  asRegion ∷ (Rect a → Rect b) → ra → rb

instance asRegionVector2 ∷ Semiring a ⇒ AsRegion a b (Vector2 a) (Vector2 b)
  where
    asRegion f = fromRegion <<< f <<< toRegion

instance asRegionRect ∷ AsRegion a b (Rect a) (Rect b) where
  asRegion = identity
