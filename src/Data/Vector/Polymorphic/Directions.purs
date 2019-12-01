module Data.Vector.Polymorphic.Directions
  ( Direction(..)
  ) where

import Prelude

import Data.Vector.Polymorphic ((><))
import Data.Vector.Polymorphic.Class (class ToPos)

minusone :: forall a. Ring a => a
minusone = negate one

data Direction a
  = Right
  | Upright
  | Up
  | Upleft
  | Left
  | Downleft
  | Down
  | Downright
  | None

derive instance eqDirection :: Eq (Direction a)

instance showDirection :: Show (Direction a) where
  show Right     = "Right"
  show Upright   = "Upright"
  show Up        = "Up"
  show Upleft    = "Upleft"
  show Left      = "Left"
  show Downleft  = "Downleft"
  show Down      = "Down"
  show Downright = "Downright"
  show None      = "None"

instance toPosDirection :: Ring a => ToPos a (Direction a) where
  toPos Right     =      one >< zero
  toPos Upright   =      one >< minusone
  toPos Up        =     zero >< minusone
  toPos Upleft    = minusone >< minusone
  toPos Left      = minusone >< zero
  toPos Downleft  = minusone >< one
  toPos Down      =     zero >< one
  toPos Downright =      one >< one
  toPos None      =     zero >< zero