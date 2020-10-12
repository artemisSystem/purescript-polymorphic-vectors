{ name = "polymorphic-vectors"
, dependencies =
  [ "canvas"
  , "distributive"
  , "foldable-traversable"
  , "math"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/3ddyy/purescript-polymorphic-vectors.git"
}
