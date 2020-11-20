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
, repository = "https://github.com/artemisSystem/purescript-polymorphic-vectors.git"
}
