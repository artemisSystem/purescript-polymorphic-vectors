{ name = "polymorphic-vectors"
, dependencies =
  [ "distributive"
  , "foldable-traversable"
  , "math"
  , "prelude"
  , "record"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository =
    "https://github.com/artemisSystem/purescript-polymorphic-vectors.git"
}
