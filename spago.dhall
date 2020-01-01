{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "polymorphic-vectors"
, dependencies =
    [ "canvas"
    , "console"
    , "distributive"
    , "effect"
    , "foldable-traversable"
    , "math"
    , "prelude"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/3ddyy/purescript-polymorphic-vectors.git"
}
