let conf = ./spago.dhall

in conf //
  { dependencies = conf.dependencies #
    [ "canvas"
    , "console"
    , "effect"
    , "psci-support"
    ]
  }
