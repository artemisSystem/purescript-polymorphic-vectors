let conf = ./spago.dhall

in conf //
  { dependencies = conf.dependencies # [ "console", "effect", "psci-support" ]
  }