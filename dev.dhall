let conf = ./spago.dhall

in conf // {
  dependencies = conf.dependencies # [ "psci-support", "console", "effect" ]
}