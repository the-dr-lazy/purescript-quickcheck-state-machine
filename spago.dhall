{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickcheck-state-machine"
, license = "MPL"
, repository =
    "https://github.com/the-dr-lazy/purescript-quickcheck-state-machine"
, dependencies = [ "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
