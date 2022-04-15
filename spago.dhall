{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickcheck-state-machine"
, license = "MPL"
, repository =
    "https://github.com/the-dr-lazy/purescript-quickcheck-state-machine"
, dependencies =
  [ "const"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lcg"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "spec"
  , "typeable"
  , "gen"
  , "tailrec"
  , "identity"
  , "ordered-collections"
  , "aff"
  , "arrays"
  , "newtype"
  , "nonempty"
  , "debugged"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
