{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4/packages.dhall
        sha256:a6d66723b6109f1e3eaf6575910f1c51aa545965ce313024ba329360e2f009ac

in  upstream
  with presto =
    { dependencies =
      [ "aff"
      , "avar"
      , "datetime"
      , "effect"
      , "either"
      , "exceptions"
      , "exists"
      , "foldable-traversable"
      , "foreign"
      , "foreign-generic"
      , "foreign-object"
      , "free"
      , "identity"
      , "maybe"
      , "newtype"
      , "parallel"
      , "prelude"
      , "record"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/belevy/purescript-presto.git"
    , version = "p-0.15-master"
    }
  with backtrack =
    { dependencies =
      [ "control", "effect", "prelude", "tailrec", "transformers" ]
    , repo = "https://github.com/belevy/purescript-backtrack.git"
    , version = "ps-0.15"
    }
  with presto-dom =
    { dependencies =
      [ "effect"
      , "prelude"
      , "hyrule"
      , "halogen-vdom"
      , "tracker"
      , "presto"
      , "aff"
      , "arrays"
      , "control"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-generic"
      , "foreign-object"
      , "functions"
      , "integers"
      , "lists"
      , "maybe"
      , "newtype"
      , "refs"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      ]
    , repo = "https://github.com/belevy/purescript-presto-dom.git"
    , version = "nammaYatri-p-0.15"
    }
  with halogen-vdom =
    { dependencies =
      [ "effect"
      , "prelude"
      , "foreign-object"
      , "arrays"
      , "bifunctors"
      , "foreign"
      , "functions"
      , "maybe"
      , "newtype"
      , "nullable"
      , "refs"
      , "tuples"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      ]
    , repo = "https://github.com/belevy/purescript-halogen-vdom.git"
    , version = "p-0.15.6-master"
    }
  with foreign-generic =
    { dependencies =
      [ "assert"
      , "console"
      , "effect"
      , "exceptions"
      , "foreign"
      , "foreign-object"
      , "identity"
      , "prelude"
      , "record"
      , "arrays"
      , "bifunctors"
      , "control"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "newtype"
      , "partial"
      , "strings"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/juspay/purescript-foreign-generic.git"
    , version = "main"
    }
  with tracker =
    { dependencies =
      [ "effect"
      , "prelude"
      , "presto"
      , "arrays"
      , "debug"
      , "foldable-traversable"
      , "foreign"
      , "foreign-generic"
      , "foreign-object"
      , "maybe"
      , "strings"
      ]
    , repo = "https://github.com/belevy/purescript-tracker.git"
    , version = "91653fb800192b9ff9f9026b2841061339ced7d2"
    }
  with otp-reader =
    { dependencies =
      [ "effect"
      , "prelude"
      , "presto"
      , "foreign-generic"
      , "tracker"
      , "aff"
      , "arrays"
      , "avar"
      , "datetime"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "maybe"
      , "newtype"
      , "numbers"
      , "parallel"
      , "refs"
      , "strings"
      , "transformers"
      ]
    , repo = "https://github.com/belevy/purescript-otp-reader.git"
    , version = "p-0.15-master"
    }
  with beckn-common = ./ui-common/spago.dhall as Location
