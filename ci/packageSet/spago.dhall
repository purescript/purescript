{ name = "my-project"
, dependencies =
  [ "abides"
  , "ace"
  , "aff"
  , "aff-bus"
  , "aff-coroutines"
  , "aff-promise"
  , "aff-retry"
  , "affjax"
  , "ansi"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "argonaut-traversals"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "arrays-zipper"
  , "assert"
  , "avar"
  , "aws-encryption-sdk"
  , "aws-sdk-basic"
  , "b64"
  , "barlow-lens"
  , "basic-auth"
  , "bifunctors"
  , "bigints"
  , "bip39"
  , "biscotti-cookie"
  , "biscotti-session"
  , "bower-json"
  , "bucketchain"
  , "bucketchain-basic-auth"
  , "bucketchain-conditional"
  , "bucketchain-cors"
  , "bucketchain-csrf"
  , "bucketchain-header-utils"
  , "bucketchain-health"
  , "bucketchain-history-api-fallback"
  , "bucketchain-logger"
  , "bucketchain-secure"
  , "bucketchain-simple-api"
  , "bucketchain-sslify"
  , "bucketchain-static"
  , "bytestrings"
  , "call-by-name"
  , "canvas"
  , "cartesian"
  , "catenable-lists"
  , "channel"
  , "channel-stream"
  , "checked-exceptions"
  , "cheerio"
  , "cirru-parser"
  , "classnames"
  , "clipboardy"
  , "codec"
  , "codec-argonaut"
  , "colors"
  , "concur-core"
  , "concur-react"
  , "concurrent-queues"
  , "console"
  , "const"
  , "contravariant"
  , "control"
  , "coroutines"
  , "crypto"
  , "css"
  , "cssom"
  , "datetime"
  , "debug"
  , "decimals"
  , "dexie"
  , "distributive"
  , "dodo-printer"
  , "dom-filereader"
  , "dom-indexed"
  , "dotenv"
  , "downloadjs"
  , "drawing"
  , "dynamic-buffer"
  , "easy-ffi"
  , "effect"
  , "either"
  , "elasticsearch"
  , "elmish"
  , "elmish-html"
  , "email-validate"
  , "encoding"
  , "enums"
  , "errors"
  , "exceptions"
  , "exists"
  , "exitcodes"
  , "expect-inferred"
  , "express"
  , "ffi-foreign"
  , "filterable"
  , "fixed-points"
  , "fixed-precision"
  , "flame"
  , "float32"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "fork"
  , "form-urlencoded"
  , "format"
  , "formatters"
  , "framer-motion"
  , "free"
  , "freeap"
  , "freet"
  , "functions"
  , "functors"
  , "fuzzy"
  , "gen"
  , "geometry-plane"
  , "github-actions-toolkit"
  , "gl-matrix"
  , "gomtang-basic"
  , "grain"
  , "grain-router"
  , "grain-virtualized"
  , "graphql-client"
  , "graphqlclient"
  , "graphs"
  , "group"
  , "halogen"
  , "halogen-bootstrap4"
  , "halogen-css"
  , "halogen-formless"
  , "halogen-hooks"
  , "halogen-hooks-extra"
  , "halogen-select"
  , "halogen-store"
  , "halogen-storybook"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "halogen-vdom"
  , "heterogeneous"
  , "heterogeneous-extrablatt"
  , "homogeneous"
  , "http-methods"
  , "httpure"
  , "httpure-contrib-biscotti"
  , "httpure-middleware"
  , "identity"
  , "identy"
  , "indexed-monad"
  , "inflection"
  , "integers"
  , "interpolate"
  , "invariant"
  , "js-date"
  , "js-fileio"
  , "js-timers"
  , "js-uri"
  , "justifill"
  , "jwt"
  , "kafkajs"
  , "lazy"
  , "lcg"
  , "leibniz"
  , "lists"
  , "literals"
  , "logging"
  , "longs"
  , "machines"
  , "makkori"
  , "math"
  , "matrices"
  , "matryoshka"
  , "maybe"
  , "media-types"
  , "metadata"
  , "midi"
  , "milkis"
  , "minibench"
  , "mmorph"
  , "monad-control"
  , "monad-logger"
  , "monad-loops"
  , "monad-unlift"
  , "monoidal"
  , "morello"
  , "motsunabe"
  , "mysql"
  , "naturals"
  , "nested-functor"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-he"
  , "node-http"
  , "node-net"
  , "node-path"
  , "node-postgres"
  , "node-process"
  , "node-readline"
  , "node-sqlite3"
  , "node-streams"
  , "node-url"
  , "nodemailer"
  , "nonempty"
  , "now"
  , "nullable"
  , "numbers"
  , "open-folds"
  , "open-memoize"
  , "open-mkdirp-aff"
  , "open-pairing"
  , "option"
  , "options"
  , "options-extra"
  , "optparse"
  , "ordered-collections"
  , "ordered-set"
  , "orders"
  , "pairs"
  , "parallel"
  , "parsing"
  , "parsing-dataview"
  , "parsing-expect"
  , "parsing-hexadecimal"
  , "parsing-repetition"
  , "parsing-replace"
  , "parsing-uuid"
  , "parsing-validation"
  , "partial"
  , "pathy"
  , "payload"
  , "phoenix"
  , "pipes"
  , "point-free"
  , "polymorphic-vectors"
  , "posix-types"
  , "precise"
  , "precise-datetime"
  , "prelude"
  , "prettier"
  , "prettier-printer"
  , "pretty-logs"
  , "profunctor"
  , "profunctor-lenses"
  , "promises"
  , "ps-cst"
  , "psa-utils"
  , "psc-ide"
  , "psci-support"
  , "queue"
  , "quickcheck"
  , "quickcheck-combinators"
  , "quickcheck-laws"
  , "quickcheck-utf8"
  , "quotient"
  , "random"
  , "rationals"
  , "rave"
  , "react"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dnd"
  , "react-basic-dom"
  , "react-basic-emotion"
  , "react-basic-hooks"
  , "react-dom"
  , "react-enzyme"
  , "react-halo"
  , "react-queue"
  , "react-testing-library"
  , "read"
  , "record"
  , "record-extra"
  , "record-extra-srghma"
  , "redux-devtools"
  , "refined"
  , "refs"
  , "remotedata"
  , "resource"
  , "resourcet"
  , "result"
  , "return"
  , "ring-modules"
  , "routing"
  , "routing-duplex"
  , "row-extra"
  , "run"
  , "safe-coerce"
  , "safely"
  , "scrypt"
  , "selection-foldable"
  , "semirings"
  , "server-sent-events"
  , "setimmediate"
  , "signal"
  , "simple-ajax"
  , "simple-emitter"
  , "simple-i18n"
  , "simple-json"
  , "simple-jwt"
  , "simple-ulid"
  , "sized-matrices"
  , "sized-vectors"
  , "slug"
  , "snabbdom"
  , "sodium"
  , "soundfonts"
  , "sparse-matrices"
  , "sparse-polynomials"
  , "spec"
  , "spec-discovery"
  , "spec-mocha"
  , "spec-quickcheck"
  , "spork"
  , "st"
  , "string-parsers"
  , "strings"
  , "strings-extra"
  , "stringutils"
  , "subcategory"
  , "substitute"
  , "subtlecrypto"
  , "suggest"
  , "sunde"
  , "supply"
  , "systemd-journald"
  , "tailrec"
  , "test-unit"
  , "text-encoding"
  , "thermite"
  , "thermite-dom"
  , "these"
  , "toppokki"
  , "transformers"
  , "tree-rose"
  , "tuples"
  , "turf"
  , "two-or-more"
  , "type-equality"
  , "typedenv"
  , "typelevel"
  , "typelevel-lists"
  , "typelevel-peano"
  , "typelevel-prelude"
  , "uint"
  , "undefinable"
  , "undefined"
  , "undefined-is-not-a-problem"
  , "undefined-or"
  , "unfoldable"
  , "unicode"
  , "unordered-collections"
  , "unorm"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "untagged-union"
  , "uri"
  , "url-regex-safe"
  , "uuid"
  , "validation"
  , "variant"
  , "vectorfield"
  , "veither"
  , "versions"
  , "vexceptt"
  , "web-clipboard"
  , "web-cssom"
  , "web-dom"
  , "web-dom-parser"
  , "web-dom-xpath"
  , "web-encoding"
  , "web-events"
  , "web-fetch"
  , "web-file"
  , "web-html"
  , "web-promise"
  , "web-resize-observer"
  , "web-socket"
  , "web-storage"
  , "web-streams"
  , "web-touchevents"
  , "web-uievents"
  , "web-url"
  , "web-xhr"
  , "which"
  , "yaml-next"
  , "yargs"
  , "zeta"
  , "zeta-extra"
  ]
, packages = ./packages.dhall
, sources = [] : List Text
}
