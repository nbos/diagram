module Diagram.Pretty (module Diagram.Pretty) where

import Text.Pretty.Simple
import Data.Text.Lazy (unpack)

pp :: Show a => a -> [Char]
pp = unpack . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsCompact       = True
  , outputOptionsCompactParens = True -- keeps parens inline too
  , outputOptionsIndentAmount  = 2
  }
