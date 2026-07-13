module Diagram.Pretty (module Diagram.Pretty) where

import Text.Pretty.Simple
import Data.Text.Lazy (unpack)

import Diagram.String
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT

import Diagram.Util

pShow :: Show a => a -> [Char]
pShow = unpack . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsCompact       = True
  , outputOptionsCompactParens = True -- keeps parens inline too
  , outputOptionsIndentAmount  = 2
  }

pShowStr :: JointType -> JointType -> [Sym] -> [Char]
pShowStr jt jt' = go
  where
    go [] = normal
    go [s] = red ++ show s ++ normal
    go (s0:s1:ss)
      | mem s0 s1 =
          (if not (mem' s0 s1) then normal ++ "***" else "") -- del i0
          ++ normal ++ "(" ++ green ++ show s0 ++ " "
          ++ (case ss of s2:_ | not (mem s1 s2)
                              , mem' s1 s2 -> normal ++ "***" ++ green -- add i1
                         _else -> "")
          ++ show s1 ++ normal ++ ") "
          ++ go ss

      | mem' s0 s1 = normal ++ "***" ++ red ++ show s0 ++ " " ++ go (s1:ss) -- add i0
      | otherwise = red ++ show s0 ++ " "
                    ++ go (s1:ss)
      where
        mem = (`JT.member` jt) .: (,)
        mem' = (`JT.member` jt') .: (,)

    red = "\ESC[91m"
    green = "\ESC[32m"
    normal = "\ESC[0m"
