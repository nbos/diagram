module Diagram.Pretty (module Diagram.Pretty) where

import Text.Pretty.Simple
import Data.Text.Lazy (unpack)

import Diagram.String
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT

pShow :: Show a => a -> [Char]
pShow = unpack . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsCompact       = True
  , outputOptionsCompactParens = True -- keeps parens inline too
  , outputOptionsIndentAmount  = 2
  }

pShowStr :: (Sym -> Bool) -> Bool -> JointType -> [Sym] -> [Char]
pShowStr _ _ _ [] = []
pShowStr mark sign jt (hd:tl) = goOut hd tl
  where
    mem = JT.member

    -- s0 not yet printed
    goOut s0 [] = show' red s0 -- end
    goOut s0 (s1:ss)
      | (s0,s1) `mem` jt =
          "[" ++ show' green s0 ++ " " ++ show' green s1 ++ goInEven s1 ss
      | otherwise = show' red s0 ++ " " ++ goOut s1 ss

    -- s0 already printed
    goInEven _ [] = "]" -- end
    goInEven s0 (s1:ss)
      | (s0,s1) `mem` jt = " " ++ goInOdd s1 ss
      | otherwise = "] " ++ goOut s1 ss

    -- s0 not yet printed
    goInOdd s0 [] = show' yellow s0 ++ "]" -- end
    goInOdd s0 (s1:ss)
      | (s0,s1) `mem` jt =
          show' green s0 ++ " " ++ show' green s1 ++ goInEven s1 ss
      | otherwise = show' yellow s0 ++ "] " ++ goOut s1 ss

    show' color s = (if mark s then "*" else "")
                    ++ color ++ show s ++ reset
    -- ansi
    reset  = "\ESC[0m"
    (green, red)
      | sign      = ("\ESC[32m", "\ESC[91m")
      | otherwise = ("\ESC[91m", "\ESC[32m")
    yellow = "\ESC[93m"
