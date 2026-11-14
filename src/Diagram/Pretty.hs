{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
module Diagram.Pretty (module Diagram.Pretty) where

import Control.Monad.Extra
import Text.Pretty.Simple hiding (pShow)
import Data.Text.Lazy (unpack)

import Streaming
import qualified Streaming.Prelude as S

import Diagram.String
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
import Diagram.Evolution.Mutation (Mutation(..))
import qualified Diagram.Evolution.Mutation as Mut

import Diagram.Util

pShow :: Show a => a -> [Char]
pShow = unpack . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsCompact       = True
  , outputOptionsCompactParens = True -- keeps parens inline too
  , outputOptionsIndentAmount  = 2 }

---------------------
-- TYPES ON STRING --
---------------------

pShowStrMut :: Mutation -> JointType -> JointType -> [Sym] -> [Char]
pShowStrMut mut mutJT typJT str =
  "Constructions before:\n" ++ pShowStr typJT str ++ "\n\n"
  ++ "Delta (" ++ pShow mut ++ "):\n" ++ pShowStr mutJT str ++ "\n\n"
  ++ "Constructions after:\n" ++ pShowStr typJT' str ++ "\n"
  where
    typJT' = Mut.apply mut typJT

pShowStr :: JointType -> [Sym] -> [Char]
pShowStr = pShowStrMark $ const False

pShowStrMark :: (Sym -> Bool) -> JointType -> [Sym] -> [Char]
pShowStrMark _ _ [] = []
pShowStrMark mark jt (hd:tl) = goOut hd tl
  where
    -- s0 not yet printed
    goOut s0 [] = show' red s0 -- end
    goOut s0 (s1:ss)
      | JT.member jt s0 s1 =
          "[" ++ show' green s0 ++ " " ++ show' green s1 ++ goInEven s1 ss
      | otherwise = show' red s0 ++ " " ++ goOut s1 ss

    -- s0 already printed
    goInEven _ [] = "]" -- end
    goInEven s0 (s1:ss)
      | JT.member jt s0 s1 = " " ++ goInOdd s1 ss
      | otherwise = "] " ++ goOut s1 ss

    -- s0 not yet printed
    goInOdd s0 [] = show' yellow s0 ++ "]" -- end
    goInOdd s0 (s1:ss)
      | JT.member jt s0 s1 =
          show' green s0 ++ " " ++ show' green s1 ++ goInEven s1 ss
      | otherwise = show' yellow s0 ++ "] " ++ goOut s1 ss

    show' color s = (if mark s then "*" else "")
                    ++ color ++ show s ++ reset

-- | Streaming + monadic version of pShowStr
pShowStream :: Monad m => (Sym -> Sym -> m Bool) ->
               Stream (Of Sym) m r -> m [Char]
pShowStream = S.toList_ .: pShowStreamMark_ (const $ return False)

-- | Streaming version of pShowStrMark
pShowStreamMark_ :: Monad m => (Sym -> m Bool) -> (Sym -> Sym -> m Bool) ->
                    Stream (Of Sym) m r -> Stream (Of Char) m r
pShowStreamMark_ mMark mMem str0 = (next str0 >>=) $ \case
  Left r -> return r
  Right (s0, str0') -> goOut s0 str0'
  where
    mark = lift . mMark
    mem = lift .: mMem
    next = lift . S.next

    -- s0 not yet printed
    goOut s0 str = (next str >>=) $ \case
      Left r -> show' red s0 >> return r
      Right (s1, ss) -> (mem s0 s1 >>=) $ \case
        True -> do S.yield '[' >> show' green s0
                   S.yield ' ' >> show' green s1 >> goInEven s1 ss
        False -> show' red s0 >> S.yield ' ' >> goOut s1 ss

    -- s0 already printed
    goInEven s0 str = (next str >>=) $ \case
      Left r -> S.yield ']' >> return r
      Right (s1, ss) -> (mem s0 s1 >>=) $ \case
        True -> S.yield ' ' >> goInOdd s1 ss
        False -> S.each "] " >> goOut s1 ss

    -- s0 not yet printed
    goInOdd s0 str = (next str >>=) $ \case
      Left r -> show' yellow s0 >> S.yield ']' >> return r
      Right (s1, ss) -> (mem s0 s1 >>=) $ \case
        True -> do show' green s0 >> S.yield ' '
                   show' green s1 >> goInEven s1 ss
        False -> show' yellow s0 >> S.each "] " >> goOut s1 ss

    show' color s = do
      whenM (mark s) (S.yield '*')
      S.each color >> S.each (show s) >> S.each reset

---------------
-- MUTATIONS --
---------------

pShowMut :: Mutation -> String
pShowMut = pShowMut_ 3 -- for 3-char numbers, [100..999]

pShowMut_ :: Int -> Mutation -> String
pShowMut_ width (AddLeft  s0) = "AddLeft  " ++ green ++ str0 ++ reset ++ pad
  where str0 = show s0
        pad = replicate (width - length str0) ' '
pShowMut_ width (AddRight s1) = "AddRight " ++ green ++ str1 ++ reset ++ pad
  where str1 = show s1
        pad = replicate (width - length str1) ' '
pShowMut_ width (Add2  s0 s1) = "Add2 " ++ green ++ str0 ++ pad
                                ++ ' ':str1 ++ reset
  where str0 = show s0
        str1 = show s1
        pad = replicate (width - length str0) ' '
pShowMut_ width (DelLeft  s0) = "DelLeft  " ++ red ++ str0 ++ reset ++ pad
  where str0 = show s0
        pad = replicate (width - length str0) ' '
pShowMut_ width (DelRight s1) = "DelRight " ++ red ++ str1 ++ reset ++ pad
  where str1 = show s1
        pad = replicate (width - length str1) ' '
pShowMut_ width (Del2  s0 s1) = "Del2 " ++ red ++ str0 ++ pad
                                ++ ' ':str1 ++ reset
  where str0 = show s0
        str1 = show s1
        pad = replicate (width - length str0) ' '

----------
-- ANSI --
----------

reset :: String
reset = "\ESC[0m"

green :: String
green = "\ESC[32m"

red :: String
red = "\ESC[91m"

yellow :: String
yellow = "\ESC[93m"

white :: String
white = "\ESC[37m"

gray :: String
gray = "\ESC[90m"
