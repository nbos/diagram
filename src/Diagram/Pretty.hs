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
import Diagram.Evolution.Mutation (Mutation)
import qualified Diagram.Evolution.Mutation as Mut

import Diagram.Util

pShow :: Show a => a -> [Char]
pShow = unpack . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsCompact       = True
  , outputOptionsCompactParens = True -- keeps parens inline too
  , outputOptionsIndentAmount  = 2
  }

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
    -- ansi codes
    reset  = "\ESC[0m"
    green  = "\ESC[32m"
    red    = "\ESC[91m"
    yellow = "\ESC[93m"

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

    -- ansi codes
    reset  = "\ESC[0m"
    green  = "\ESC[32m"
    red    = "\ESC[91m"
    yellow = "\ESC[93m"
