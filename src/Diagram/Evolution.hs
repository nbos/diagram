{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

module Diagram.Evolution (module Diagram.Evolution) where

import System.Random (StdGen)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Random.Lazy (RandT)
import Control.Lens hiding (both,last1,Index,(:>),index)

import Text.Printf
import Data.Text (unpack)
import Data.Ord
import Data.Word
import Data.Tuple
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

import Data.Hashable
import qualified Data.Map.Strict as M

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive
import Diagram.Streaming ()
import qualified Data.Vector.Unboxed as U

import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType.Random as JT

import Diagram.Joints (Joints)
import qualified Diagram.Joints as Jts
import Diagram.ConstrIntervals.Internal (CIs)
import qualified Diagram.ConstrIntervals.Internal as CIs

import Diagram.Evolution.MutEntry (MutEntry(ME))
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution.Run (RunState)
import qualified Diagram.Evolution.Run as Run

data EvolutionState = EvolutionState

-- | Keep generating random JointTypes and hill-climbing to minima,
-- printing results.
searchForever :: forall m. (MonadIO m, PrimMonad m) => Int -> Int ->
  Doubly (PrimState m) -> U.Vector Int -> Joints CIs -> RandT StdGen m ()
searchForever m bigN dly ns allCIs = go 1
  where
    allCIs2s = Jts.sized $ Jts.doubleIndex m allCIs

    go :: Int -> RandT StdGen m ()
    go !num = do
      (jt@(JT u0 u1), cis) <- JT.genRandom allCIs2s
      liftIO $ putStrLn $ "Generated random type [" ++ shortHashStr jt ++ "]:"
      liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0
      liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1 ++ " )"
      hillClimb m bigN dly ns allCIs (jt, cis)
      go (num+1) -- repeat

-- | Hill climb to a local minimum, given parameters m (number of
-- symbols), N (string length), the string, symbol counts, constructive
-- intervals of all joints in the string and a joint type with its CIs
-- indexed by joint.
hillClimb :: forall m. (PrimMonad m, MonadIO m) => Int -> Int ->
             Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
             (JointType, Joints CIs) -> m ()
hillClimb m bigN dly ns allCIs = Run.init_ m bigN dly ns allCIs
                                 >=> go
  where
    go :: RunState (PrimState m) -> m ()
    go st = do
      let jt = Run.jointType st
          h = shortHashStr jt
      (ddInfo, me@(ME mut _ _ _ _)) <- Run.bestMut st
      if ddInfo <= 0
        then do
        liftIO $ putStr $ "" ++ gray ++ h ++ reset ++ " "
        liftIO $ putStr $ printf "%-22s" (pShowMut mut)
        liftIO $ putStr $ printf "%-11.4e" ddInfo
        mutJts <- TS.jointsOfMut (st^.Run.typeState) mut
        let maxMutJts :: [(Sym, Sym)] -- sorted by significance
            maxMutJts = fmap snd $ L.sortOn (Down . fst) $ fmap swap $ M.toList $
                        M.intersectionWith (const . CIs.jointCount) allCIs mutJts
        liftIO $ putStrLn $ showSomeJoints 60 maxMutJts
        execStateT (Run.pushMut me) st >>= go -- rec

        else do
        let jt'@(JT u0' u1') = st^.Run.typeCIs.CIs.jointType
        liftIO $ putStrLn $ "Minimal type achieved [" ++ shortHashStr jt' ++ "]:"
        liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0'
        liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1' ++ " )"
        typJts <- M.fromList . fmap (,()) <$> TS.toJoints (st^.Run.typeState)
        let maxTypJts :: [(Sym, Sym)] -- sorted by significance
            maxTypJts = fmap snd $ L.sortOn (Down . fst) $ fmap swap $ M.toList $
                        M.intersectionWith (const . CIs.jointCount) allCIs typJts

        liftIO $ putStrLn $ showSomeJoints 999 maxTypJts
        liftIO $ putStrLn "" -- end

hashStr :: Hashable a => a -> String
hashStr = printf ("%016x") . hash

shortHashStr :: Hashable a => a -> String
shortHashStr = take 7 . printf ("%016x") . hash

-- provisionally, only for Syms < 256
showSyms :: [Word8] -> String
showSyms = escapeChars . unpack . TE.decodeUtf8Lenient . BS.pack

escapeChars :: String -> String
escapeChars = concatMap $ \case
  '\n' -> "\\n"  -- newline with \n
  '\t' -> "\\t"  -- tab with \t
  '\r' -> "\\r"  -- carriage return with \r
  '\\' -> "\\\\" -- backslash with \\
  '\"' -> "\\\"" -- double quote with \"
  '\'' -> "\\'"  -- single quote with \'
  -- ','  -> "\\,"  -- for CSV
  c    -> [c]

showSomeJoints :: Int -> [(Sym, Sym)] -> String
showSomeJoints width ss
  | width < 5 = pad (if null ss && width >= 2 then gray ++ "[]" ++ reset
                     else gray ++ take width "[...]" ++ reset)
  | otherwise = pad (gray ++ '[':reset ++ go "" (width - 1) ss)
  where
    go :: String -> Int -> [(Sym,Sym)] -> String
    go _ _ [] = "]"
    go sep w ((s0,s1):rest)
      | null rest && len + 1 <= w = sep' ++ str' ++ gray ++ "]" ++ reset
      | len + 5 <= w = sep' ++ str' ++ go "," (w - len) rest
      | otherwise = sep' ++ gray ++ "...]" ++ reset
      where
        str = showSyms [toEnum s0, toEnum s1]
        len = length sep + length str + 2
        str' = gray ++ '"':reset ++ str ++ gray ++ "\"" ++ reset
        sep' = gray ++ sep ++ reset

    pad s = s ++ replicate (width - length s) ' '
