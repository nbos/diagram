{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

module Diagram.Evolution (module Diagram.Evolution) where

import Prelude hiding (init)
import System.Random (StdGen)
import Control.Monad.State.Strict
import Control.Monad.Trans.Random.Lazy (RandT)
import Control.Lens hiding (both,last1,Index,(:>),index)

import Text.Printf

import Data.Ord
import Data.Text (unpack)
import Data.Word
import Data.Tuple
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

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

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
data EvolutionState s = EvolutionState
  { _initsCount    :: !Int
  , _visitedStates :: !(HashMap JointType (Either Double JointType))
  , _minimalStates :: !(Map Double (JointType, Count))
  , _run           :: !(RunState s) } -- runState was taken
makeLenses ''EvolutionState

init :: RunState s -> EvolutionState s
init = EvolutionState 1 HM.empty M.empty

-- | Keep generating random JointTypes and hill-climbing to minima,
-- given parameters m (number of symbols), N (string length), the
-- string, symbol counts, constructive intervals of all joints in the
-- string and a joint type with its CIs indexed by joint.
searchForever :: forall m. (MonadIO m, PrimMonad m) => Int -> Int ->
  Doubly (PrimState m) -> U.Vector Int -> Joints CIs -> RandT StdGen m ()
searchForever m bigN dly ns allCIs = evalStateT go . init =<< newRun
  where
    allCIs2s = Jts.sized $ Jts.doubleIndex m allCIs

    newRun :: RandT StdGen m (RunState (PrimState m))
    newRun = do
      (jt@(JT u0 u1), cis) <- JT.genRandom allCIs2s -- new joint type
      liftIO $ putStrLn $ "Generated random type [" ++ shortHashStr jt ++ "]:"
      liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0
      liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1 ++ " )"
      Run.init_ m bigN dly ns allCIs (jt, cis) -- new run state

    go :: EvolutionT (RandT StdGen m) ()
    go = do
      jt@(JT u0 u1) <- run `uses` Run.jointType
      let h = shortHashStr jt
      (visitedStates `uses` HM.lookup jt >>=) $ \case

        Just next -> do -- collision case
          liftIO $ putStr $ "" ++ yellow ++ h ++ reset ++ " "
          liftIO $ putStr $ "State already visited, ends at ["
          (endHash, dInfo) <- case next of
            Left dInfo -> return (h, dInfo)
            Right jt' -> do
              hm <- use visitedStates
              let followStates a = case hm HM.! a of
                    Left dInfo -> (shortHashStr a, dInfo)
                    Right a'   -> followStates a'
              return $ followStates jt'

          liftIO $ putStr $ endHash ++ "]. Loss was "
          liftIO $ putStrLn $ pShow dInfo ++ "."
          liftIO $ putStrLn "" -- end space

          minimalStates %= M.adjust (second (+1)) dInfo -- +1 visits
          reinit >> go -- restart

        Nothing -> (use run >>= Run.bestMut >>=) $
          \(ddInfo, me@(ME mut _ _ _ _)) -> if ddInfo <= 0
          then do -- step case
            liftIO $ putStr $ "" ++ gray ++ h ++ reset ++ " "
            liftIO $ putStr $ printf "%-22s" (pShowMut mut)
            liftIO $ putStr $ printf "%-11.4e" ddInfo
            mutJts <- flip TS.jointsOfMut mut =<< use (run.Run.typeState)
            let maxMutJts = sortJointsDesc mutJts
            liftIO $ putStrLn $ showSomeJoints 60 maxMutJts
            zoom run (Run.pushMut me)

            jt' <- run `uses` Run.jointType
            visitedStates %= HM.insert jt (Right jt')
            go

          else do -- end/reset case
            liftIO $ putStr $ "Minimal type achieved [" ++ h ++ "]. Loss is "
            dInfo <- run `uses` Run.getIntroInfo
            liftIO $ putStrLn $ pShow dInfo ++ "."

            liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0
            liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1 ++ " )"
            typJts <- use (run.Run.typeState) >>= TS.toJoints
            let maxTypJts = sortJointsDesc $ M.fromList $ fmap (,()) typJts
            liftIO $ putStrLn $ showSomeJoints 999 maxTypJts
            liftIO $ putStrLn "" -- end space

            visitedStates %= HM.insert jt (Left dInfo)
            minimalStates %= M.insert dInfo (jt, 1)
            reinit >> go -- restart

    -- increment counter, reinitialize run
    reinit :: EvolutionT (RandT StdGen m) ()
    reinit = do
      initsCount += 1
      run <~ lift newRun

    -- sort joints from high to low joints count (uses allCIs for counts)
    sortJointsDesc :: Map (Sym, Sym) b -> [(Sym, Sym)]
    sortJointsDesc = fmap snd . L.sortOn (Down . fst) . fmap swap . M.toList
                     . M.intersectionWith (const . CIs.jointCount) allCIs

hashStr :: Hashable a => a -> String
hashStr = printf ("%016x") . hash

shortHashStr :: Hashable a => a -> String
shortHashStr = take 7 . printf ("%016x") . hash

showBytes :: [Word8] -> String
showBytes = escapeChars . unpack . TE.decodeUtf8Lenient . BS.pack

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

-- only for Syms < 256
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
        str = showBytes [toEnum s0, toEnum s1]
        len = length sep + length str + 2
        str' = gray ++ '"':reset ++ str ++ gray ++ "\"" ++ reset
        sep' = gray ++ sep ++ reset

    pad s = s ++ replicate (width - length s) ' '
