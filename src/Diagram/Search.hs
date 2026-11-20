{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Search (module Diagram.Search) where

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

import Diagram.Joints (Joints, Joints2S)
import qualified Diagram.Joints as Jts
import Diagram.ConstrIntervals.Internal (CIs)
import qualified Diagram.ConstrIntervals.Internal as CIs

import Diagram.Evolution.MutEntry (MutEntry(ME))
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution (EvolutionState)
import qualified Diagram.Evolution as Run

type SearchT m = StateT (SearchState (PrimState m)) m
data SearchState s = SearchState
  { _initsCount    :: !Int
  , _visitedStates :: !(HashMap JointType (Either Double JointType))
  , _minimalStates :: !(Map Double (JointType, Count))
  , _run           :: !(EvolutionState s) -- runState was taken
  , _jointsIndex   :: !(Joints2S CIs) } -- two-way sized index for gen
makeLenses ''SearchState

-- GETTERS --

numSymbols :: Monad m => SearchT m Int
numSymbols = run `uses` Run.numSymbols

getBest :: SearchState s -> (Double, JointType)
getBest = minimalStates `views` (fmap fst . M.findMin)

-----------
-- ENTRY --
-----------

-- | Given parameters, run hill-climbing NUM times, return the best type
hillClimbN :: (PrimMonad m, MonadIO m) => Int -> Int -> Int ->
              Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
              RandT StdGen m (Double, JointType)
hillClimbN num m bigN dly ns allCIs = init m bigN dly ns allCIs
                                      >>= evalStateT go
  where go = do
          hillClimb
          i <- use initsCount
          if i < num then reinit >> go else
            minimalStates `uses` (fmap fst . M.findMin)

-- | Initialize given the right parameters
init :: (PrimMonad m, MonadIO m) => Int -> Int -> Doubly (PrimState m) ->
        U.Vector Int -> Joints CIs -> RandT StdGen m (SearchState (PrimState m))
init m bigN dly ns allCIs = do
  (jt, jtCIs) <- JT.genRandom jtx
  liftIO $ printGen m jt
  flip (SearchState 1 HM.empty M.empty) jtx
    <$> lift (Run.init_ m bigN dly ns allCIs (jt,jtCIs))
  where
    jtx = Jts.sized $ Jts.doubleIndex m allCIs

-- | Re-initialize run state with a freshly sampled random JointType
reinit :: (PrimMonad m, MonadIO m) => SearchT (RandT StdGen m) ()
reinit = do
  initsCount += 1
  run <~ (lift . newRun =<< get)

newRun :: (PrimMonad m, MonadIO m) => SearchState (PrimState m) ->
          RandT StdGen m (EvolutionState (PrimState m))
newRun (SearchState _ _ _ rst jtx) = do
  (jt, jtCIs) <- JT.genRandom jtx
  liftIO $ printGen m jt
  lift $ Run.init_ m bigN dly ns allCIs (jt, jtCIs)
  where
    m = Run.numSymbols rst
    bigN   = rst^.Run.stringLen
    dly    = rst^.Run.doubly
    ns     = rst^.Run.symCounts
    allCIs = rst^.Run.jointCIs

printGen :: Int -> JointType -> IO ()
printGen m jt@(JT u0 u1) = do
  putStrLn $ "Generated random type [" ++ shortHashStr jt ++ "]:"
  putStrLn $ "( 0x" ++ UT.toHex m u0
  putStrLn $ ", 0x" ++ UT.toHex m u1 ++ " )"

-------------------
-- HILL-CLIMBING --
-------------------

hillClimb :: forall m. (MonadIO m, PrimMonad m) => SearchT (RandT StdGen m) ()
hillClimb = do
  m <- numSymbols
  allCIs <- use (run.Run.jointCIs)
  let
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
          return ()

        Nothing -> (use run >>= Run.bestMut >>=) $
          \(ddInfo, me@(ME mut _ _ _ _)) -> if ddInfo <= 0
          then do -- step case
            liftIO $ putStr $ "" ++ gray ++ h ++ reset ++ " "
            liftIO $ putStr $ printf "%-22s" (pShowMut mut)
            liftIO $ putStr $ printf "%-10.3e" ddInfo
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
            return ()

    -- sort joints from high to low joints count (uses allCIs for counts)
    sortJointsDesc :: Map (Sym, Sym) b -> [(Sym, Sym)]
    sortJointsDesc = fmap snd . L.sortOn (Down . fst) . fmap swap . M.toList
                     . M.intersectionWith (const . CIs.jointCount) allCIs
  go

--------------
-- PRINTING --
--------------

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
