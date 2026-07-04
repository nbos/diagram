{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO
    ( BufferMode(NoBuffering),
      IOMode(ReadMode),
      hSetBuffering,
      stdout,
      openFile,
      hFileSize )
import Options.Applicative
    ( Parser,
      argument,
      str,
      metavar,
      help,
      optional,
      option,
      auto,
      long,
      short,
      execParser,
      info,
      (<**>),
      helper,
      fullDesc,
      progDesc,
      header )
import System.Random (StdGen)
import qualified System.Random as R

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Random.Lazy (RandT,evalRand,evalRandT)
import Control.Monad.State.Strict (MonadTrans(lift))
import Control.Monad.Random.Class (MonadRandom(getRandom))

import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q
import Diagram.Streaming ()
import Diagram.String

import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
import qualified Diagram.JointType.Random as JT

import qualified Diagram.Doubly as D
import qualified Diagram.Joints as Jts
import qualified Diagram.Model as Mdl
import qualified Diagram.ConstrIntervals as CIs
import qualified Diagram.Evolution as Evo

import Diagram.Progress (withPB)

data Options = Options
  { optFilename :: !FilePath
  , optSeed     :: !(Maybe Word64)
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
  ( metavar "FILENAME"
    <> help "Input text file" )
  <*> optional
  (option auto
    ( long "seed"
      <> short 's'
      <> metavar "SEED"
      <> help "Set random seed" ))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "Chunking with joints and unions"
      <> header "diagram" )

  -- Random won't let you inspect seed at init or deconstruct to seed,
  -- so we manually gen a StdGen seed based on a random StdGen
  seed <- case optSeed opts of
    Just sd -> return sd
    Nothing -> evalRand getRandom <$> R.initStdGen
  let stdGen = R.mkStdGen64 seed
  putStr "Using seed: " >> print seed

  -- read file
  h <- openFile (optFilename opts) ReadMode
  bigN <- fromInteger @Int <$> hFileSize h

  (allCIs, (dly, ns)) <-
    CIs.fromStream $ -- allCIs
    S.zip (S.enumFrom 0) $ -- zip [0..]
    D.fromStream @_ @U.MVector bigN $ S.copy $ -- dly
    S.map fromEnum $ -- Word8 -> Int
    fmap fst $ -- discard ()
    Mdl.countAtoms $ S.copy $ -- ns
    withPB bigN "Initializing string" $
    Q.unpack $ Q.fromHandle h

  let top = JT.fromJoints allCIs
  putStr "Top type: " >> print top

  let allCIs2s = Jts.sized $ Jts.doubleIndex 256 allCIs
      m = 256 :: Int

  case () of
    _ -> evalRandT go stdGen -- run

      where -- MAIN LOOP --
      go :: RandT StdGen IO ()
      go = do
        (jt, cis) <- JT.genRandom allCIs2s

        -- report stats, verify properties/integrity
        printInfo (top, allCIs) (jt, cis)
        lift $ putStr "Generated type: " >> print jt
        printLUB jt cis
        printSubtyping (top, allCIs) (jt, cis)
        printConservation (top, allCIs) (jt, cis)
        printMembership allCIs (jt, cis)
        lift $ putStrLn ""
        --

        jt' <- Evo.hillClimb m bigN dly ns allCIs (jt,cis)
        lift $ putStr "Minimal type: " >> print jt'

        go -- repeat

---------------------
-- IO STATS/CHECKS --
---------------------

printInfo :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
             (JointType, Map (Sym,Sym) b) -> m ()
printInfo (jt,jts) (rjt,rjts) = liftIO $ putStrLn $
  "generated refinement type with size "
  ++ show (JT.dims rjt)
  ++ " from "  ++ show (JT.dims jt)
  ++ " covering " ++ show (Jts.size rjts)
  ++ " joints out of " ++ show (Jts.size jts)
  ++ " ("  ++ show
  (round @_ @Int $ 100.0 * fromIntegral (Jts.size rjts)
    / fromIntegral @_ @Double (Jts.size jts))
  ++ "%)"

printLUB :: MonadIO m => JointType -> Map (Sym,Sym) a -> m ()
printLUB jt jts = liftIO $ do
  putStr "refinement is "
  if jt == JT.fromJoints jts
    then putStrLn $ inGreen "LUB" ++ " of its joints"
    else do putStrLn $ inRed "not LUB" ++ " of its joints"
            putStrLn $ "rtjt: " ++ show (jt, void jts)
            error "LUB error"

printSubtyping :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                  (JointType, Map (Sym,Sym) b) -> m ()
printSubtyping (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "refinement is "
  if rjt `JT.leq` jt
    then putStrLn $ inGreen "subtype" ++ " of its parent"
    else do putStrLn $ inRed "not subtype" ++ " of its parent"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "subtype error"

printConservation :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                     (JointType, Map (Sym,Sym) a) -> m ()
printConservation (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "split " -- TODO: check disjointness too?
  if void jts == (void rjts `M.union` void jts')
    then putStrLn $ inGreen "preserves" ++ " all joints"
    else do putStrLn $ inRed "does not preserve" ++ " all joints"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "joints split error"

printMembership :: MonadIO m => Map (Sym,Sym) a -> (JointType, Map (Sym,Sym) a) -> m ()
printMembership jts (rjt,rjts) = liftIO $ do
  let rjtsVerif = M.filterWithKey (\k _ -> k `JT.member` rjt) jts
  putStr "returned joints "
  if M.keys rjts == M.keys rjtsVerif
    then putStrLn $ inGreen "match" ++ " joints covered by the refinement"
    else do putStrLn $ inRed "don't match" ++ " joints covered by the refinement"
            putStrLn $ "rtjt: " ++ show (M.keys rjts)
              ++ "\nrjts: " ++ show (M.keys rjts)
              ++ "\nrjtsVerif: " ++ show (M.keys rjtsVerif)
            error "joints coverage error"

inRed :: String -> String
inRed s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

inGreen :: String -> String
inGreen s = "\ESC[32m" ++ s ++ "\ESC[0m"
