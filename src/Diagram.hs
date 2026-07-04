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

import Control.Monad.Trans.Random.Lazy (RandT,evalRand,evalRandT)
import Control.Monad.State.Strict (MonadTrans(lift))
import Control.Monad.Random.Class (MonadRandom(getRandom))

import Data.Word (Word64)

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q
import Diagram.Streaming ()

import qualified Diagram.Joints as Jts
import qualified Diagram.JointType as JT
import qualified Diagram.ConstrIntervals as CIs
import Diagram.Progress (withPB)

import qualified Diagram.JointType.Random as Gen

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
  sz <- fromInteger @Int <$> hFileSize h

  (allJointCIs, ()) <- CIs.fromStream $
                       S.zip (S.enumFrom 0) $
                       S.map fromEnum $
                       withPB sz "Initializing string" $
                       Q.unpack $ Q.fromHandle h

  let allJointCounts = CIs.jointCount <$> allJointCIs
      top = JT.fromJoints allJointCounts
      joints2S = Jts.sized $ Jts.doubleIndex 256 allJointCounts

  putStr "Top type: " >> print top
  case () of
    _ -> evalRandT go stdGen -- run

      where -- MAIN LOOP --
      go :: RandT StdGen IO ()
      go = do
        (jt,jtns) <- Gen.genRandom joints2S

        -- report stats, verify properties/integrity
        Gen.printInfo (top, allJointCounts) (jt, jtns)
        lift $ print jt
        Gen.printLUB jt jtns
        Gen.printSubtyping (top, allJointCounts) (jt, jtns)
        Gen.printConservation (top, allJointCounts) (jt, jtns)
        Gen.printMembership allJointCounts (jt, jtns)
        lift $ putStrLn ""
        --

        go -- repeat
