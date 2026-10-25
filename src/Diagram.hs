{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO
    ( BufferMode(NoBuffering),
      IOMode(ReadMode),
      hSetBuffering,
      stdout,
      openFile,
      hFileSize )
import qualified Options.Applicative as Opts
import Options.Applicative
    ( Parser,
      argument,
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

import qualified System.Random as R

import Control.Monad.Trans.Random.Lazy (evalRand,evalRandT)
import Control.Monad.Random.Class (MonadRandom(getRandom))

import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q
import Diagram.Streaming ()

import qualified Diagram.Doubly as D
import qualified Diagram.Model as Mdl
import qualified Diagram.ConstrIntervals.Internal as CIs
import qualified Diagram.Evolution as Evo

import Diagram.Progress (withPB)

data Options = Options
  { optFilename :: !FilePath
  , optSeed     :: !(Maybe Word64)
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> argument Opts.str
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
    S.map fromEnum $ -- i8 -> i64
    fmap fst $ -- discard ()
    Mdl.countAtoms $ S.copy $ -- ns
    withPB bigN "Initializing string" $
    Q.unpack $ Q.fromHandle h
  putStrLn ""

  str <- D.toList dly
  putStrLn "String: " >> print str
  putStrLn ""

  let m = 256 :: Int

  flip evalRandT stdGen $ -- go
    Evo.searchForever m bigN dly ns allCIs
