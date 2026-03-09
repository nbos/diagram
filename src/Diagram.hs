{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO
import Options.Applicative
import System.Random (StdGen)
import qualified System.Random as R
import Control.Monad.Trans.Random.Lazy (RandT,evalRand,evalRandT)
import Control.Monad.Random.Class

import Data.Word
import Data.Maybe
import qualified Data.Map.Strict as M

import Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q

import qualified Diagram.Joints as Jts
import qualified Diagram.JointType as JT
import qualified Diagram.Refinement as JTR
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

  h <- openFile (optFilename opts) ReadMode

  -- can't inspect seed at init or deconstruct to seed, so we gen a
  -- random StdGen seed with a StdGen
  seedStdGen <- R.initStdGen
  let seed = fromMaybe (evalRand getRandom seedStdGen)
             (optSeed opts)
      stdGen = R.mkStdGen64 seed
  putStr "Using seed: " >> print seed

  sz <- fromInteger @Int <$> hFileSize h
  (jts, _) <- Jts.fromStream $ -- Map (Sym,Sym) a
              S.zip (S.enumFrom 0) $
              S.map fromEnum $
              withPB sz "Counting joints" $
              Q.unpack $ Q.fromHandle h

  let jt = JT.fromJoints jts
      jts2 = Jts.doubleIndex 256 jts -- IntMap (IntMap a)
      jts2S = Jts.sized jts2 -- Map Sym (Map Sym a)

  let go :: RandT StdGen IO ()
      go = do
        (rjt,rjts) <- JTR.genRefinement jts2S

        -- report stats, verify properties/integrity
        JTR.printInfo (jt,jts) (rjt,rjts)
        JTR.printLUB rjt rjts
        JTR.printSubtyping (jt,jts) (rjt,rjts)
        JTR.printConservation (jt,jts) (rjt,rjts)
        JTR.printCoverage jts (rjt,rjts)
        --

        go -- loop

  evalRandT go stdGen

red :: String -> String
red s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"
