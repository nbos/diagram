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
import qualified Diagram.Refinement as Refinement
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
  (jts, _) <- Jts.fromStream $
              S.zip (S.enumFrom 0) $
              S.map fromEnum $
              withPB sz "Counting joints" $
              Q.unpack $ Q.fromHandle h

  let jt = JT.fromJoints jts
      byFst = Jts.byFstSized jts
      bySnd = Jts.bySndSized 256 jts

  let go :: RandT StdGen IO ()
      go = do
        (rjt,rjts_A) <- Refinement.genRefinement byFst bySnd
        let jts' = jts M.\\ rjts_A
            rjts_B = M.filterWithKey (\k _ -> k `JT.member` rjt) jts

        lift $ putStrLn $
          "generated refinement type with size "
          ++ show (JT.size rjt)
          ++ " from "  ++ show (JT.size jt)
          ++ " covering " ++ show (Jts.size rjts_B)
          ++ " joints out of " ++ show (Jts.size jts)
          ++ " ("  ++ show
          (round @_ @Int $ 100.0 * fromIntegral (Jts.size rjts_B)
            / fromIntegral @_ @Double (Jts.size jts))
          ++ "%)"

        lift $ putStr "refinement is "
        if rjt == JT.fromJoints rjts_B
          then lift $ putStrLn $ green "LUB" ++ " of its joints"
          else do lift $ putStrLn $ red "not LUB" ++ " of its joints"
                  lift $ putStrLn $ "rtjt: " ++ show (rjt,rjts_B)
                  error "LUB error"

        lift $ putStr "refinement is "
        if rjt `JT.leq` jt
          then lift $ putStrLn $ green "subtype" ++ " of its parent"
          else do lift $ putStrLn $ red "not subtype" ++ " of its parent"
                  lift $ putStrLn $ "tjt: " ++ show (jt,jts)
                    ++ "\ntjt': " ++ show (jt,jts')
                    ++ "\nrtjt: " ++ show (rjt,rjts_B)
                  error "subtype error"

        lift $ putStr "split "
        if jts == (rjts_B `Jts.union` jts')
          then lift $ putStrLn $ green "preserves" ++ " all joints"
          else do lift $ putStrLn $ red "does not preserve" ++ " all joints"
                  lift $ putStrLn $ "tjt: " ++ show (jt,jts)
                    ++ "\ntjt': " ++ show (jt,jts')
                    ++ "\nrtjt: " ++ show (rjt,rjts_B)
                  error "joints split error"

        lift $ putStr "returned joints "
        if M.keys rjts_B == M.keys rjts_A
          then lift $ putStrLn $ green "match" ++ " joints covered by the refinement"
          else do lift $ putStrLn $ red "don't match" ++ " joints covered by the refinement"
                  lift $ putStrLn $ "rtjt: " ++ show (M.keys rjts_B)
                    ++ "\nrjts: " ++ show (M.keys rjts_A)
                  error "joints coverage error"

        go -- loop

  evalRandT go stdGen

red :: String -> String
red s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"
