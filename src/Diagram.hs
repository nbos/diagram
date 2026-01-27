{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO
import Options.Applicative
import qualified Control.Monad.Random.Lazy as R
import Control.Monad.Random.Lazy (StdGen)
import Control.Monad.Trans.Random.Lazy (RandT,evalRandT)

import Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q

import qualified Diagram.Joints as Jts
import qualified Diagram.JointType as JT
import qualified Diagram.TrainJointType as TJT
import Diagram.Progress (withPB)

data Options = Options
  { optFilename :: !FilePath
  , optSeed     :: !(Maybe Int)
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
  stdGen <- maybe R.initStdGen (return . R.mkStdGen) $
            optSeed opts

  sz <- fromInteger @Int <$> hFileSize h
  (jts, _) <- Jts.fromStream $
              S.zip (S.enumFrom 0) $
              S.map fromEnum $
              withPB sz "Counting joints" $
              Q.unpack $ Q.fromHandle h

  let tjt = TJT.fromJoints jts

  let go :: RandT StdGen IO ()
      go = do
        (tjt',rtjt) <- TJT.genRefinement tjt
        lift $ putStrLn $
          "generated refinement type with size "
          ++ show (JT.size $ TJT.jointType rtjt)
          ++ " covering " ++ show (Jts.size $ TJT.joints rtjt)
          ++ " joints out of " ++ show (Jts.size $ TJT.joints tjt)

        lift $ putStr "refinement is "
        if TJT.isLUB rtjt
          then lift $ putStrLn $ green "LUB" ++ " of its joints"
          else do lift $ putStrLn $ red "not LUB" ++ " of its joints"
                  lift $ putStrLn $ "rtjt: " ++ show rtjt
                  error "LUB error"

        lift $ putStr "refinement is "
        if TJT.jointType rtjt `JT.leq` TJT.jointType tjt'
          then lift $ putStrLn $ green "subtype" ++ " of its parent"
          else do lift $ putStrLn $ red "not subtype" ++ " of its parent"
                  lift $ putStrLn $ "tjt: " ++ show tjt
                    ++ "\ntjt': " ++ show tjt'
                    ++ "\nrtjt: " ++ show rtjt
                  error "subtype error"

        lift $ putStr "split "
        if TJT.joints tjt == (TJT.joints rtjt `Jts.union` TJT.joints tjt')
          then lift $ putStrLn $ green "preserves" ++ " all joints"
          else do lift $ putStrLn $ red "does not preserve" ++ " all joints"
                  lift $ putStrLn $ "tjt: " ++ show tjt
                    ++ "\ntjt': " ++ show tjt'
                    ++ "\nrtjt: " ++ show rtjt
                  error "joints split error"
        go

  evalRandT go stdGen

red :: String -> String
red s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"
