{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO
import Options.Applicative
-- import Control.Monad.Random.Lazy (evalRand,mkStdGen)

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q

import qualified Diagram.Joints as J
import qualified Diagram.EM as EM
import Diagram.Progress (withPB)

data Options = Options
  { optFilename :: !FilePath
  , optSeed     :: !Int
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
  ( metavar "FILENAME"
    <> help "Input text file" )
  <*> option auto
  ( long "seed"
    <> short 's'
    <> metavar "SEED"
    <> value 0
    <> help "Random seed (default: 0)" )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "Chunking with joints and unions"
      <> header "diagram" )

  h <- openFile (optFilename opts) ReadMode
  sz <- fromInteger @Int <$> hFileSize h
  (joints, _) <- J.fromStream $
                      S.zip (S.enumFrom 0) $
                      S.map fromEnum $
                      withPB sz "Counting joints" $
                      Q.unpack $ Q.fromHandle h

  EM.em joints
