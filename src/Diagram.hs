{-# LANGUAGE TypeApplications #-}
module Diagram (module Diagram) where

import System.IO (openFile, hFileSize, IOMode(ReadMode))
import Options.Applicative

import qualified Data.Map.Strict as M

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q

import Diagram.EM
import Diagram.Model
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
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
      <> progDesc "Chunking with joints and unions"
      <> header "diagram" )

  h <- openFile (optFilename opts) ReadMode
  sz <- fromInteger @Int <$> hFileSize h
  (jointCounts, _) <- countJointsM $
                      S.map fromEnum $
                      withPB sz "Counting joints" $
                      Q.unpack $ Q.fromHandle h

  let (as, bs) = split (optSeed opts) $
                 M.keys jointCounts

  print as
  print bs


