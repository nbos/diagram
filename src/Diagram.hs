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
import Data.Maybe
import qualified Data.Strict.Tuple as Strict
import qualified Data.Vector.Unboxed as U

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q
import Diagram.Streaming ()

import qualified Diagram.Doubly as D
import qualified Diagram.Joints as Jts
import qualified Diagram.JointType as JT
import qualified Diagram.Model as Mdl
import Diagram.Progress (withPB)

import qualified Diagram.JointType.Random as Ref

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

  -- can't inspect seed at init or deconstruct to seed, so we gen a
  -- random StdGen seed with a StdGen
  seedStdGen <- R.initStdGen
  let seed = fromMaybe (evalRand getRandom seedStdGen)
             (optSeed opts)
      stdGen = R.mkStdGen64 seed
  putStr "Using seed: " >> print seed

  -- read file
  h <- openFile (optFilename opts) ReadMode
  sz <- fromInteger @Int <$> hFileSize h

  (dly,(mdl,())) <- D.fromStream @_ @U.MVector sz $
                    S.map fromEnum $
                    Mdl.emptyFromAtoms $
                    S.copy $
                    withPB sz "Counting symbols" $
                    Q.unpack $ Q.fromHandle h

  (jtniss,()) <- Jts.fromStream $
                 withPB sz "Counting joints" $
                 D.streamWithKey dly

  -- forM types
  let jtns = Strict.fst <$> jtniss
      jt = JT.fromJoints jtns
  putStr "Top type: " >> print jt

  let jtns2 = Jts.doubleIndex 256 jtns -- IntMap (IntMap a)
      jtns2S = Jts.sized jtns2 -- Map Sym (Map Sym a)

  let go :: RandT StdGen IO ()
      go = do

        (rjt,rjtns) <- Ref.genRandom jtns2S

        -- report stats, verify properties/integrity
        Ref.printInfo (jt,jtns) (rjt,rjtns)
        lift $ print rjt

        Ref.printLUB rjt rjtns
        Ref.printSubtyping (jt,jtns) (rjt,rjtns)
        Ref.printConservation (jt,jtns) (rjt,rjtns)
        Ref.printMembership jtns (rjt,rjtns)
        lift $ putStrLn ""
        --

        go -- loop

  -- run loop -------
  evalRandT go stdGen
  -------------------
