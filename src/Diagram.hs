{-# LANGUAGE TypeApplications, LambdaCase #-}
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

import Control.Lens hiding (both,last1,argument)
import Control.Monad.Trans.Random.Lazy (RandT,evalRand,evalRandT)
import Control.Monad.State.Strict
    (MonadIO(liftIO), MonadTrans(lift), evalStateT)
import Control.Monad.Random.Class (MonadRandom(getRandom))

import Data.Word (Word64)
import Data.Maybe
import qualified Data.Strict.Tuple as Strict

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as Q
import Diagram.Streaming ()

import qualified Diagram.Doubly as D
import Diagram.Joints (constructive)
import qualified Diagram.Joints as Jts
import qualified Diagram.JointType as JT
import qualified Diagram.Refinement as Ref
import Diagram.Refinement (refinement)
import qualified Diagram.Model as Mdl
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

  (ss,(mdl,())) <- D.fromStream sz $
                   S.map fromEnum $
                   Mdl.emptyFromAtoms $
                   S.copy $
                   withPB sz "Counting symbols" $
                   Q.unpack $ Q.fromHandle h

  (jtniss,()) <- Jts.fromStream $
                 withPB sz "Counting joints" $
                 D.streamWithKey ss

  -- form types
  let jtns = Strict.fst . (^.constructive) <$> jtniss
      jt = JT.fromJoints jtns
  putStr "Top type: " >> print jt

  let jtns2 = Jts.doubleIndex 256 jtns -- IntMap (IntMap a)
      jtns2S = Jts.sized jtns2 -- Map Sym (Map Sym a)

  -- freeze params
  params <- Mdl.params mdl

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

        lift $ putStrLn "Beginning hill climb."
        let rjts2 = Jts.doubleIndex 256 rjtns
            rst0 = Ref.initState params ss rjts2 rjt

            go' = (Ref.stepHillClimb >>=) $ \case
              Just t -> return t
              Nothing -> use refinement
                         >>= liftIO . print
                         >> go'

        rjt' <- evalStateT go' rst0
        lift $ putStr "Final ref: " >> print rjt'

        lift $ putStrLn ""
        go -- loop

  -- run loop -------
  evalRandT go stdGen
  -------------------
