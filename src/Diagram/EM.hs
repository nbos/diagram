module Diagram.EM (module Diagram.EM) where

import System.IO
import Control.Monad.Random (MonadRandom)
import Control.Monad.IO.Class

import qualified Data.Map as M

import Diagram.Joints (Joints)
-- import Diagram.Random (split)
import qualified Diagram.JointType as JT

em :: (MonadRandom m, MonadIO m) => Joints -> m ()
em jts = go0
  where
    top = JT.fromJoints jts

    go0 = do
      t <- JT.genRefinement top
      let jts' = M.filterKeys (`JT.member` t) jts
          t' = JT.fromJoints jts'
      liftIO $ if t == t' then putStr "0" else putStr "."
      liftIO $ hFlush stdout
      go0 -- loop

    -- go1 = do
    --   (jts0,jts1) <- split $ M.toAscList jts
    --   let t0 = JT.fromJoints $ M.fromAscList jts0
    --   if not $ any ((`JT.member` t0) . fst) jts1
    --     then liftIO $ putStr "1"
    --     else liftIO $ putStr "'"
    --   go1
