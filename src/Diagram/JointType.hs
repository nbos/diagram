{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE InstanceSigs, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase, TupleSections, TypeApplications #-}

module Diagram.JointType (module Diagram.JointType, Sym) where

import GHC.Generics (Generic)
import Control.Monad
import Control.Lens hiding (both,Index)

import Data.Hashable
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Primitive
import Diagram.Information

import Diagram.String
import Diagram.Joints (Joints)
import qualified Diagram.Joints as Jts
import Diagram.UnionType (UnionType(..))
import qualified Diagram.UnionType as UT

import Diagram.Util

-- | A joint of unions
data JointType = JT {
  _left  :: !UnionType, -- s0s
  _right :: !UnionType  -- s1s
} deriving (Eq,Generic,Hashable)
makeLenses ''JointType

instance Show JointType where
  show :: JointType -> String
  show jt = "fromLists " ++ show u0 ++ " " ++ show u1
    where (u0,u1) = toLists jt

------------------
-- CONSTRUCTION --
------------------

singleton :: Sym -> Sym -> JointType
singleton s0 s1 = JT u0 u1
  where u0 = UT.singleton s0
        u1 = UT.singleton s1

fromJoints :: Joints a -> JointType
fromJoints = uncurry JT . both UT.fromList . unzip . M.keys

fromLists :: [Sym] -> [Sym] -> JointType
fromLists syms0 syms1 = JT (UT.fromList syms0) (UT.fromList syms1)

toLists :: JointType -> ([Sym],[Sym])
toLists (JT u0 u1) = (UT.toAscList u0, UT.toAscList u1)

-------------
-- METHODS --
-------------

-- | Dimensionality
dims :: JointType -> (Int, Int)
dims (JT u0 u1) = (UT.size u0, UT.size u1)

-- | Is the symbol a member of the union?
member :: JointType -> Sym -> Sym -> Bool
member (JT u0 u1) s0 s1 = UT.member s0 u0 && UT.member s1 u1

-- | Return a Map of all joints that could be member of the given type,
-- regardless of their presence in the string. O(n^2)
extension :: JointType -> Joints ()
extension = M.fromDistinctAscList . fmap (,()) . extension_

-- | Return a list of all joints that could be member of the given type,
-- regardless of their presence in the string. List is in ascending
-- order. O(n^2)
extension_ :: JointType -> [(Sym,Sym)]
extension_ (JT u0 u1) = [ (s0,s1) | s0 <- s0s, s1 <- s1s ]
  where
    s0s = UT.toAscList u0
    s1s = UT.toAscList u1

------------
-- MODIFY --
------------

-- | Safe left insertion
insertLeft :: Sym -> JointType -> JointType
insertLeft s (JT u0 u1) = JT (UT.insert s u0) u1

-- | Unsafe left insertion. Breaks invariant if the symbol is already
-- present in left union.
insertLeftMissing :: Sym -> JointType -> JointType
insertLeftMissing s (JT u0 u1) = JT (UT.insertMissing s u0) u1

-- | Safe right insertion
insertRight :: Sym -> JointType -> JointType
insertRight s (JT u0 u1) = JT u0 (UT.insert s u1)

-- | Unsafe right insertion. Breaks invariant if the symbol is already
-- present in right union.
insertRightMissing :: Sym -> JointType -> JointType
insertRightMissing s (JT u0 u1) = JT u0 (UT.insertMissing s u1)

-- | Safe both left/right insertion at once
insertBoth :: Sym -> Sym -> JointType -> JointType
insertBoth s0 s1 (JT u0 u1) = JT (UT.insert s0 u0) (UT.insert s1 u1)

-- | Unsafe both left/right insertion at once. Breaks invariant if the
-- symbols are already present in the respective unions.
insertBothMissing :: Sym -> Sym -> JointType -> JointType
insertBothMissing s0 s1 (JT u0 u1) = JT u0' u1'
  where u0' = UT.insertMissing s0 u0
        u1' = UT.insertMissing s1 u1

deleteLeft :: Sym -> JointType -> JointType
deleteLeft s = left %~ UT.delete s

-- | Unsafe left deletion. Breaks invariant if the symbol is not already
-- present in left union.
deleteLeftMember :: Sym -> JointType -> JointType
deleteLeftMember s = left %~ UT.deleteMember s

deleteRight :: Sym -> JointType -> JointType
deleteRight s = right %~ UT.delete s

-- | Unsafe right deletion. Breaks invariant if the symbol is not already
-- present in right union.
deleteRightMember :: Sym -> JointType -> JointType
deleteRightMember s = right %~ UT.deleteMember s

deleteBoth :: Sym -> Sym -> JointType -> JointType
deleteBoth s0 s1 = deleteRight s1 . deleteLeft s0

deleteBothMember :: Sym -> Sym -> JointType -> JointType
deleteBothMember s0 s1 (JT u0 u1) = JT u0' u1'
  where u0' = UT.deleteMember s0 u0
        u1' = UT.deleteMember s1 u1

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: JointType
bot = JT UT.bot UT.bot

-- | Subtype relation (partial order)
leq :: JointType -> JointType -> Bool
leq (JT u0 u1) (JT u0' u1') =
  UT.size u1 <= UT.size u1' -- short-circuit
  && UT.leq u0 u0'
  && UT.leq u1 u1'

-- | Strict subtype relation (partial order)
lt :: JointType -> JointType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: JointType -> JointType -> JointType
join (JT u0 u1) (JT u0' u1') =
  JT (UT.join u0 u0') (UT.join u1 u1')

-- | Greatest lower bound
meet :: JointType -> JointType -> JointType
meet (JT u0 u1) (JT u0' u1') =
  JT (UT.meet u0 u0') (UT.meet u1 u1')

-- | Partial ordering
compare :: JointType -> JointType -> Maybe Ordering
compare (JT u0 u1) (JT u0' u1') = do -- Maybe
  ord0 <- UT.compare u0 u0'
  ord1 <- UT.compare u1 u1'
  case ord0 of
    LT | GT <- ord1 -> Nothing
       | otherwise  -> Just LT
    GT | LT <- ord1 -> Nothing
       | otherwise  -> Just GT
    EQ -> Just ord1

-----------
-- CODEC --
-----------

-- | Refine a type with a bit mask
refine :: JointType -> BitVec -> JointType
refine (JT u0 u1) bv
  | len /= n0 + n1 = err $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = JT (UT.refine u0 bv0) (UT.refine u1 bv1)
  where
    n0 = UT.size u0
    n1 = UT.size u1
    len = BV.length bv
    (bv0,bv1) = BV.splitAt n0 bv

-- | For a joint count, a type and a code, instantiate the type into
-- specific joints. Returns Nothing if the given code is not long enough
-- to fully specify a resolution. If code is long enough, returns the
-- rest of the code with the resolution's code removed.
resolve :: Int -> JointType -> BitVec -> Maybe ([(Sym,Sym)], BitVec)
resolve k t bv = first (resolveIndexes t) <$> Variety.decode (bases k t) bv

-- | For encoding/decoding, [n0,n1,n0,n1,n0,n1,...]
bases :: Int -> JointType -> [Integer]
bases k (JT u0 u1) = concat $
  L.transpose [UT.bases k u0, UT.bases k u1]

-- | For decoding
resolveIndexes :: JointType -> [Integer] -> [(Sym,Sym)]
resolveIndexes (JT u0 u1) is = zip s0s s1s
  where
    s0s = UT.fromIdxs u0 is0
    s1s = UT.fromIdxs u1 is1
    (is0,is1) = case L.transpose (L.chunksOf 2 is) of
                  [l0,l1] -> (l0,l1)
                  _else -> err "resolveIndexes: impossible"

err :: String -> a
err = error . ("JointType." ++)

-----------------
-- INFORMATION --
-----------------

-- | Codelen (bits) of a refinement
refineLen :: JointType -> Int
refineLen (JT u0 u1) = UT.refineLen u0 + UT.refineLen u1

-- | O(1) Number of different joints covered by the type
variety :: JointType -> Int
variety (JT u0 u1) = UT.size u0 * UT.size u1

-- | k log(n0*n1)
resolutionInfo :: Int -> JointType -> Double
resolutionInfo k jt = fromIntegral k * log2 base
  where base = fromIntegral $ variety jt

-- | Length of a code to instantiate a type into `k` constructions
resolutionLen :: Int -> JointType -> Int
resolutionLen = ceiling .: resolutionInfo

---------------------
-- IO STATS/CHECKS --
---------------------

printInfo :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
             (JointType, Map (Sym,Sym) b) -> m ()
printInfo (jt,jts) (rjt,rjts) = liftIO $ putStrLn $
  "Generated type with size "
  ++ show (dims rjt)
  ++ " from "  ++ show (dims jt)
  ++ " covering " ++ show (Jts.size rjts)
  ++ " joints out of " ++ show (Jts.size jts)
  ++ " ("  ++ show
  (round @_ @Int $ 100.0 * fromIntegral (Jts.size rjts)
    / fromIntegral @_ @Double (Jts.size jts))
  ++ "%)"

printLUB :: MonadIO m => JointType -> Map (Sym,Sym) a -> m ()
printLUB jt jts = liftIO $ do
  putStr "type is "
  if jt == fromJoints jts
    then putStrLn $ inGreen "LUB" ++ " of its joints"
    else do putStrLn $ inRed "not LUB" ++ " of its joints"
            putStrLn $ "rtjt: " ++ show (jt, void jts)
            error "LUB error"

printSubtyping :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                  (JointType, Map (Sym,Sym) b) -> m ()
printSubtyping (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "type is "
  if rjt `leq` jt
    then putStrLn $ inGreen "subtype" ++ " of its parent"
    else do putStrLn $ inRed "not subtype" ++ " of its parent"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "subtype error"

printConservation :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                     (JointType, Map (Sym,Sym) a) -> m ()
printConservation (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "split " -- TODO: check disjointness too?
  if void jts == (void rjts `M.union` void jts')
    then putStrLn $ inGreen "preserves" ++ " all joints"
    else do putStrLn $ inRed "does not preserve" ++ " all joints"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "joints split error"

printMembership :: MonadIO m => Map (Sym,Sym) a -> (JointType, Map (Sym,Sym) a) -> m ()
printMembership jts (rjt,rjts) = liftIO $ do
  let rjtsVerif = M.filterWithKey (const . uncurry (member rjt)) jts
  putStr "returned joints "
  if M.keys rjts == M.keys rjtsVerif
    then putStrLn $ inGreen "match" ++ " joints under the type"
    else do putStrLn $ inRed "don't match" ++ " joints under the type"
            putStrLn $ "rtjt: " ++ show (M.keys rjts)
              ++ "\nrjts: " ++ show (M.keys rjts)
              ++ "\nrjtsVerif: " ++ show (M.keys rjtsVerif)
            error "joints coverage error"

inRed :: String -> String
inRed s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

inGreen :: String -> String
inGreen s = "\ESC[32m" ++ s ++ "\ESC[0m"
