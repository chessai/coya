{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

-- | Consider some log-semiring R. Then, for any two x,y :: R, the following holds:
--
--   @x ^ log y == y ^ log x == e ^ (log x * log y)@
--
--   Coya is a commutative monoid (R, #), where x # y = x ^ log y.
--
--   The following laws hold:
-- 
-- [__Left Identity__]: @e # x == x@
-- [__Right Identity__]: @x # e == x@
-- [__Associativity__]: @(x \# y) \# z == x \# (y \# z)@
-- [__Commutativity__]: @x \# y == y \# x@
--
--   If R is a poset where all elements in R are greater than one,
--   then R also forms a group:
--
-- @x # (exp (1 / log x)) = x@
module Coya
  ( Coya(..)
  , CoyaGroup(..)
  , coyaGroup
  ) where

import Data.Coerce (coerce)
import Data.Group
import Data.Primitive.Types (Prim)
import Data.Semiring (Semiring(..),Ring(..))
import Foreign.Storable (Storable)
import Prelude hiding (Num(..))
import Refined
import Refined.Unsafe (reallyUnsafeRefine)
import qualified GHC.Num as GHCNum

-- | The 'Coya' monoid. Its 'semigroup' instance
--   is a binary operation that distributes over multiplication, i.e:
--
--   @'Coya' x '<>' ('Coya' y '*' 'Coya' z) '==' ('Coya' x <> 'Coya' y) '*' ('Coya' x <> 'Coya' z)@
--
--   The 'Semiring' and 'GHCNum.Num' instances simply lift the underlying type's.
newtype Coya a = Coya { getCoya :: a }
  deriving newtype (Eq,Ord,Semiring,Ring)
  deriving newtype (GHCNum.Num,Floating,Fractional,Real,RealFloat,RealFrac)
  deriving newtype (Storable,Prim)
  deriving stock (Show,Read)

-- | @'Coya' x '<>' 'Coya' y '==' Coya (x '**' 'log' y)@
instance Floating a => Semigroup (Coya a) where
  Coya x <> Coya y = Coya (x ** log y)
  {-# inline (<>) #-}
  {-# specialise (<>) :: Coya Float -> Coya Float -> Coya Float #-}
  {-# specialise (<>) :: Coya Double -> Coya Double -> Coya Double #-}

-- | @'mempty' '==' e@
instance Floating a => Monoid (Coya a) where
  mempty = Coya (exp 1)
  {-# inline mempty #-}
  {-# specialise mempty :: Coya Float #-}
  {-# specialise mempty :: Coya Double #-}

-- | The 'Coya' monoid constrained to numbers which are greater than
--   1. This ensures that the group property of inversion holds:
--
--   @x '<>' ('exp' (1 '/' 'log' x)) '==' x@
newtype CoyaGroup a = CoyaGroup { getCoyaGroup :: Refined (From 1) (Coya a) }

-- | A smart constructor for 'CoyaGroup'.
coyaGroup :: forall a. (Ord a, GHCNum.Num a) => a -> Maybe (CoyaGroup a)
coyaGroup a = do
  r <- refineThrow (Coya a)
  pure (coerce r)

-- | Equivalent to the 'Semigroup' instance for 'Coya'.
instance (Floating a, Ord a) => Semigroup (CoyaGroup a) where
  CoyaGroup r <> CoyaGroup r' = CoyaGroup (reallyUnsafeRefine (unrefine r <> unrefine r')) 

-- | Equivalent to the 'Monoid' instance for 'Coya'.
instance (Floating a, Ord a) => Monoid (CoyaGroup a) where
  mempty = CoyaGroup (reallyUnsafeRefine mempty)

-- | @x <> ('exp' (1 '/' 'log' x)) '==' x@
instance (Floating a, Ord a) => Group (CoyaGroup a) where
  invert (CoyaGroup x) = CoyaGroup (reallyUnsafeRefine (Coya (exp (1 / log (getCoya (unrefine x))))))
