{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor,DeriveDataTypeable #-}

module Faceted.Internal(
  Label,
  Faceted(Raw,Faceted),
  PC,
  Branch(Private,Public),
  View,
  FIO(FIO),
  pcF,
  project,
  visibleTo,
  secureRunFIO,
  branch
  ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import System.IO
import Data.Dynamic

-- | A security label is any string.
-- Labels need not be secrets; they
-- may be readable strings. Information flow security is ensured by a
-- combination of the type system and dynamic checks.
type Label = String

-- | A _view_ is any set of labels. 
-- In enforcing information flow security Each view may see a different value.
-- In this sense a particular user may be associated with a number of labels, thereby determining
-- the set of secured data they can and cannot see (what their view of the data is).
type View = [Label]

-- | Type 'Faceted a' represents (possibly) faceted values.
--
-- <k ? x : y>   ====>  Faceted k x y 
 
data Faceted a =
    Raw a
  | Faceted Label (Faceted a) (Faceted a)
  deriving (Show, Eq, Typeable)

-- | Functor: For when the function is pure but the argument has facets.
instance Functor Faceted where
  fmap f (Raw v             ) = Raw (f v)
  fmap f (Faceted k priv pub) = Faceted k (fmap f priv) (fmap f pub)

-- | Applicative: For when the function and argument both have facets.
instance Applicative Faceted where
  pure  = Raw
  (Raw f             ) <*> x  = fmap f x
  (Faceted k priv pub) <*> x  = Faceted k (priv <*> x) (pub <*> x)

-- | Monad: Like applicative, but even more powerful. 'Faceted' the free monad
-- over the function 'Facets a = F Label a a'. 
instance Monad Faceted where
  return = Raw
  (Raw x             ) >>= f  = f x
  (Faceted k priv pub) >>= f  = Faceted k (priv >>= f) (pub >>= f)

-- | A Branch is a principal or its negatives, and a pc is a set of branches.

data Branch = Public Label | Private Label deriving (Eq, Show)
type PC = [Branch]

-- | << pc ? x : y >>  =====>   pcF pc x y

pcF :: PC -> Faceted a -> Faceted a -> Faceted a
pcF []                     x _ = x
pcF (Private k : branches) x y = Faceted k (pcF branches x y) y
pcF (Public k  : branches) x y = Faceted k y (pcF branches x y)

-- Private
project :: View -> Faceted a -> a
project view (Raw v) = v
project view (Faceted k priv pub)
  | k `elem`    view = project view priv
  | k `notElem` view = project view pub

-- Private
visibleTo :: PC -> View -> Bool
visibleTo pc view = all consistent pc
  where consistent (Private k)  = k `elem` view
        consistent (Public k) = k `notElem` view

-- | Faceted IO
data FIO a = FIO { runFIO :: PC -> IO a }

-- | Monad is straightforward
instance Monad FIO where
  return x = FIO (\pc -> return x)
  x >>= f  = FIO (\pc -> do v <- runFIO x pc
                            runFIO (f v) pc)


-- | With an empty context it is safe to run
secureRunFIO :: FIO a -> IO a
secureRunFIO fio = runFIO fio []

-- | Commuting pure and effectful facets
-- Optimized to prune inconsistent views
branch :: Faceted (FIO a) -> FIO (Faceted a)
branch (Raw fio)   = FIO (\pc -> do runFIO fio pc >>= (return . Raw))
branch (Faceted k priv pub) = FIO branchForPC
  where branchForPC pc
          | Private k `elem` pc = runFIO (branch priv) pc
          | Public k  `elem` pc = runFIO (branch pub) pc
          | otherwise           = do privV <- runFIO (branch priv) (Private k : pc)
                                     pubV  <- runFIO (branch pub)  (Public k : pc)
                                     return (Faceted k privV pubV)