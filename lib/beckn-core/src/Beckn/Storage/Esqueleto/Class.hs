{-# LANGUAGE FunctionalDependencies #-}

module Beckn.Storage.Esqueleto.Class where

import Beckn.Types.Id (Id)
import Database.Esqueleto.Experimental
import EulerHS.Prelude hiding (Key)

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntity t a
    | t -> a,
      a -> t
  where
  fromTEntity :: MonadThrow m => Entity t -> m a
  toTEntity :: a -> Entity t
  toTType :: a -> t

class
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  TEntityKey t a
    | t -> a,
      a -> t
  where
  fromKey :: Key t -> Id a
  toKey :: Id a -> Key t

class QEntity a b where
  toResult :: MonadThrow m => a -> m b

instance TEntity a b => QEntity (Entity a) b where
  toResult = fromTEntity

instance TEntityKey a b => QEntity (Value (Key a)) (Id b) where
  toResult = return . fromKey . unValue

instance QEntity (Value a) a where
  toResult = return . unValue

instance
  ( QEntity a1 b1,
    QEntity a2 b2
  ) =>
  QEntity (a1, a2) (b1, b2)
  where
  toResult (a1, a2) =
    (,) <$> toResult a1
      <*> toResult a2

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3
  ) =>
  QEntity (a1, a2, a3) (b1, b2, b3)
  where
  toResult (a1, a2, a3) =
    (,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4
  ) =>
  QEntity (a1, a2, a3, a4) (b1, b2, b3, b4)
  where
  toResult (a1, a2, a3, a4) =
    (,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5
  ) =>
  QEntity (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
  where
  toResult (a1, a2, a3, a4, a5) =
    (,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
  where
  toResult (a1, a2, a3, a4, a5, a6) =
    (,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6

instance
  ( QEntity a1 b1,
    QEntity a2 b2,
    QEntity a3 b3,
    QEntity a4 b4,
    QEntity a5 b5,
    QEntity a6 b6,
    QEntity a7 b7
  ) =>
  QEntity (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
  where
  toResult (a1, a2, a3, a4, a5, a6, a7) =
    (,,,,,,)
      <$> toResult a1
      <*> toResult a2
      <*> toResult a3
      <*> toResult a4
      <*> toResult a5
      <*> toResult a6
      <*> toResult a7