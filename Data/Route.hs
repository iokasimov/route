module Data.Route (Route (..), Reference (..)) where

import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)))

type Stack = Cofree Maybe

data Reference = Absolute | Relative

newtype Route (reference :: Reference) resource
	= Route { route :: Stack resource }

-- | Relative routes are defined in direct order, absolute - in reverse

instance Show resource => Show (Route Absolute resource) where
	show (Route (x :< Just xs)) = show (Route @Absolute xs) <> " / " <> show x
	show (Route (x :< Nothing)) = show x

instance Show resource => Show (Route Relative resource) where
	show (Route (x :< Just xs)) = show x <> " / " <> show (Route @Relative xs)
	show (Route (x :< Nothing)) = show x

instance Functor (Route reference) where
	fmap f (Route p) = Route $ f <$> p

instance Foldable (Route Absolute) where
	foldr f acc (Route (x :< Just xs)) = foldr f (f x acc) (Route @Absolute xs)
	foldr f acc (Route (x :< Nothing)) = f x acc

instance Foldable (Route Relative) where
	foldr f acc (Route r) = foldr f acc r
