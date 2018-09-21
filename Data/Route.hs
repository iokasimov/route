module Data.Route where

import "base" Text.Show (Show (show))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Semigroup ((<>))
import "free" Control.Comonad.Cofree (Cofree ((:<)))

type Stack = Cofree Maybe

data Reference = Absolute | Relative

newtype Route (reference :: Reference) resource
	= Route { route :: Stack resource }

instance Show resource => Show (Route Absolute resource) where
	show (Route (x :< Just xs)) = show (Route @Absolute xs) <> " / " <> show x
	show (Route (x :< Nothing)) = show x

instance Show resource => Show (Route Relative resource) where
	show (Route (x :< Just xs)) = show x <> " / " <> show (Route @Relative xs)
	show (Route (x :< Nothing)) = show x
