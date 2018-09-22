module Data.Route.Usage.FHS (Filepath (..), current) where

-- | Filesystem Hierarchy Standard
import "apart" Data.Apart.Structures.Stack (foldaway)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.List (reverse, tail)
import "base" Data.Maybe (maybe)
import "base" Data.String (String)
import "base" Text.Show (Show (show))
import "base" System.IO (IO)
import "directory" System.Directory (getCurrentDirectory)
import "split" Data.List.Split (splitOn)

import Data.Route (Route (Route), Reference (Absolute, Relative))

data Filepath = Root
	| Full (Route Absolute String)
	| Partial (Route Relative String)
	deriving Show

-- | Get current working directory
current :: IO Filepath
current = parse <$> getCurrentDirectory where

	parse :: String -> Filepath
	parse "/" = Root
	parse directory = maybe Root (Full . Route) . foldaway
		. reverse . splitOn "/" . tail $ directory
