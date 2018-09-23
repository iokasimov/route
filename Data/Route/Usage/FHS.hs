module Data.Route.Usage.FHS (Filepath (..), current, stringify) where

-- | Filesystem Hierarchy Standard
import "apart" Data.Apart.Structures.Stack (foldaway)
import "base" Control.Monad ((>>=))
import "base" Data.Foldable (foldr)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>), ($>))
import "base" Data.List (init, intercalate, reverse, tail)
import "base" Data.Maybe (maybe)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Text.Show (Show (show))
import "base" System.IO (IO)
import "directory" System.Directory (getCurrentDirectory, setCurrentDirectory)
import "split" Data.List.Split (splitOn)

import Data.Route (Route (Route), Reference (Absolute, Relative))

data Filepath = Root
	| Full (Route Absolute String)
	| Partial (Route Relative String)

-- | Get current working directory
current :: IO Filepath
current = parse <$> getCurrentDirectory where

	parse :: String -> Filepath
	parse "/" = Root
	parse directory = maybe Root (Full . Route) . foldaway
		. reverse . splitOn "/" . tail $ directory

come :: Filepath -> IO Filepath
come Root = setCurrentDirectory "/" $> Root
come route@(Full _) = setCurrentDirectory (stringify route) $> route
come route@(Partial _) = (getCurrentDirectory >>= setCurrentDirectory . target) $> route where

	target :: String -> String
	target directory = directory <> "/" <> stringify route

stringify :: Filepath -> String
stringify Root = "/"
stringify (Full route) = (:) '/' . intercalate "/" . foldr (:) [] $ route
stringify (Partial route) = intercalate "/" . foldr (:) [] $ route
