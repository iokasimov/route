module Data.Route.Usage.FHS (Path (..), current) where

-- | Filesystem Hierarchy Standard

data Filepath = Root
	| Full (Route Absolute String)
	| Partial (Route Relative String)
	deriving Show

-- | Get current working directory
cwd :: IO Filepath
cwd = parse <$> getCurrentDirectory where

	parse :: String -> Filepath
	parse "/" = Root
	parse directory = maybe Root (Full . Route) . foldaway
		. reverse . splitOn "/" . tail $ directory
