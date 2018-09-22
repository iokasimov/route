module Data.Route.Usage.FHS (Path (..), current) where

-- | Filesystem Hierarchy Standard

data Path = Root
	| Full (Route Absolute String)
	| Partial (Route Relative String)

current :: IO Path
current = getCurrentDirectory >>= \case
	"/" -> pure Root
	d -> pure . maybe Root (Full . Route)
		. foldaway . splitOn "/" . tail $ d
