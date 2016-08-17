module Main where
import Options.Applicative
import Solver
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Monad
import Data.List (intercalate)

parsequery :: (Graph gr) => String -> Maybe (Solver gr Float)
parsequery s = Nothing

mapfileOpt :: Parser String
mapfileOpt = strArgument (metavar "mapfile")

queryfileOpt :: Parser String
queryfileOpt = strArgument (metavar "queryfile")

outfileOpt :: Parser String
outfileOpt = strArgument (metavar "outfile")

debugOpt :: Parser Bool
debugOpt = flag False True (
    short 'd'
  <> long "debug")

programInfo :: InfoMod a
programInfo = progDesc "COMP3702 Assignment 1"

parser :: Parser ProgramSettings
parser = ProgramSettings <$> mapfileOpt <*> queryfileOpt <*> outfileOpt <*> debugOpt

data ProgramSettings = ProgramSettings {
    _envFileName :: String
  , _queryFileName :: String
  , _outFileName :: String
  , _debug :: Bool
  }

main :: IO ()
main = execParser (info parser program_info) >>= main'


main' :: ProgramSettings -> IO ()
main' settings = do
  graph <- (fmap graphFromStr . readFile . _envFileName $ settings) :: IO (Gr () Float)
  queries <- fmap (map queryFromStr . tail . lines) . readFile . _queryFileName $ settings
  writeFile (_outFileName settings) . unlines . map (intercalate "-" . map show . reverse . flip query graph) $ queries
  
