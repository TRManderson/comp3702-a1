module Main where
import Options.Applicative
import Solver
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Monad
import Data.List (intercalate)

parsequery :: (Graph gr) => String -> Maybe (Solver gr Float)
parsequery s = Nothing

dir_in_file_completer :: String -> IO String
dir_in_file_completer start = undefined

mapfile_opt :: Parser String
mapfile_opt = strArgument (metavar "mapfile")

queryfile_opt :: Parser String
queryfile_opt = strArgument (metavar "queryfile")

outfile_opt :: Parser String
outfile_opt = strArgument (metavar "outfile")

debug_opt :: Parser Bool
debug_opt = flag False True (
    short 'd'
  <> long "debug")

program_info :: InfoMod a
program_info = progDesc "COMP3702 Assignment 1"

parser :: Parser ProgramSettings
parser = ProgramSettings <$> mapfile_opt <*> queryfile_opt <*> outfile_opt <*> debug_opt

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
  writeFile (_outFileName settings) . unlines . map (intercalate "-") . map (map show . reverse . (flip query $ graph)) $ queries
  
