{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import WhileLang.Interpreter
import WhileLang.Parser hiding (Parser)
import Options.Applicative hiding (value, command)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import System.Exit (exitFailure)
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (pack)

data Options = Options
  { inputFile :: FilePath
  , initialVars :: Maybe String
  }

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Run a While program with an optional initial variable mapping"
 <> header "WhileLang Interpreter - A simple interpreter for While programs" )


optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "FILE"
     <> help "Path to the While program file" )
  <*> optional (strOption
      ( long "vars"
     <> short 'v'
     <> metavar "VARS"
     <> help "Initial variable mappings in the format x=10,y=20,z=30" ))

parseVarMap :: String -> VarMap
parseVarMap input =
  Map.fromList $ map parsePair (splitOn ',' input)
  where
    splitOn :: Char -> String -> [String]
    splitOn delim s = case break (== delim) s of
      (a, ',' : rest) -> a : splitOn delim rest
      (a, _)          -> [a]

    parsePair :: String -> (String, Int)
    parsePair pair = case break (== '=') pair of
      (key, '=' : value) -> (key, read value)
      _ -> error $ "Invalid variable mapping: " ++ pair

main :: IO ()
main = do
  Options{..} <- execParser opts
  programContent <- readFile inputFile

  -- Parse initial variable mappings
  initialVarMap <- case initialVars of
    Just vars -> return $ parseVarMap vars
    Nothing   -> return Map.empty

  -- Parse and run the While program
  case parse (sc *> pCommand) "" (pack programContent) of
    Left err -> do
      putStrLn $ "Error parsing While program: " ++ errorBundlePretty err
      exitFailure
    Right command -> do
      result <- runStateT (runExceptT (interpCommand command)) initialVarMap
      case result of
        (Left whileErr, _) -> do
          putStrLn $ "Runtime error: " ++ show whileErr
          exitFailure
        (Right (), finalState) -> do
          putStrLn "Program executed successfully. Final variable state:"
          print finalState


