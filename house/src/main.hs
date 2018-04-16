module Main where

import qualified Data.Text          as T
import qualified Hue
import           Hue.Reading
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [u] -> do
      putStrLn "Will attempt to read from Philips Hue"
      putStrLn "====================================="
      reading <- Hue.getReading $ T.pack u
      case reading of
        Right r -> showReading r
        Left e ->
          putStrLn $ "Failed to get readings with error: " ++ e
    _ ->
      putStrLn $ "ERROR: too many/few arguments. Expected username, got: " ++ show args

showReading r = do
  let l = lights r
      s = motionSensors r
  putStrLn "Lights:"
  mapM_ print l
  putStrLn "\nSensors:"
  mapM_ print s
