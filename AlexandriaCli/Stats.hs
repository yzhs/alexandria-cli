module AlexandriaCli.Stats (printStats) where

import Text.Printf (printf)

import Alexandria.Config
import Alexandria.Stats

printStats :: Configuration a => a -> IO ()
printStats conf = do
  n <- countScrolls conf
  --tags <- countTags conf
  bytes <- countBytes conf
  printf "The library contains %d scrolls with a total size of %d bytes\n" n bytes
