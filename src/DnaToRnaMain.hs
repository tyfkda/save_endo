{-# LANGUAGE DeriveDataTypeable #-}
import DnaToRna (execute1, toDna, pattern, search, template)
import Prelude hiding (drop, mapM_)
import Data.Foldable (mapM_, toList)
import Data.List (intercalate)
import Data.Sequence (drop, empty, (><))
import System.Environment (getArgs)
import System.IO (IOMode(..), hClose, hPutStrLn, openFile, stdout, stderr)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import Control.Monad (forM_)

main = do
  let options = [
                 Option ['h'] ["help"] (NoArg 'h') "Show help.",
                 Option ['d'] ["disasm"] (NoArg 'd') "Disassemble prefix dna.",
                 Option ['s'] ["search"] (NoArg 's') "Search prefix."
                ]
  args <- getArgs
  let (opts, left, err) = getOpt Permute options args
  if not (null err)
    then print err
    else do
      let prefix = if left == [] then ""
                                 else left !! 0
      if 'h' `elem` opts then printHelp
        else if 'd' `elem` opts then dumpPrefix prefix
          else if 's' `elem` opts then searchPrefix left
                                  else convert prefix

printHelp = do
  putStrLn "usage: DnaToRnaMain [OPTION...] [DNA-prefix] < dna-file"
  putStrLn "options:"
  putStrLn "  -h --help    Print usage"
  putStrLn "  -d --dump    Disassemble prefix dna"
  putStrLn "  -s --search  Search prefix dna from "

convert prefix = do
  cs <- getContents
  args <- getArgs
  let dna = toDna prefix >< toDna cs
  loop stdout dna

loop fhRna dna = do
  case execute1 dna of
    Nothing -> hPutStrLn stderr "Dna is fully converted to Rna"
    Just (rna, dna') -> do
      writeRna fhRna rna
      loop fhRna dna'

writeRna fhRna rna = do
  mapM_ (hPutStrLn fhRna . toList) rna


dumpPrefix prefix = do
  let dna = toDna prefix
  let (pss, rna, dna') = pattern empty 0 empty dna
  let (tss, rna2, dna'') = template empty empty dna'

  putStrLn ("Pattern: " ++ concatMap show (toList pss))
  putStrLn ("Template: " ++ concatMap show (toList tss))
  putStrLn ("Left: [" ++ toList dna'' ++ "]")

searchPrefix prefixs = do
  cs <- getContents
  let dna = toDna cs
  loop dna 0 prefixs
  where loop dna _ [] = return ()
        loop dna ofs (prefix: prefixs) = do
          let sub = toDna prefix
          case search sub dna of
            Nothing -> putStrLn (prefix ++ ": Cannot find prefix") >> loop dna ofs prefixs
            Just i  -> putStrLn (prefix ++ ": Found at " ++ show (i + ofs)) >> loop (drop (i + length prefix) dna) (i + length prefix) prefixs
