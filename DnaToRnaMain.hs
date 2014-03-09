{-# LANGUAGE DeriveDataTypeable #-}
import DnaToRna (execute1, toDna, pattern2, search, template2)
import Prelude hiding (drop, mapM_)
import Data.Foldable (mapM_, toList)
import Data.List (intercalate)
import Data.Sequence (drop, (><))
import System.Environment (getArgs)
import System.IO (IOMode(..), hClose, hPutStrLn, openFile)
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
  let n = 10000000000
  fhRna <- openFile "rna" WriteMode
  loop fhRna 0 n dna
  hClose fhRna

loop fhRna i n dna | i >= n = putStrLn (show n ++ " loop over")
                   | otherwise = do
  case execute1 dna of
    Nothing -> putStrLn ("Dna is fully converted to Rna at #" ++ show i)
    Just (rna, dna') -> do
      writeRna fhRna rna
      loop fhRna (i + 1) n dna'

writeRna fhRna rna = do
  mapM_ (hPutStrLn fhRna . toList) rna


dumpPrefix prefix = do
  let dna = toDna prefix
  let (pss, rna, dna') = pattern2 dna
  let (tss, rna2, dna'') = template2 dna'

  putStrLn ("Pattern: " ++ concatMap (show . fst) (toList pss))
  mapM_ (\(pitem, dnas) -> putStrLn (show pitem ++ "\t" ++ (intercalate " " $ map toList $ toList dnas))) $ toList pss
  putStrLn ("\nTemplate: " ++ concatMap (show . fst) (toList tss))
  mapM_ (\(titem, dnas) -> putStrLn (show titem ++ "\t" ++ (intercalate " " $ map toList $ toList dnas))) $ toList tss
  putStrLn ("\nLeft: [" ++ toList dna'' ++ "]")

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
