{-# LANGUAGE DeriveDataTypeable #-}
import DnaToRna --(toDna, execute1)
import Prelude hiding (mapM_)
import Data.Foldable (mapM_, toList)
import Data.List (intercalate)
import Data.Sequence ((><))
import System.Environment (getArgs)
import System.IO
--import System.Console.CmdArgs
import System.Console.GetOpt  --(ArgOrder(..), getOpt)

--data Option = Option { label :: String,
--                       size :: Int }
--  deriving (Show, Data, Typeable)

--option = Option { label = "no label",
--                  size = 0 }

main = do
  --opt <- cmdArgs option
  --print opt

  let options = [
                 Option ['h'] ["help"] (NoArg 'h') "Show help.",
                 Option ['d'] ["dump"] (NoArg 'd') "Dump prefix dna."
                ]
  args <- getArgs
  let (opts, left, err) = getOpt Permute options args
  if not (null err)
    then print err
    else do
      let prefix = if left == [] then ""
                                 else left !! 0
      if 'd' `elem` opts then dumpPrefix prefix
                         else convert prefix

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
