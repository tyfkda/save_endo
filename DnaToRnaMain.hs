import DnaToRna (toDna, execute, execute1)
import Prelude hiding (mapM_)
import Data.Foldable (mapM_, toList)
import Data.Sequence ((><))
import System.Environment (getArgs)
import System.IO

main = do
  cs <- getContents
  args <- getArgs
  let dna = if args == []
              then toDna cs
              else toDna (args !! 0) >< toDna cs
  --let rna = execute dna  
  --mapM_ (putStrLn . toList) rna
  let n = if length args >= 2
              then read (args !! 1)
              else 10000000000
  test n dna

test n dna = do
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
