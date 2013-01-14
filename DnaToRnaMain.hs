import DnaToRna
import Data.Foldable
import Data.Sequence
import Prelude hiding (mapM_)
import System

main = do
  cs <- getContents
  args <- getArgs
  let dna = if args == []
              then toDna cs
              else toDna (args !! 0) >< toDna cs
  let rna = execute dna  
  mapM_ (putStrLn . toList) rna
