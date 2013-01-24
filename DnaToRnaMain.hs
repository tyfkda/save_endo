import DnaToRna (toDna, execute)
import Prelude hiding (mapM_)
import Data.Foldable (mapM_, toList)
import Data.Sequence ((><))
import System.Environment (getArgs)

main = do
  cs <- getContents
  args <- getArgs
  let dna = if args == []
              then toDna cs
              else toDna (args !! 0) >< toDna cs
  let rna = execute dna  
  mapM_ (putStrLn . toList) rna
