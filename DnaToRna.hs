module DnaToRna where
import Prelude hiding (take, drop, length, null, foldl)
import Data.Foldable (foldl, toList)
import Data.Sequence

type Base = Char
type Dna = Seq Base
type Rna = Seq Dna

type Pattern = Seq PItem
data PItem = PBase Base | PSkip Int | PSearch Dna | PBegin | PEnd
  deriving (Eq, Show)

type Template = Seq TItem
data TItem = TBase Base | TRefer Int Int | TEncode Int
  deriving (Eq, Show)

toDna :: [Base] -> Dna
toDna = fromList

execute1 :: Dna -> Maybe (Rna, Dna)
execute1 dna | null dna = Nothing
             | otherwise = Just $ step dna

step :: Dna -> (Rna, Dna)
step dna = let (p, rna1, dna') = pattern dna
               (t, rna2, dna'') = template dna'
           in (rna1 >< rna2, matchreplace p t dna'')

matchreplace :: Pattern -> Template -> Dna -> Dna
matchreplace pat t dna = loop 0 empty empty pat dna
  where
    loop i e c pat dna = case viewl pat of
      (PBase b :< pat') -> if index dna i == b
                             then loop (i + 1) e c pat' dna
                             else dna
      (PSkip n :< pat') -> if i + n > length dna
                             then dna
                             else loop (i + n) e c pat' dna
      (PSearch s :< pat') ->
        case search s (drop i dna) of
          Just n -> loop (i + n + length s) e c pat' dna
          Nothing -> dna
      (PBegin :< pat') -> loop i e (i <| c) pat' dna
      (PEnd :< pat') -> loop i (e |> subseq (index c 0) i dna) (drop 1 c) pat' dna
      EmptyL ->
        replace t e (drop i dna)

replace :: Template -> Seq Dna -> Dna -> Dna
replace tpl e dna = loop empty tpl
  where
    loop r tpl = case viewl tpl of
      (TBase b :< tpl') -> loop (r |> b) tpl'
      (TRefer n l :< tpl') -> loop (r >< protect l (index e n)) tpl'
      (TEncode n :< tpl') -> loop (r >< asnat (length (index e n))) tpl'
      EmptyL -> r >< dna

protect :: Int -> Dna -> Dna
protect 0 d = d
protect l d = protect (l - 1) (quote d)

quote :: Dna -> Dna
quote d = case viewl d of
  ('I' :< d') -> 'C' <| quote d'
  ('C' :< d') -> 'F' <| quote d'
  ('F' :< d') -> 'P' <| quote d'
  ('P' :< d') -> (toDna "IC") >< quote d'
  EmptyL -> empty

asnat :: Int -> Dna
asnat 0 = singleton 'P'
asnat n | n < 0  = undefined
        | even n = 'I' <| asnat (n `div` 2)
        | odd  n = 'C' <| asnat (n `div` 2)

search :: Dna -> Dna -> Maybe Int
search sub dna =
  let (skip, left) = breakl (`startsWith` sub) $ tails dna
  in if null left
       then Nothing
       else Just $ length skip

pattern :: Dna -> (Pattern, Rna, Dna)
pattern dna = loop empty 0 empty dna
  where
    loop p lvl rna dna
      | dna `startsWith` (toDna "C") = loop (p |> PBase 'I') lvl rna (drop 1 dna)
      | dna `startsWith` (toDna "F") = loop (p |> PBase 'C') lvl rna (drop 1 dna)
      | dna `startsWith` (toDna "P") = loop (p |> PBase 'F') lvl rna (drop 1 dna)
      | dna `startsWith` (toDna "IC") = loop (p |> PBase 'P') lvl rna (drop 2 dna)
      | dna `startsWith` (toDna "IP") =
          let (n, dna') = nat (drop 2 dna)
          in loop (p |> PSkip n) lvl rna dna'
      | dna `startsWith` (toDna "IF") =
          let (s, dna') = consts (drop 3 dna)
          in loop (p |> PSearch s) lvl rna dna'
      | dna `startsWith` (toDna "IIP") =
          loop (p |> PBegin) (lvl + 1) rna (drop 3 dna)
      | (dna `startsWith` (toDna "IIC") ||
         dna `startsWith` (toDna "IIF")) =
          if lvl == 0
            then (p, rna, (drop 3 dna))
            else loop (p |> PEnd) (lvl - 1) rna (drop 3 dna)
      | dna `startsWith` (toDna "III") =
          loop p lvl (rna |> subseq 3 10 dna) (drop 10 dna)
      | otherwise = (p, rna, empty)

template :: Dna -> (Template, Rna, Dna)
template dna = loop empty empty dna
  where
    loop t rna dna
      | dna `startsWith` (toDna "C") = loop (t |> TBase 'I') rna (drop 1 dna)
      | dna `startsWith` (toDna "F") = loop (t |> TBase 'C') rna (drop 1 dna)
      | dna `startsWith` (toDna "P") = loop (t |> TBase 'F') rna (drop 1 dna)
      | dna `startsWith` (toDna "IC") = loop (t |> TBase 'P') rna (drop 2 dna)
      | (dna `startsWith` (toDna "IF") ||
         dna `startsWith` (toDna "IP")) =
          let (l, dna') = nat (drop 2 dna)
              (n, dna'') = nat dna'
          in loop (t |> TRefer n l) rna dna''
      | (dna `startsWith` (toDna "IIC") ||
         dna `startsWith` (toDna "IIF")) = (t, rna, drop 3 dna)
      | dna `startsWith` (toDna "IIP") =
          let (n, dna') = nat (drop 3 dna)
          in loop (t |> TEncode n) rna dna'
      | dna `startsWith` (toDna "III") =
          loop t (rna |> subseq 3 10 dna) (drop 10 dna)
      | otherwise = (t, rna, empty)


nat :: Dna -> (Int, Dna)
nat dna
  | dna `startsWith` (toDna "P") = (0, drop 1 dna)
  | (dna `startsWith` (toDna "I") ||
     dna `startsWith` (toDna "F")) =
      let (n, dna') = nat (drop 1 dna)
      in (2 * n, dna')
  | dna `startsWith` (toDna "C") =
      let (n, dna') = nat (drop 1 dna)
      in (2 * n + 1, dna')

consts :: Dna -> (Dna, Dna)
consts dna
  | dna `startsWith` (toDna "C") =
      let (s, dna') = consts (drop 1 dna)
      in ('I' <| s, dna')
  | dna `startsWith` (toDna "F") =
      let (s, dna') = consts (drop 1 dna)
      in ('C' <| s, dna')
  | dna `startsWith` (toDna "P") =
      let (s, dna') = consts (drop 1 dna)
      in ('F' <| s, dna')
  | dna `startsWith` (toDna "IC") =
      let (s, dna') = consts (drop 2 dna)
      in ('P' <| s, dna')
  | otherwise = (empty, dna)

startsWith :: Dna -> Dna -> Bool
startsWith seq prefix = take (length prefix) seq == prefix

subseq :: Int -> Int -> Seq a -> Seq a
subseq i e seq = take (e - i) $ drop i seq
