module DnaToRna where
import Prelude hiding (take, drop, length, null, foldl)
import Data.Foldable (foldl, toList)
import Data.Sequence (Seq, ViewL(..), breakl, drop, empty, fromList, index, length, null, singleton, tails, take, viewl, (|>), (<|), (><))

type Base = Char
type Dna = Seq Base
type Rna = Seq Dna

type Pattern = Seq PItem
data PItem = PBase Base | PSkip Int | PSearch Dna | PBegin | PEnd
  deriving (Eq)

type Template = Seq TItem
data TItem = TBase Base | TRefer Int Int | TEncode Int
  deriving (Eq)

instance Show PItem where
  show (PBase b) = [b]
  show (PSkip n) = "!" ++ show n
  show (PSearch dna) = "?\"" ++ toList dna ++ "\""
  show (PBegin) = "("
  show (PEnd) = ")"

instance Show TItem where
  show (TBase b) = [b]
  show (TRefer n l) = "{" ++ show n ++ "_" ++ show l ++ "}"
  show (TEncode n) = "|" ++ show n ++ "|"

toDna :: [Base] -> Dna
toDna = fromList

execute1 :: Dna -> Maybe (Rna, Dna)
execute1 dna | null dna = Nothing
             | otherwise = Just $ step dna

step :: Dna -> (Rna, Dna)
step dna = let (p, rna1, dna') = pattern empty 0 empty dna
               (t, rna2, dna'') = template empty empty dna'
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
        replace empty t e (drop i dna)

--replace :: Template -> Seq Dna -> Dna -> Dna
replace r tpl e dna =
    case viewl tpl of
      (TBase b :< tpl') -> replace (r |> b) tpl' e dna
      (TRefer n l :< tpl') ->
        let ref = if n < length e then index e n
                                  else empty
        in replace (r >< protect l ref) tpl' e dna
      (TEncode n :< tpl') ->
        let len = if n < length e then length $ index e n
                                  else 0
        in replace (r >< asnat len) tpl' e dna
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

pattern :: Seq PItem -> Int -> Rna -> Dna -> (Pattern, Rna, Dna)
pattern p lvl rna dna
  | d1 == 'C' = pattern (p |> PBase 'I') lvl rna (drop 1 dna)
  | d1 == 'F' = pattern (p |> PBase 'C') lvl rna (drop 1 dna)
  | d1 == 'P' = pattern (p |> PBase 'F') lvl rna (drop 1 dna)
  | d2 == dnaIC = pattern (p |> PBase 'P') lvl rna (drop 2 dna)
  | d2 == dnaIP =
      let (n, dna') = nat (drop 2 dna)
      in pattern (p |> PSkip n) lvl rna dna'
  | d2 == dnaIF =
      let (s, dna') = consts (drop 3 dna)
      in pattern (p |> PSearch s) lvl rna dna'
  | d3 == dnaIIP =
      pattern (p |> PBegin) (lvl + 1) rna (drop 3 dna)
  | d3 == dnaIIC || d3 == dnaIIF =
      if lvl == 0
        then (p, rna, (drop 3 dna))
        else pattern (p |> PEnd) (lvl - 1) rna (drop 3 dna)
  | d3 == dnaIII =
      pattern p lvl (rna |> subseq 3 10 dna) (drop 10 dna)
  | otherwise = (p, rna, empty)
  where d1 = index dna 0
        d2 = take 2 dna
        d3 = take 3 dna

template :: Seq TItem -> Rna -> Dna -> (Template, Rna, Dna)
template t rna dna
  | d1 == 'C' = template (t |> TBase 'I') rna (drop 1 dna)
  | d1 == 'F' = template (t |> TBase 'C') rna (drop 1 dna)
  | d1 == 'P' = template (t |> TBase 'F') rna (drop 1 dna)
  | d2 == dnaIC = template (t |> TBase 'P') rna (drop 2 dna)
  | d2 == dnaIF || d2 == dnaIP =
      let (l, dna') = nat (drop 2 dna)
          (n, dna'') = nat dna'
      in template (t |> TRefer n l) rna dna''
  | d3 == dnaIIC || d3 == dnaIIF = (t, rna, drop 3 dna)
  | d3 == dnaIIP =
      let (n, dna') = nat (drop 3 dna)
      in template (t |> TEncode n) rna dna'
  | d3 == dnaIII =
      template t (rna |> subseq 3 10 dna) (drop 10 dna)
  | otherwise = (t, rna, empty)
  where d1 = index dna 0
        d2 = take 2 dna
        d3 = take 3 dna

nat :: Dna -> (Int, Dna)
nat dna
  | d1 == 'P' = (0, drop 1 dna)
  | d1 == 'I' || d1 == 'F' =
      let (n, dna') = nat (drop 1 dna)
      in (2 * n, dna')
  | d1 == 'C' =
      let (n, dna') = nat (drop 1 dna)
      in (2 * n + 1, dna')
  where d1 = index dna 0

consts :: Dna -> (Dna, Dna)
consts dna
  | d1 == 'C' =
      let (s, dna') = consts (drop 1 dna)
      in ('I' <| s, dna')
  | d1 == 'F' =
      let (s, dna') = consts (drop 1 dna)
      in ('C' <| s, dna')
  | d1 == 'P' =
      let (s, dna') = consts (drop 1 dna)
      in ('F' <| s, dna')
  | d2 == dnaIC =
      let (s, dna') = consts (drop 2 dna)
      in ('P' <| s, dna')
  | otherwise = (empty, dna)
  where d1 = index dna 0
        d2 = take 2 dna

startsWith :: Dna -> Dna -> Bool
startsWith seq prefix = take (length prefix) seq == prefix

subseq :: Int -> Int -> Seq a -> Seq a
subseq i e seq = take (e - i) $ drop i seq

dnaIC = toDna "IC"
dnaIP = toDna "IP"
dnaIF = toDna "IF"
dnaIIP = toDna "IIP"
dnaIIC = toDna "IIC"
dnaIIF = toDna "IIF"
dnaIII = toDna "III"
