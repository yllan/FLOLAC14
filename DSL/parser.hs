data Grammar = Empty | Unit | Single Char | Conc Grammar Grammar | Union Grammar Grammar deriving Show

alphabets = foldr (Union) Empty $ map (Single) ['A'..'Z']

numericals = foldr (Union) Empty $ map (Single) ['0'..'9']

alphanumericals = Union alphabets numericals

times :: Grammar -> Int -> Grammar
times g 0 = Unit
times g n = Conc g (times g (n - 1))

plate :: Grammar
plate = Conc (times alphabets 3) (Conc (Single 'A') (times numericals 4))