

data Node = Leaf Char Int
          | Interior Node Node [Char] Int
            deriving (Show)

createTree :: [Node] -> Node
createTree [n] = n
createTree (first:second:rest) = 
    let n = Interior 
                first
                second
                ((getNodeStr first) ++ (getNodeStr second))
                ((getNodeWeight first) + (getNodeWeight second))
            in
        createTree (insert n rest)

getNodeStr :: Node -> [Char]
getNodeStr (Leaf s _) = [s]
getNodeStr (Interior _ _ syms _) = syms

getNodeWeight :: Node -> Int
getNodeWeight (Leaf _ w) = w
getNodeWeight (Interior _ _ _ w) = w

insert :: Node -> [Node] -> [Node]
insert n [] = [n]
insert n all@(first:rest)
    | getNodeWeight n <= getNodeWeight first = n : all
    | otherwise = first : (insert n rest)

decode :: Node -> Node -> [Bool] -> [Char]
decode codeTree (Leaf s w) bits = 
    s : decode codeTree codeTree bits
decode codeTree (Interior l _ _ _) (False:bits) =
    decode codeTree l bits
decode codeTree (Interior _ r _ _) (True:bits) =
    decode codeTree r bits
decode _ _ [] = []
        
encode :: Node -> Node -> [Char] -> [Bool]
encode _ _ [] = []
encode codeTree (Leaf s w) (sym:symbols) =
    encode codeTree codeTree symbols
encode codeTree (Interior l r _ _) (sym : symbols) 
    | elem sym (getNodeStr l) = False : encode codeTree l (sym:symbols)
    | otherwise               = True  : encode codeTree r (sym:symbols)

