module Tree (Tree, root, rewrites, readTrees) where

import Text.Parsec
import Text.Parsec.String

import Grammar (Rule, brule, lrule)

data Tree a b = BNode a [Tree a b] | LNode a b deriving (Show,Eq,Ord)

root :: Tree a b -> a
root (LNode x _) = x
root (BNode x _) = x

rewrites :: Tree a b -> [Rule a b]
rewrites (LNode x y)  = [lrule x y]
rewrites (BNode x ts) = (brule x (map root ts)) : concatMap rewrites ts

---------------------------------------------------

readTrees :: FilePath -> IO [Tree String String]
readTrees file =
    do
        e <- parseFromFile (many line) file
        case e of
            Left err -> fail ("PARSE ERROR: " ++ show err)
            Right pairs -> return (concat [replicate n t | (n,t) <- pairs])

line :: Parsec String () (Int, Tree String String)
line =
    do _ <- many space
       n <- option 1 (fmap read (between (many space) (many1 space) (many1 digit)))
       t <- sexp
       _ <- many space
       return (n, t)

sexp :: Parsec String () (Tree String String)
sexp = try leaf <|> nonleaf

leaf :: Parsec String () (Tree String String)
leaf =
    do _  <- char '('
       nt <- many1 alphaNum
       _  <- many1 space
       w  <- many1 alphaNum
       _  <- many space
       _  <- char ')'
       return (LNode nt w)

nonleaf :: Parsec String () (Tree String String)
nonleaf =
    do _        <- char '('
       nt       <- many1 alphaNum
       _        <- many1 space
       children <- sexp `sepEndBy1` (many1 space)
       _        <- char ')'
       return (BNode nt children)

