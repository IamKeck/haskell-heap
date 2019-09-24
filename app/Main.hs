module Main where

import Lib
import Data.Bits
import Debug.Trace

data Direction = L | R deriving Show
type Directions = [Direction]
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a) deriving Show

type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)
type HeapCheck a = a -> a -> Bool

data Heap a =  Heap {tree :: Tree a, check :: HeapCheck a, nodes :: Int}

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeft (Empty, bs) = (Empty, bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (l, RightCrumb x l:bs)
goRight (Empty, bs) = (Empty, bs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
goUp (t, []) = (t, [])

goTop :: Zipper a -> Zipper a
goTop (t, []) = (t, [])
goTop z = goTop $ goUp z

changeTop :: Directions -> Tree Char -> Tree Char
changeTop (L:ds) (Node x l r) = Node x (changeTop ds l) r
changeTop (R:ds) (Node x l r) = Node x l (changeTop ds r)
changeTop []  (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree Char -> Char
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

heapify :: Tree a -> (a -> a -> Bool) -> Tree a
heapify Empty _ = Empty
heapify t@(Node a Empty Empty) _ = t
heapify t@(Node x (Node y l r) Empty) conv =
    if conv x y then t
    else
        Node y (heapify (Node x l r) conv) Empty

heapify t@(Node x Empty (Node y l r)) conv =
    if conv x y then t
    else
        Node y Empty (Node x l r)

heapify t@(Node x lt@(Node y ll lr) rt@(Node z rl rr)) conv =
    if conv x y && conv x z then t
    -- Leftは満たすがRightが満たさない場合
    else if conv x y && not (conv x z) then
        Node z lt (heapify (Node x rl rr) conv)
    -- Rightは満たすがLeftが満たさない場合
    else if not (conv x y) && conv x z then
        Node y (heapify (Node x ll lr) conv) rt
    -- 両方とも満たさない場合 -- Leftが親になる
    else if conv y z then
        Node y (heapify (Node x ll lr) conv) rt
    -- 両方とも満たさない場合 -- Rightが親になる
    else
        Node z lt (heapify (Node x rl rr) conv)

goNode :: Zipper a -> Int -> Zipper a
goNode zip 0 = zip where
goNode zip num = go zip (tail bits) where
    bitNum = finiteBitSize num - countLeadingZeros num
    bits = foldl (\b a -> testBit num a : b) [] [0..bitNum - 1]
    go z [] = z
    go z (False:bits) = go (goLeft z) bits
    go z (True:bits) = go (goRight z) bits

appendNode :: Tree a -> Int -> a -> Zipper a
appendNode t currentNodeNum newVal = let
    (emptyNode, bcs) =  goNode (t, []) (currentNodeNum + 1)
    in
        (Node newVal Empty Empty, bcs)

emptyHeap :: HeapCheck a -> Heap a
emptyHeap chk = Heap {tree = Empty, check=chk, nodes = 0}

recursiveHeapify :: HeapCheck a -> Zipper a -> Tree a
recursiveHeapify c (t, []) = heapify t c
recursiveHeapify c (t, bs)= recursiveHeapify c $ goUp (heapify t c, bs)

appendHeap :: a -> Heap a -> Heap a
appendHeap a h = let
    t = tree h
    nodeNum = nodes h
    z = appendNode t nodeNum a
    heapifiedTree = recursiveHeapify (check h) z
    in
        h {tree = heapifiedTree, nodes = nodeNum + 1}

heapFromList :: [a] -> HeapCheck a -> Heap a
heapFromList as c =
    foldr appendHeap (emptyHeap c) as

dequeue :: Heap a -> (Maybe a, Heap a)
dequeue h = let
    t = tree h
    ret = case t of
        Empty -> Nothing
        Node x _ _ -> Just x
    (lastTree, bcs) = goNode (t, []) (nodes h)
    lastNum = case lastTree of
        Empty -> undefined
        Node x _ _ -> x
    topNode = case goTop (Empty, bcs) of
        ((Node top l r), _) -> (Node lastNum l r)
        (Empty, _) -> Empty
    finalTree = heapify topNode (check h)
    in
        (ret, h {tree = finalTree, nodes = (nodes h - 1)})


main :: IO ()
main = do
    print freeTree
    putStrLn "-------------------------------"
    print $ goLeft (freeTree, [])
    putStrLn "-------------------------------"
    print $ goNode (freeTree, []) 9
    putStrLn "-------------------------------"
    print $ goNode (freeTree, []) 16
    putStrLn "-------------------------------"
    print $ appendNode freeTree 15 'X'
    putStrLn "-------------------------------"
    print $ appendNode (Empty) 0 'X'
    putStrLn "------------ここからヒープ-----------------"
    let h = appendHeap 7 . appendHeap 8 . appendHeap 9 . appendHeap 10
            $ emptyHeap (>)
    print $ tree h
    putStrLn "------------ここからヒープ2-----------------"
    let (v, h2) = dequeue h
    print v
    print (tree h2)
    let (v', h2') = dequeue h2
    print v'
    print (tree h2')
    putStrLn "------------ここからヒープ3-----------------"
    let (v2, h3) = dequeue $ emptyHeap ((<) :: Int -> Int -> Bool)
    print v2
    print (tree h3)
