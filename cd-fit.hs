module Main where

import Text.ParserCombinators.Parsec
import Data.List (sortBy)
import Test.QuickCheck
import Control.Monad (liftM2, replicateM)

-- parseInput parses output of "du -sb", which consists of many lines.
-- each of which describes single directory
parseInput =
    do dirs <- many dirAndSize
       eof :: Parser ()
       return dirs

-- Datatype Dir holds information about single directory - its size and name
data Dir = Dir {dir_size::Int, dir_name::String} deriving Show

-- dirAndSize parses information about single directory, which is:
-- a size in bytes (number), som spacaes, then directory name, which extends till newline
dirAndSize =
    do size <- many1 digit
       spaces
       dir_name <- anyChar `manyTill` newline
       return (Dir (read size) dir_name)

-- DirPack holds a set of directories which are to be stored on a single CD.
-- 'pack_size' could be calculated, be we will store it separately to reduce
-- amount of calculation
data DirPack = DirPack {pack_size::Int, dirs::[Dir]} deriving Show

-- For simplicity, lets assume we deal with standard 700MB CDs for now
media_size = 700*1024*1024

-- Greedy packer tries to add directories one by one to initially empty 'DirPack'
greedy_pack dirs = foldl maybe_add_dir (DirPack 0 []) $ sortBy cmpSize dirs
   where
   cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

-- Helper function which only adds directory "d" to the pack "p" when new
-- total size does not exceed media_size
maybe_add_dir p d =
    let new_size = pack_size p + dir_size d
        new_dirs = d:(dirs p)
        in if new_size > media_size then p else DirPack new_size new_dirs


-- We must teach quickcheck how to generate arbitrary "Dir"s
instance Arbitrary Dir where
    arbitrary = liftM2 Dir gen_size gen_name
        where gen_size = do s <- choose(10,1400)
                            return (s*1024*1024)
              gen_name = do n <- choose(1,300)
                            replicateM n (elements "fubar/")

prop_greedy_pack_is_fixpoint ds =
    let pack = greedy_pack ds
        in pack_size pack == pack_size (greedy_pack (dirs pack))

main = do input <- getContents
       	  putStrLn ("DEBUG: got input " ++ input)
          let dirs = case parse parseInput "stdin" input of
                       Left err -> error $ "Input:\n" ++ show input ++
                                           "\nError:\n" ++ show err
                       Right result -> result
          putStrLn "Solution:"; print (greedy_pack dirs)

                   
