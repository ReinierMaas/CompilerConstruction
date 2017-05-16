module Main where

import Data.GraphViz
import Data.GraphViz.Attributes (toLabel)
import Data.GraphViz.Printing (toDot, renderDot)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy (unpack)

import Lexer
import Parser

main :: IO ()
main = undefined

myGraph :: Gr String String
myGraph = foldl (\acc x -> x acc) empty cmds
    where cmds = [ insNode (1, "x = 1")
                 , insNode (2, "x == 1")
                 , insNode (3, "x = x + 2")
                 , insNode (4, "x = x - 2")
                 , insEdge (1, 2, "")
                 , insEdge (2, 3, "true")
                 , insEdge (2, 4, "false")]
