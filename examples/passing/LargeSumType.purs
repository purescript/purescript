module Main where

import Control.Monad.Eff.Console (log)

data Large = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

explode A A = "A"
explode B B = "B"
explode C C = "C"
explode D D = "D"
explode E E = "E"
explode F F = "F"
explode G G = "G"
explode H H = "H"
explode I I = "I"
explode J J = "J"
explode K K = "K"
explode L L = "L"
explode M M = "M"
explode N N = "N"
explode O O = "O"
explode P P = "P"
explode Q Q = "Q"
explode R R = "R"
explode S S = "S"
explode T T = "T"
explode U U = "U"
explode V V = "V"
explode W W = "W"
explode X X = "X"
explode Y Y = "Y"
explode Z Z = "Z"
explode _ _ = ""

main = log "Done"
