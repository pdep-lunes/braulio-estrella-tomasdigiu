module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String, 
    superPoder :: String,
    tieneSuperPoder :: Bool,
    cantidadDeVida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas" True 4800