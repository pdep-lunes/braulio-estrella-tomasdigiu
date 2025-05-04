module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String, 
    superPoder :: String,
    tieneSuperPoder :: Bool,
    vida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas Radio 5" True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" "Lluvia de Tuercas Sanadoras" "Torreta Curativa" False 9600

brawlers :: [Personaje]
brawlers = [espina, pamela]

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje
  | vida unPersonaje > 1000 = unPersonaje {vida = vida unPersonaje - 1000}
  | otherwise = unPersonaje {vida = 0}
