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
espina = UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas Radio 5" True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" "Lluvia de Tuercas Sanadoras" "Torreta Curativa" False 9600

bolaEspinosa :: Int -> Int
bolaEspinosa vida
  | vida >= 1000 = vida - 1000
  | otherwise = 0

lluviaDeTuercas :: String -> Int -> Int
lluviaDeTuercas tipoDeTuercas vida
  | tipoDeTuercas == "Sanadoras" = vida + 800
  | tipoDeTuercas == "Dañinas" = div vida 2
  | otherwise = vida

granadaDeEspinas :: Personaje -> Int -> Personaje
granadaDeEspinas unPersonaje radio
  | radio > 3 =  unPersonaje {nombre = nombre ++ " Espina estuvo aqui"}
  | radio > 3 && cantidadDeVida unPersonaje < 800 = UnPersonaje 
  | otherwise = unPersonaje