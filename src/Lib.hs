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

lluviaDeTuercas :: Personaje -> Personaje -> Personaje
lluviaDeTuercas unPamela otroPersonaje
  | poderBasico unPamela == "Lluvia de Tuercas Sanadoras" = otroPersonaje {vida = vida otroPersonaje + 800}
  | poderBasico unPamela == "Lluvia de Tuercas Dañinas" = otroPersonaje {vida = div (vida otroPersonaje) 2}
  | otherwise = otroPersonaje

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio otroPersonaje
  | radio > 3 && vida otroPersonaje < 800 = otroPersonaje {nombre = nombre otroPersonaje ++ " - Espina estuvo aqui", tieneSuperPoder = False, vida = 0}
  | radio > 3 = bolaEspinosa (otroPersonaje {nombre = nombre otroPersonaje ++ " - Espina estuvo aqui"})
  | otherwise = bolaEspinosa otroPersonaje

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje {tieneSuperPoder = True, vida = vida unPersonaje * 2}

atacarConPoderEspecial :: Personaje -> String
atacarConPoderEspecial unPersonaje
  | tieneSuperPoder unPersonaje == True = "Atacas a tu contrincante con el super y con el basico."
  | otherwise = "No haces nada"

quienesEstanUltimas :: [Personaje] -> [String]
quienesEstanUltimas listaPersonajes = map extraerNombre (filter (estaUltimas) listaPersonajes)

estaUltimas :: Personaje -> Bool
estaUltimas unPersonaje = vida unPersonaje < 800

extraerNombre :: Personaje -> String
extraerNombre unPersonaje = nombre unPersonaje