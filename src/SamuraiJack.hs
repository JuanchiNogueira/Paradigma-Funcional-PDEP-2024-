module SamuraiJack where

import PdePreludat
import GHC.Num (Num)

data Elemento = UnElemento {tipo :: String,
                            ataque :: Personaje-> Personaje,
                            defensa :: Personaje-> Personaje
                            }


data Personaje = UnPersonaje { nombre :: String,
                                salud :: Number ,
                                elementos :: [Elemento],
                                anioPresente :: Number
                             }

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio n personaje = personaje {anioPresente = n}

alterarSalud :: Number -> Personaje -> Personaje
alterarSalud n p = p{salud = salud p + n}

meditar :: Personaje -> Personaje
meditar p = alterarSalud (salud p /2) p

causarDanio :: Number -> Personaje -> Personaje
causarDanio danio p
        |salud p >= danio = alterarSalud (-danio) p
        |otherwise = alterarSalud (-(salud p)) p

--Punto 2

esMalvado :: Personaje -> Bool
esMalvado p =  any (\elemento -> tipo elemento == "Maldad") (elementos p)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

esMortal :: Personaje -> Personaje -> Bool
esMortal p rival = salud (ataque (head (elementos rival)) p ) == 0

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales p = filter (esMortal p)

--Punto 3

concentracion :: Number -> Elemento
concentracion n = UnElemento "Magia" id ((!!n). iterate meditar)

esbirroMalvado :: Elemento
esbirroMalvado = UnElemento "Maldad" (causarDanio 1) id

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirroMalvado

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (causarDanio 1000) id

portalFuturo :: Number -> Number -> Elemento
portalFuturo anio salud = UnElemento "Magia" (mandarAlAnio (anio+2800)) (\personaje -> aku (anio+2800) salud)

jack :: Personaje
jack = UnPersonaje "Jack" 300  [concentracion 3 , katanaMagica] 200

aku :: Number -> Number -> Personaje
aku anio salud = UnPersonaje "Aku" salud ([concentracion 4, portalFuturo anio salud] ++ esbirrosMalvados (100*anio)) 200


--Punto 4
estaMuerto :: Personaje -> Bool
estaMuerto = (==0).salud

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
        |estaMuerto atacante = (defensor,atacante)
        |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar :: t1 -> (t1 -> t2) -> t2
afectar personaje funcion = funcion personaje

--https://docs.google.com/document/d/1mhQ2R8VjpoVrQ5JroYbkiBHjZME9gw6jEn6OI2q6Q2U/edit#heading=h.31aewt8j1c85