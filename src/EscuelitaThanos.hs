{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
module EscuelitaThanos where
import PdePreludat



--Punto 1
data Personaje = UnPersonaje {nombre :: String,
                              edad :: Number,
                              energia :: Number,
                              habilidades :: [String],
                              planeta :: String
                              }deriving (Show)

data Guantelete = UnGuantelete {material :: String,
                                gemas :: [Gema]
                                }deriving (Show)

type Universo = [Personaje]
type Gema = Personaje -> Personaje

--Punto 2

aptoPendex :: Universo -> Bool
aptoPendex = any ((>45) . edad)

energiaUniverso :: Universo -> Number
energiaUniverso universo = sum (map energia universo)

--Punto 3

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad hab per = per {habilidades = filter (/= hab) (habilidades per)}

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades per = per {habilidades = []}

sinEnergia :: Personaje -> Personaje
sinEnergia per = per {energia = 0}

transportarPlaneta :: String -> Personaje -> Personaje
transportarPlaneta planetaNuevo per = per {planeta = planetaNuevo}

debilitar :: Number -> Personaje -> Personaje
debilitar n personaje = personaje {energia = energia personaje - n}

reducirEdad :: Number -> Personaje -> Personaje
reducirEdad n per
        |edad per >= 36 = per {edad = edad per - n}
        |otherwise = per{edad = 18}

mente :: Number -> Gema
mente = debilitar

alma :: String -> Gema
alma hab = debilitar 10.quitarHabilidad hab

espacio :: String -> Gema
espacio planeta = transportarPlaneta planeta. debilitar 20

poder :: Gema
poder personaje
    |length (habilidades personaje) <= 2 = (quitarHabilidades.sinEnergia) personaje

tiempo :: Gema
tiempo per = reducirEdad (edad per / 2) per

gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca gema  = gema.gema

--Punto 4

guanteleteGoma :: Guantelete
guanteleteGoma = UnGuantelete "Goma" [tiempo, alma "Usar Mjolnir",gemaLoca (alma "Programacion en Haskell")]

--Punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldr ($) personaje gemas

--Punto 6

masFuerte ::Personaje -> Gema -> Gema -> Gema
masFuerte per g1 g2
        |energia (g1 per) >= energia (g2 per) = g1
        |otherwise = g2

gemaPoderosa :: [Gema] -> Personaje -> Gema
gemaPoderosa [g1] _ = g1
gemaPoderosa (g1:g2 : gs) personaje = gemaPoderosa (masFuerte personaje g1 g2 : gs) personaje

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete = gemaPoderosa (gemas guantelete)

--Punto 7

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
Si, se puede ejecutar debido a que la funcion take devuelve las primeras 3 gemas de la lista infinita
para que luego la funcion utilizar trabaje solamente con eso. Esto es posible dbeido a que Haskell trabaja
con un metodo de Lazy Evaluation donde no es necesario analizar la lista entera de gemas sino solamente lo que 
necesita
-}

--https://docs.google.com/document/d/1IKrJkdbPyoxfHqREIfqzxpsBdANcL2g9gvs9t-IR30E/edit#heading=h.ov2zcvcgcy0t