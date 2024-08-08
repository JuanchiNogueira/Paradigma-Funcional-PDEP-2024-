--Modelaje 
module HaskellEspacial where

import PdePreludat

data Nave = UnaNave {nombre :: String,
                     durabilidad :: Number,
                     escudo :: Number,
                     ataque :: Number,
                     poder :: Poder}

type Poder = Nave -> Nave

modificarAtaque :: Number -> Nave -> Nave
modificarAtaque n nave = nave {ataque = ataque nave + n}

modificarDurabilidad :: Number -> Nave -> Nave
modificarDurabilidad n nave= nave {durabilidad = durabilidad nave + n}

alteroEscudos :: Number -> Nave -> Nave
alteroEscudos n nave = nave {escudo = escudo nave + n}

turbo :: Poder
turbo = modificarAtaque 25

reparacionEmergencia :: Poder
reparacionEmergencia = modificarAtaque (-30).modificarDurabilidad 50

superTurbo :: Poder
superTurbo = turbo.turbo.turbo.modificarDurabilidad 45

cargaSismica :: Poder
cargaSismica = modificarAtaque 100.alteroEscudos (-50)

tieFighter :: Nave
tieFighter = UnaNave "X-Wing" 200 100 50 turbo

xWing :: Nave
xWing = UnaNave "X-Wing" 300 150 100 reparacionEmergencia

naveDarth :: Nave
naveDarth = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo

halconMilenario :: Nave
halconMilenario = UnaNave "Halcon milenario" 1000 500 50  (reparacionEmergencia.alteroEscudos 100)

slaveOne :: Nave
slaveOne = UnaNave "Slave one" 700 400 300 cargaSismica

--https://docs.google.com/document/d/1rbOy1rIFmBxMRhTOWvI-u097l9KatHRbqt5KBXvVSfI/edit

--Punto 2
type Flota = [Nave]

durabilidadTotal :: Flota -> Number
durabilidadTotal naves = sum (map durabilidad naves)

--Punto 3

noPuedeAtacar :: Nave -> Nave -> Bool
noPuedeAtacar atacada atacante = escudo (poder atacada atacada) > ataque (poder atacante atacante)

pelea :: Nave -> Nave -> Nave
pelea atacada atacante
        |noPuedeAtacar atacada atacante = atacada
        |otherwise = modificarDurabilidad (-danio) (poder atacada atacada)

    where danio = ataque (poder atacante atacante) - escudo (poder atacada atacada)

--Punto 4

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

--Punto 5 (Esta mal creo)

type Estrategia = [Nave] -> [Nave]

inofensiva :: Nave -> Bool
inofensiva nave = ataque nave < 100 && escudo nave < 50

navesDebiles :: Estrategia
navesDebiles = filter (\nave -> escudo nave < 200)

navesPeligrosas :: Number -> Estrategia
navesPeligrosas n = filter (\nave -> ataque nave > n)

navesFueraDeCombate :: Nave -> Estrategia
navesFueraDeCombate atacante naves = filter fueraDeCombate (map (pelea atacante) naves)

navesInofensivas :: Estrategia
navesInofensivas = filter inofensiva

misionSorpresa :: Nave -> Flota -> Estrategia -> Flota
misionSorpresa nave flota estrategia = map (pelea nave) (estrategia flota)

--Punto 6 (creo que esta mal)

minimizaDurabilidad :: Flota -> Estrategia -> Estrategia -> Estrategia
minimizaDurabilidad flota e1 e2
        |sum (map durabilidad (e1 flota)) >= sum (map durabilidad (e2 flota)) = e1
        |otherwise = e2


mejorEsrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
mejorEsrategia nave flota e1 e2 = misionSorpresa nave flota (minimizaDurabilidad flota e1 e2)

--Punto 7  (Mal)

flotaInfinita :: Flota 
flotaInfinita = repeat xWing

{-
No, es imposible debido que al funcion map necesita acceder a todos las naves de la flota

Cuando se lleva adelante una mision con ella entra en un loop infinito, dara stack overflow

https://docs.google.com/document/d/1rbOy1rIFmBxMRhTOWvI-u097l9KatHRbqt5KBXvVSfI/edit

-}
