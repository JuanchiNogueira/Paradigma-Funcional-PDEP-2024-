module PisteoComoCampeon where
import PdePreludat

data Auto = UnAuto {marca :: String,
                    modelo :: String,
                    desgaste :: (Ruedas , Chasis),
                    velocidadMax :: Number,
                    tiempoCarrera :: Number
                    }

type Ruedas = Number
type Chasis = Number
type Angulo = Number
type Longitud = Number
type Tramo = Auto -> Auto
type Pista = [Tramo]

--Modelaje

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0,0) 65 0

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" (7,4) 73 0

fiat :: Auto
fiat = UnAuto "Fiat" "600" (33,27) 44 0

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

tramoClassic :: Tramo
tramoClassic = tramoRecto 750

tramito :: Tramo
tramito = tramoRecto 280

--Punto 2

buenEstado :: Auto -> Bool
buenEstado auto = fst (desgaste auto) < 40 && snd (desgaste auto) < 60

noDaMas :: Auto -> Bool
noDaMas auto = fst (desgaste auto) > 80 || snd (desgaste auto) > 80

--Punto 3

cambioChasis :: Number -> Auto -> Auto
cambioChasis n auto = auto{desgaste = (fst (desgaste auto), snd (desgaste auto) * n / 100)}

repararAuto :: Auto -> Auto
repararAuto = cambioChasis 85

--Punto 4

danioRuedas :: Number -> Auto -> Auto
danioRuedas n auto = auto {desgaste = (fst (desgaste auto), snd (desgaste auto) + n)}

sumarTiempo :: Number -> Auto -> Auto
sumarTiempo n auto = auto {tiempoCarrera = tiempoCarrera auto + n}

curva :: Angulo -> Longitud -> Tramo
curva ang long auto = (danioRuedas ((3 * long)/ang).sumarTiempo (long / (velocidadMax auto / 2))) auto

tramoRecto :: Longitud -> Tramo
tramoRecto long auto = (cambioChasis (long / 100).sumarTiempo (long / velocidadMax auto)) auto

box :: Tramo -> Auto -> Auto
box tramo auto
    |buenEstado auto = tramo auto
    |otherwise = (sumarTiempo 10.tramo.repararAuto) auto

pistaMojada :: Tramo -> Auto -> Auto
pistaMojada tramo auto =  sumarTiempo (tiempoCarrera (tramo auto) / 2) auto

ripio :: Tramo -> Auto -> Auto
ripio tramo = tramo . tramo

obstruccion :: Longitud -> Tramo -> Auto -> Auto
obstruccion long tramo = tramo . danioRuedas (long * 2)

--Punto 5

pasarTramo2 :: Tramo -> Auto -> Auto
pasarTramo2 tramo auto
        |noDaMas auto = auto
        |otherwise = tramo auto

--Punto 6

superPista :: Pista
superPista = [tramoClassic , curvaTranca , pistaMojada tramito, tramito , obstruccion 2 (curva 80 400), curva 115 650
             ,tramoRecto 970 , curvaPeligrosa , ripio tramito , box (tramoRecto 800)]

--6(b) -- No me salio

peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista = filter (not . noDaMas) . map (pasarTramo2 . vueltaPista $ pista)

vueltaPista :: Pista -> Tramo
vueltaPista = foldl (.) id

-- Punto 7

data Carrera = Carrera {
    pista :: Pista,
    vueltas :: Number
}

tourBuenosAires :: Carrera
tourBuenosAires = Carrera {
    pista = superPista,
    vueltas = 20
}

correrUnaCarrera :: Carrera -> [Auto] -> [[Auto]]
correrUnaCarrera carrera autos = take (vueltas carrera) (iterate (peganLaVuelta (pista carrera)) autos)