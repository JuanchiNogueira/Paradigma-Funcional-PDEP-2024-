module ParcialMario where
import PdePreludat
import GHC.Num (Num)
import System.Mem (performGC)
import GHC.Base (List)
import Data.Char (isUpper)
import GHC.Generics (Generic(Rep))
import Test.Hspec (xcontext)


--Modelaje
data Plomero = UnPlomero {nombre :: String,
                          caja :: [Herramienta],
                          reparaciones :: [Reparacion],
                          dinero :: Number
                        }deriving (Show,Eq)

data Herramienta = UnaHerramienta {denominacion :: String,
                                   precio :: Number,
                                   material :: Mango
                                  } deriving (Show, Eq)

data Mango = Hierro | Goma | Madera | Plastico deriving (Show, Eq)

--Ejemplos

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave inglesa" 200 Hierro

martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 Madera

destornillador :: Herramienta
destornillador = UnaHerramienta "Destornllador" 0 Plastico

llaveFrancesa :: Herramienta
llaveFrancesa = UnaHerramienta "Llave francesa" 1 Hierro

sumarPrecio :: Number -> Herramienta -> Herramienta
sumarPrecio sumo herramienta = herramienta {precio = precio herramienta + sumo}

herramientaInfinita :: Herramienta -> Herramienta
herramientaInfinita herramienta = sumarPrecio 1 llaveFrancesa

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa , martillo] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" (repeat (herramientaInfinita llaveFrancesa)) [] 20

--Punto 2

denominacionHerramienta :: [Herramienta] -> [String]
denominacionHerramienta = map denominacion

tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta herramienta plomero = herramienta `elem` denominacionHerramienta (caja plomero)

esMalvado :: Plomero -> Bool
esMalvado plomero = take 2 (nombre plomero) == "Wa"

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar herramienta plomero = dinero plomero >= precio herramienta

--Punto 3
esDe :: Mango -> Herramienta -> Bool
esDe recurso herramienta = material herramienta == recurso

precioMayor :: Number -> Herramienta -> Bool
precioMayor numero herramienta = precio herramienta > numero

herramientaBuena :: Herramienta -> Bool
herramientaBuena herramienta = (esDe Hierro herramienta && precioMayor 10000 herramienta) || (esDe Madera herramienta|| esDe Goma herramienta)

--Punto 4
agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero {caja = caja plomero ++ [herramienta]}

pagarPrecio :: Herramienta -> Plomero -> Plomero
pagarPrecio herramienta plomero = plomero {dinero = dinero plomero - precio herramienta}

comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta plomero herramienta
    |puedeComprar herramienta plomero = (pagarPrecio  herramienta .agregarHerramienta  herramienta ) plomero
    |otherwise = plomero

--Punto 5

data Reparacion = UnaReparacion {descripcion :: String,
                                 requerimiento :: Herramienta}deriving (Show,Eq)

filtracionDeAgua :: Reparacion
filtracionDeAgua = UnaReparacion "Filtracion de agua" llaveInglesa

estaEnMayusculas :: String -> Bool
estaEnMayusculas = all isUpper

reparacionDificil :: Reparacion -> Bool
reparacionDificil reparacion = length (descripcion reparacion) > 100 && estaEnMayusculas (descripcion reparacion)

presupuestoReparacion :: Reparacion -> Number
presupuestoReparacion reparacion = length (descripcion reparacion) * 3

--Punto 6

cobrarReparacion :: Number -> Plomero -> Plomero
cobrarReparacion cobro plomero = plomero{dinero = dinero plomero + cobro}

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero{reparaciones = reparaciones plomero ++ [reparacion]}


puedeReparar :: Plomero -> Reparacion -> Bool
puedeReparar plomero reparacion = requerimiento reparacion `elem` caja plomero

hacerReparacion :: Plomero -> Reparacion -> Plomero
hacerReparacion plomero reparacion
        |puedeReparar plomero reparacion = (efectosReparacion reparacion.agregarReparacion reparacion.cobrarReparacion (presupuestoReparacion reparacion)) plomero
        |otherwise = cobrarReparacion 100 plomero

pierdeBuenas :: Plomero -> Plomero
pierdeBuenas plomero = plomero {caja = filter (not.herramientaBuena) (caja plomero)}

sacarPrimerElemento :: [Herramienta] -> [Herramienta]
sacarPrimerElemento [] = []
sacarPrimerElemento (_:xs) = xs


efectosReparacion :: Reparacion -> Plomero -> Plomero
efectosReparacion reparacion plomero
        |esMalvado plomero = agregarHerramienta destornillador plomero
        |reparacionDificil reparacion = pierdeBuenas plomero
        |otherwise = plomero{caja = sacarPrimerElemento (caja plomero)}

esMalvadoConMartillo :: Plomero -> Bool
esMalvadoConMartillo plomero 
    |esMalvado plomero = tieneHerramienta "martillo" plomero
    |otherwise = False

--https://docs.google.com/document/d/16YC_GPPnety13jm46Y6uhlpBdv9zb7m1HgwDjfMILG0/edit#heading=h.rdm4at4lk1gl
