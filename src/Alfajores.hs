module Alfajores where
import GHC.Num (Num)
import System.Mem (performGC)

import PdePreludat



type Criterio = Alfajor -> Bool

--Datas

data Alfajor =UnAlfajor{capas :: [Relleno],
                        peso :: Number,
                        dulzor :: Number,
                        nombre :: String
                       }deriving (Show,Eq)

data Cliente = UnCliente {nombreCliente :: String,
                          dinero :: Number,
                          alfajoresComprados :: [Alfajor],
                          gustos :: [Criterio]}deriving (Show)

data Relleno = DulceDeLeche | Mousee | Fruta deriving (Show, Eq)

--Modelos

jorgito :: Alfajor
jorgito = UnAlfajor [DulceDeLeche] 80 8 "Jorgito"

havanna :: Alfajor
havanna = UnAlfajor [Mousee , Mousee] 60 12 "Havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = UnAlfajor [DulceDeLeche] 40 12 "Capitan del espacio"

jorgitito :: Alfajor
jorgitito = (abaratarAlfajor.renombrarAlfajor "Jorgitito") jorgito

jorgelin :: Alfajor
jorgelin = (agregarCapa DulceDeLeche .renombrarAlfajor "Jorgelin") jorgito

capitanDelEspacioDecostaAcosta :: Alfajor
capitanDelEspacioDecostaAcosta = renombrarAlfajor "Capitan del espacio de costa a costa" (alfajorNpremium 4 alfajorPremium (abaratarAlfajor capitanDelEspacio))

emi :: Cliente
emi = UnCliente "Emi" 120 [] [soloMarca "Capitan del espacio"]

tomi :: Cliente
tomi = UnCliente "Tomi" 100 [] [pretencioso , dulcero]

dante :: Cliente
dante = UnCliente "Dante" 200 [] [sinRelleno DulceDeLeche, extraño]
--1b

coeficieteDeDulzor :: Alfajor -> Number
coeficieteDeDulzor alfajor = dulzor alfajor / peso alfajor

precioRelleno :: Relleno -> Number
precioRelleno relleno
    |relleno == DulceDeLeche = 12
    |relleno == Fruta = 10
    |relleno == Mousee = 15

precioAlfajor :: Alfajor -> Number
precioAlfajor alfajor = (peso alfajor * 2) + sum (map precioRelleno (capas alfajor))

alfajorPotable :: Alfajor -> Bool
alfajorPotable alfajor = not (null (capas alfajor)) && coeficieteDeDulzor alfajor >= 0.1

--Parte 2

type Modificacion = Alfajor -> Alfajor

modificarPeso :: Number -> Modificacion
modificarPeso cambio alfajor = alfajor{peso = peso alfajor + cambio}

modificarDulzor :: Number -> Modificacion
modificarDulzor cambio alfajor = alfajor {dulzor = dulzor alfajor + cambio}

renombrarAlfajor :: String -> Modificacion
renombrarAlfajor nombreNuevo alfajor = alfajor {nombre = nombreNuevo}

agregarCapa :: Relleno -> Modificacion
agregarCapa relleno alfajor = alfajor{capas = capas alfajor ++ [relleno]}

abaratarAlfajor :: Modificacion
abaratarAlfajor = modificarPeso (-10).modificarDulzor (-7)

alfajorPremium :: Modificacion
alfajorPremium alfajor
        |alfajorPotable alfajor = (renombrarAlfajor ("Premium " ++ nombre alfajor).agregarCapa (head (capas alfajor))) alfajor
        |otherwise = alfajor

aplicarNveces :: Number -> Modificacion -> Alfajor -> Alfajor
aplicarNveces grado modificacion alfajor
        | grado > 0 = aplicarNveces (grado - 1) modificacion (modificacion alfajor)
        |otherwise = alfajor

alfajorNpremium :: Number -> Modificacion -> Alfajor -> Alfajor
alfajorNpremium = aplicarNveces

--3

soloMarca :: String -> Criterio
soloMarca marca alfajor = contieneSubcadena marca (nombre alfajor)

pretencioso :: Criterio
pretencioso alfajor = contieneSubcadena "Premium" (nombre alfajor)

dulcero :: Criterio
dulcero alfajor = coeficieteDeDulzor alfajor > 0.15

sinRelleno :: Relleno -> Criterio
sinRelleno relleno alfajor = relleno `notElem` capas alfajor

contieneSubcadena :: String -> String -> Bool
contieneSubcadena subcadena cadena
      |length subcadena > length cadena = False
      |take (length subcadena) cadena == subcadena = True
      |otherwise = False

extraño :: Criterio
extraño alfajor = not (alfajorPotable alfajor)

--b

cualesLesGusta :: [Alfajor] -> Cliente -> [Alfajor]
cualesLesGusta alfajores cliente = lesGusta alfajores (gustos cliente)

lesGusta :: [Alfajor] -> [Criterio] -> [Alfajor]
lesGusta alfajores criterios = filter (cumpleCriterio criterios) alfajores

cumpleCriterio :: [Criterio] -> Alfajor -> Bool
cumpleCriterio criterios alfajor = all  (\gustoPersonal -> gustoPersonal alfajor) criterios

--c

puedeComprar2 :: Cliente -> Alfajor -> Bool
puedeComprar2 cliente alfajor = dinero cliente >= precioAlfajor alfajor

agregarAlfajor :: Cliente -> Alfajor -> Cliente
agregarAlfajor cliente alfajor = cliente {alfajoresComprados = alfajoresComprados cliente ++ [alfajor]}

pagarPrecio2 :: Cliente -> Alfajor -> Cliente
pagarPrecio2 cliente alfajor = cliente {dinero = dinero cliente - precioAlfajor alfajor}

comprarAlfajor :: Cliente -> Alfajor-> Cliente
comprarAlfajor cliente alfajor 
                |puedeComprar2 cliente alfajor = agregarAlfajor (pagarPrecio2 cliente alfajor) alfajor

--d

comprarRicos :: Cliente -> [Alfajor] -> Cliente
comprarRicos cliente alfajores = foldl comprarAlfajor cliente (cualesLesGusta alfajores cliente)

--https://docs.google.com/document/d/1m8gRD-gheA2fDbiDXc7-dpiymkhDQRbmzr-plL5BGuA/edit#heading=h.koqhgouszkej