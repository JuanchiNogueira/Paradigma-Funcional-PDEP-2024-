module Fmi where
import PdePreludat
import GHC.Num (Num)



data Pais = UnPais {ingresoPerCapita :: Number,
                     activoPublico :: Number,
                     activoPrivado :: Number,
                     recursosNaturales :: [Recurso],
                     deudaActiva :: Number
                     }

type Estrategia = Pais -> Pais
type Receta = [Estrategia]
type Recurso = String

--Modelaje 

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["Mineria" , "Exoturismo"] 5000000

--Funciones auxiliares
cambioDeuda :: Number -> Estrategia
cambioDeuda n pais = pais {deudaActiva = deudaActiva pais + n}

reducirPorcentajePublico :: Number -> Pais -> Pais
reducirPorcentajePublico n pais = pais {activoPublico = activoPublico pais * (n/100)}

reducirIPC :: Number -> Pais -> Pais
reducirIPC n pais = pais {ingresoPerCapita = ingresoPerCapita pais - n}

quitarRecursopais :: Recurso -> Pais -> Pais
quitarRecursopais n pais = pais {recursosNaturales = filter (/= n) (recursosNaturales pais)}


pbi :: Pais -> Number
pbi pais = ingresoPerCapita pais * (activoPrivado pais + activoPublico pais)

--Estrategias (Punto 2)

prestarDolares :: Number -> Estrategia
prestarDolares n = cambioDeuda (n*1.5)

acortarSectorPublico ::  Number -> Estrategia
acortarSectorPublico n pais
    |activoPublico pais > 100 = (reducirPorcentajePublico 20. reducirIPC n) pais

explotacionRecursos :: Recurso -> Estrategia
explotacionRecursos recurso = cambioDeuda (-2000000).quitarRecursopais recurso

--El enunciado no aclara si el calculo del PBI se debe hacer previo o post reduccion del sector publico, yo lo hize POST
blindaje :: Estrategia
blindaje pais = (reducirPorcentajePublico 500.prestarDolares (pbi pais / 2)) pais

--Punto 3

receta1 :: Receta
receta1 = [prestarDolares 200,explotacionRecursos "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta

--Punto 4

tieneRecurso :: String -> Pais -> Bool
tieneRecurso recurso pais = recurso `elem` recursosNaturales pais

paisesZafan :: [Pais] -> [Pais]
paisesZafan = filter (tieneRecurso "Petroleo")

deudaTotal :: [Pais] -> Number
deudaTotal paises = sum (map deudaActiva paises)

{- 
Orden superior: Usando funciones del tipo map y filter 
Composicion

No pude aplicar composicion ni aplicacion parcial en este punto, no le encontre
ninguna ventaja a hacerlo
-}

--Punto 5

recetasOrdenadas :: [Receta] -> Pais -> Bool 
recetasOrdenadas [] pais = True
recetasOrdenadas [_] pais = True 
recetasOrdenadas (x1: x2 :xs) pais = (pbi (aplicarReceta x1 pais) < pbi (aplicarReceta x2 pais)) && recetasOrdenadas xs pais

--Punto 6

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

{-
a) al aplicar la funcion elem al pais con recursos naturales infinitos, esta buscara entre todos sus elementos
hasta encontrar el petroleo o que termine la lista, al no tener el petroleo y esta ser infinita quedara en un loop infinito

b) Me devolvera el valor de la deuda activa sin problema debido a que no es necesario realizar ningun funcion con los 
recursos naturales 

relacionado con evaluacion diferida, solo se evalua lo que se necesita

https://docs.google.com/document/d/1l9UjDqVhLdeiON6rtXf7EwGU5JZvN2TWu5AJQzVmSwE/edit
-}

