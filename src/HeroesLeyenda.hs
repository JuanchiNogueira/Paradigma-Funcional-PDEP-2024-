{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use replicate" #-}

module HeroesLeyenda where
import PdePreludat


--Estructuras
data Heroe = UnHeroe {nombre :: String,
                      epiteto :: String,
                      reconocimiento :: Number,
                      artefactos :: [Artefacto],
                      tareas :: [Tarea]
                      }deriving (Show , Eq)

data Artefacto = UnArtefacto {denominacion :: String,
                              rareza :: Number
                            }deriving(Show,Eq)

data Bestia = UnaBestia {nombreBestia :: String,
                         debilidad :: Debilidad}deriving (Show , Eq)

--Ejemplo

perseo :: Heroe
perseo = UnHeroe "Perseo" "Nada" 50 [] []

pistola :: Artefacto
pistola = UnArtefacto "Pistola" 1000

rayoZeus :: Artefacto
rayoZeus = UnArtefacto "El relampago de Zeus" 500

heracles :: Heroe
heracles = UnHeroe "heracle" "Guardian del Olimpo" 700 [pistola, rayoZeus] []

nemea :: Bestia
nemea = UnaBestia "Nemes" ((>20) . length . epiteto)
--Tipos

type Tarea = Heroe -> Heroe
type Debilidad = Heroe ->  Bool

--Punto 2

agregarArtefacto :: String -> Number -> Heroe -> Heroe
agregarArtefacto artefacto indiceRareza heroe = heroe {artefactos = artefactos heroe ++ [UnArtefacto artefacto indiceRareza]}

nuevoEpiteto :: String -> Heroe -> Heroe
nuevoEpiteto titulo heroe = heroe {epiteto = titulo }

pasarAlaHistoria :: Heroe -> Heroe
pasarAlaHistoria heroe
        |reconocimiento heroe > 1000 = nuevoEpiteto "El mitico" heroe
        |reconocimiento heroe >= 500 = (agregarArtefacto "Lanza del olimpo" 100.nuevoEpiteto "El magnifico") heroe
        |reconocimiento heroe > 100 = (agregarArtefacto "Xiphos" 50. nuevoEpiteto "Hoplita") heroe
        |otherwise = heroe

--Punto 3

--Funciones auxiliares
sumarReconocimiento :: Number -> Heroe -> Heroe
sumarReconocimiento nuevo heroe = heroe {reconocimiento = reconocimiento heroe + nuevo}

agregarArtefacto' :: Artefacto -> Heroe -> Heroe
agregarArtefacto' artefacto heroe = heroe {artefactos = artefactos heroe ++ [artefacto]}

triplicoRareza :: Artefacto -> Artefacto
triplicoRareza artefacto = artefacto {rareza = rareza artefacto * 3}

modificoArtefactos :: Heroe -> Heroe
modificoArtefactos heroe = heroe {artefactos = map triplicoRareza (artefactos heroe)}

noCumple :: Number -> Artefacto -> Bool
noCumple minimo artefacto = rareza artefacto > minimo

desecharNoCumplen :: Number -> Heroe -> Heroe
desecharNoCumplen minimo heroe = heroe {artefactos = filter (noCumple 1000) (artefactos heroe)}

epitetoLargo :: Number -> String ->  Heroe -> Heroe
epitetoLargo veces titulo = nuevoEpiteto (titulo ++ take veces (repeat (last titulo) ))

sacarPrimero :: [Artefacto] -> [Artefacto]
sacarPrimero [] = []
sacarPrimero [_] = []
sacarPrimero (x:xs) = xs

perderPrimerArtefacto :: Heroe -> Heroe
perderPrimerArtefacto heroe = heroe {artefactos = sacarPrimero (artefactos heroe)}

--Tareas
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto = agregarArtefacto' artefacto.sumarReconocimiento (rareza artefacto)

escalarOlimpo :: Tarea
escalarOlimpo = agregarArtefacto' rayoZeus.sumarReconocimiento 500.desecharNoCumplen 1000.modificoArtefactos

ayudaCruzarCalle :: Number -> Tarea
ayudaCruzarCalle veces = epitetoLargo (veces - 1) "Groso"

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe
                |debilidad bestia heroe = nuevoEpiteto ("El asesino de " ++ nombreBestia bestia) heroe
                |otherwise = (perderPrimerArtefacto. nuevoEpiteto "El cobarde") heroe

--Punto 6

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea = agregarTarea unaTarea . unaTarea

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = unHeroe {tareas = unaTarea : tareas unHeroe}

--Punto 7

hacerTareas :: [Tarea] -> Heroe -> Heroe
hacerTareas [] heroe = heroe
hacerTareas [x] heroe = x heroe
hacerTareas (x:xs) heroe = hacerTareas xs (hacerUnaTarea x heroe)
--(punto 9 hacer tareas)

rarezaTotal :: [Artefacto] -> Number
rarezaTotal artefactos = sum (map rareza artefactos)

presumir :: Heroe -> Heroe -> (Heroe , Heroe)
presumir heroe1 heroe2
        |reconocimiento heroe1 > reconocimiento heroe2 = (heroe1,heroe2)
        |reconocimiento heroe2 > reconocimiento heroe1 = (heroe2,heroe1)
        |rarezaTotal (artefactos heroe1) > rarezaTotal (artefactos heroe2) = (heroe1,heroe2)
        |rarezaTotal (artefactos heroe2) > rarezaTotal (artefactos heroe1) = (heroe2,heroe1)
        |otherwise = presumir (hacerTareas (tareas heroe1) heroe2) (hacerTareas (tareas heroe2) heroe1)

--Punto 8

{-
El resultado sera un loop infiniro en el que se seguira comparando ambos heroes devido a que la funcion hacerTareas
al recibir una lista vacia de tareas devuelve al mismo heroe, estos volveran a compararse en la funcion presumir y asi infinitamente
-}

--Punto 10

{-
no, no se podrá conocer el estado final porque se iterará infinitamente y nunca podrá mostrarse un "estado final"
-}

--https://docs.google.com/document/d/1-99rJlDO-mZLzJMwKLThLFKEXAmziyZJEfHtYzUV0jU/edit

--CORRECCION :: Tendria que arreglar las funciones Agregar artefacto, la segunda forma es mas correcta y tendria que modelar previamente las armas que quiero agregar