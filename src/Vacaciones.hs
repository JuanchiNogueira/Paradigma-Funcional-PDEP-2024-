{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Vacaciones where
import PdePreludat
import GHC.Generics (Generic(to))


--Funcion que me dan

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

--Datas

data Turista = UnTurista {cansancio :: Number,
                          stress :: Number,
                          viajaSolo :: Bool,
                          idiomas :: [String]
                          }deriving (Show)

data Marea = Fuerte | Moderada | Tranquila deriving(Show,Eq)

type Excursion = Turista -> Turista



alteroCansancio :: Number -> Turista -> Turista
alteroCansancio num turista = turista {cansancio = cansancio turista + num}

alteroStress :: Number -> Turista -> Turista
alteroStress num turista = turista {stress = stress turista + num}

nuevoIdioma :: String -> Turista -> Turista
nuevoIdioma idioma turista = turista {idiomas = idiomas turista ++ [idioma]}

acompañado :: Turista -> Turista
acompañado turista = turista {viajaSolo = False}



irAlaPlaya :: Excursion
irAlaPlaya turista
        |viajaSolo turista = alteroCansancio (-5) turista
        |otherwise = alteroStress (-1) turista

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento = alteroStress (length elemento)

hablarIdioma :: String -> Excursion
hablarIdioma idioma = acompañado.nuevoIdioma idioma

caminar :: Number -> Excursion
caminar minutos = alteroCansancio (minutos / 4).alteroStress (-(minutos / 4))

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista
        |marea == Fuerte = (alteroStress 6.alteroCansancio 10) turista
        |marea == Moderada = turista
        |marea == Tranquila = (caminar 10.apreciarPaisaje "Mar".hablarIdioma "Aleman") turista

--Modelos

ana :: Turista
ana = UnTurista 0 21 False ["Español"]

beto :: Turista
beto = UnTurista 15 15 True ["Aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["Aleman" , "Catalan"]

--Punto 2

type Indice = Turista -> Number

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = alteroStress (-(stress (excursion turista) * 0.1)) (excursion turista)

deltaExcursionSegun  :: Indice -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista 


esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista

--Tours

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20,apreciarPaisaje "cascada" , caminar 40, irAlaPlaya , hablarIdioma "Melmacquiano"]

ladoB ::Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion , caminar 120]

excursionIsla :: Marea -> Excursion
excursionIsla marea turista
        |marea == Fuerte = apreciarPaisaje "Lago" turista
        |otherwise = irAlaPlaya turista

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea , excursionIsla marea , paseoEnBarco marea ]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = alteroStress (length tour) (foldl (flip hacerExcursion) turista tour)

tourConvincente :: Tour -> Turista -> Bool
tourConvincente tour turista = any (esDesestresante turista) tour && viajaSolo (hacerTour tour turista)

convencidos :: Tour -> [Turista] -> [Turista]
convencidos tour = filter (tourConvincente tour)

espiritualidad :: Tour -> Turista -> Number
espiritualidad tour turista = deltaSegun stress (hacerTour tour turista) turista + deltaSegun cansancio (hacerTour tour turista) turista

efectividadTour :: Tour -> [Turista] -> Number
efectividadTour tour turistas = sum (map (espiritualidad tour) (convencidos tour turistas))


--Punto 4

infinitasPlayas :: Tour
infinitasPlayas = repeat irAlaPlaya

{-

--Punto B
No, es imposible ya que al aplicar la funcion any la funcion va a analizar todas las excursiones "Ir a la playa"
hasta que o encuentre una que sea desestresante o termine, como ir a la playa no es desestresante para Beto y no puede 
terminar la lista de excursiones quedara ciclando infinitamente.

Para Ana si es desestresante ir a la playa asi que si puedo saber si es convincente o no depende de si esta o no acompañada

--Punto C

No, para calcular la efectividad de este tour es necesario aplicar todas las excursiones del tour al
turista y como estas son infinitas sera imposible. Excepto que la lista de turistas este vacia caso en el que siempre dara 0

--https://docs.google.com/document/d/1C_oehBaJYavsacmThRZcrpIpX6axxVOdX19vYusRhlE/edit#heading=h.jqehittq4m6

-}