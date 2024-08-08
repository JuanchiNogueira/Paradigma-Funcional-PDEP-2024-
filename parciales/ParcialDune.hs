module ParcialDune where
import PdePreludat


--Modelaje
data Fremen = UnFremen {nombre :: String,
                        tolerancia :: Number,
                        titulos :: [String],
                        reconocimientos :: Number}deriving(Show,Eq)
type Tribu = [Fremen]

data GusanoArena =UnGusano{longitud :: Number,
                            hidratacion :: Number,
                            descripcion :: String}deriving(Show)

--Casos

stilgar :: Fremen
stilgar = UnFremen "Stilgar" 150 ["Guia"] 3

gusano1 :: GusanoArena
gusano1 = UnGusano 10 5 "Rojo con lunares"

gusano2 :: GusanoArena
gusano2 = UnGusano 8 1 "Dientes puntiagudos"

--1a (Funciones genericas)

nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen {reconocimientos = reconocimientos fremen + 1}

sumarTitulo :: String -> Fremen -> Fremen
sumarTitulo titulo fremen = fremen{titulos = titulos fremen ++ [titulo]}

cambiarTolerancia :: Number -> Fremen -> Fremen
cambiarTolerancia num fremen = fremen {tolerancia = tolerancia fremen + num}
--1b

condicionElegido :: Fremen -> Bool
condicionElegido fremen = "Domador" `elem` titulos fremen && tolerancia fremen > 100

candidatoElegido :: Tribu -> Bool
candidatoElegido = any condicionElegido

--1c

candidatosElegido :: Tribu -> Tribu
candidatosElegido = filter condicionElegido

tieneMasReconocimiento :: Fremen -> Fremen -> Fremen
tieneMasReconocimiento fremen1 fremen2
    |reconocimientos fremen1 > reconocimientos fremen2 = fremen1
    |otherwise = fremen2

elegido :: Tribu -> Fremen
elegido tribu = foldl1 tieneMasReconocimiento (candidatosElegido tribu)

--2

--Parte1
longitudEntreDos :: Number -> GusanoArena -> GusanoArena -> Number
longitudEntreDos porcentaje gus1 gus2
        |longitud gus1 > longitud gus2 = (longitud gus1 * porcentaje) / 100
        |otherwise = (longitud gus2 * porcentaje) / 100

concatenacionDescripciones :: GusanoArena -> GusanoArena -> String
concatenacionDescripciones gus1 gus2 = descripcion gus1 ++ "-" ++ descripcion gus2

criaGusanos :: GusanoArena -> GusanoArena -> GusanoArena
criaGusanos gusano1 gusano2 = UnGusano (longitudEntreDos 10 gusano1 gusano2) 0 (concatenacionDescripciones gusano1 gusano2)


--Parte 2
type Gusanos = [GusanoArena]

apareoMasivo :: Gusanos -> Gusanos -> Gusanos
apareoMasivo [] _ = []
apareoMasivo _ [] = []
apareoMasivo (g1:gs1) (g2:gs2) = criaGusanos g1 g2 : apareoMasivo gs1 gs2

--Parte 3 

--Modelo misiones

type Mision = GusanoArena -> Fremen-> Fremen

puedeDomar :: Fremen -> GusanoArena -> Bool
puedeDomar fremen gusano = tolerancia fremen >= (longitud gusano / 2)

domarGusano :: Mision
domarGusano gusano fremen
        |puedeDomar fremen gusano = (cambiarTolerancia 100. sumarTitulo "Domador") fremen
        |otherwise = cambiarTolerancia (- (tolerancia fremen * 0.1)) fremen

puedeDestruir :: Fremen -> GusanoArena -> Bool
puedeDestruir fremen gusano = "Domador" `elem` titulos fremen && tolerancia fremen < (longitud gusano / 2)

destruirGusano :: Mision
destruirGusano gusano fremen
        |puedeDestruir fremen gusano = (nuevoReconocimiento.cambiarTolerancia 100) fremen 
        |otherwise = cambiarTolerancia (-(tolerancia fremen * 0.2)) fremen

puedeMontar :: Fremen -> GusanoArena -> Bool
puedeMontar fremen gusano = puedeDomar fremen gusano && tolerancia fremen > hidratacion gusano

montarGusano :: Mision
montarGusano gusano fremen
        |puedeMontar fremen gusano = (nuevoReconocimiento.sumarTitulo "Jinete") fremen 
        |otherwise = cambiarTolerancia (-(tolerancia fremen * 0.5)) fremen

--3 a
tuqui :: Number -> Number -> Bool
tuqui n1 n2 = n1 /= n2

realizacionMision :: Mision -> GusanoArena -> Tribu -> Tribu
realizacionMision mision gusano = map (mision gusano) 

nuevoElegido :: Tribu -> Mision -> GusanoArena -> Bool
nuevoElegido tribu mision gusano = 
    elegido tribu /= elegido (realizacionMision mision gusano tribu)

--4

{-
a) Al querer saber si hay algun candidato a ser elegido

En este caso en particular, como la función es "any", Haskell va a revisar
    la lista hasta encontrar alguno que cumpla la condicion. En cuanto lo haga
    dejara de evaluarla y no importará si es infinita o no. Esto en el caso de que
    alguno de los infinitos fremen que se encuentren pueda ser un 
    candidato a elegido, si no hubiera ninguno entonces Haskell se quedaría
    evaluando por siempre, solo pudiendo ser interrumpido con un control c. 
    No produciría ningun resultado y se quedaría evaluando. 

b) Al encontrar al elegido

    Como encontrarlo requiere de la función de orden superior "filter"
    que evalua la lista hasta el final, entonces nunca se podría llegar a un
    resultado, incluso si el primero cumple o si ninguno cumple. 
-}

--https://docs.google.com/document/d/1vfmY4xOaGVMtDCixKtA6rDugwWM86ZJaL7K5vDKcksA/edit?usp=sharing