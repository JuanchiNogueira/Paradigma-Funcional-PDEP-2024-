{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
module ParcialPlagio where
import PdePreludat


--Datas
data Obra = UnaObra {contenido :: String,
                    fecha :: Number}

data Autor = UnAutor {nombre :: String,
                      obras :: [Obra]
                     }

--Modelos

obraA :: Obra
obraA = UnaObra "Había una vez un pato." 1997

obraB :: Obra
obraB = UnaObra "¡Habia una vez un pato!" 1996

obraC :: Obra
obraC = UnaObra "Mirtha , Susana y Moria." 2010

obraD :: Obra
obraD = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

obraE :: Obra
obraE = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

autor1 :: Autor
autor1 = UnAutor "nn1" [obraA]
autor2 :: Autor
autor2 = UnAutor "nn2" [obraB, obraC]
autor3 :: Autor
autor3 = UnAutor "nn3" [obraB, obraD]
autor4 :: Autor
autor4 = UnAutor "nn4" [obraE, obraB]
--Punto 2 

versionCruda :: String -> String
versionCruda = filter esLetraONumero.map sinAcento

esLetraONumero :: Char ->  Bool
esLetraONumero caracter = elem caracter todasLasLetrasYNumeros

todasLasLetrasYNumeros :: [Char]
todasLasLetrasYNumeros = ['a'..'z']++['A'..'Z'] ++ "0123456789 "

sinAcento :: Char ->  Char
sinAcento 'á' = 'a'
sinAcento 'é' = 'e'
sinAcento 'í' = 'i'
sinAcento 'ó' = 'o'
sinAcento 'ú' = 'u'
sinAcento 'Á' = 'A'
sinAcento 'É' = 'E'
sinAcento 'Í' = 'I'
sinAcento 'Ó' = 'O'
sinAcento 'Ú' = 'U'
sinAcento letra = letra

--3 Plagios

type Plagio = String -> String -> Bool

copiaLiteral :: Plagio
copiaLiteral plagio original = versionCruda original == versionCruda plagio

empiezaIgual :: Number -> Plagio
empiezaIgual n plagio original = take n original == take n plagio && length plagio < length original

leAgregaronIntro :: Plagio
leAgregaronIntro plagio original = ultimosElemento (length original) plagio == original

ultimosElemento :: Number -> String -> String
ultimosElemento n plagio = drop (length plagio - n) plagio

mismaLongitud :: Plagio 
mismaLongitud = (\original plagio -> length original == length plagio)

--Punto 4

data Bot = UnBot {formasDePlagio :: [Plagio],
                  fabricante :: String
                  }deriving (Show)

alexa ::  Bot 
alexa  = UnBot [copiaLiteral, empiezaIgual 10] "Microsoft"

copilot :: Bot 
copilot = UnBot [mismaLongitud , leAgregaronIntro ] "Chat GPT"

-- Punto 5

deteccion :: Obra -> Obra -> Plagio -> Bool
deteccion obra obraOriginal forma = fecha obra > fecha obraOriginal && forma (contenido obra) (contenido obraOriginal)  

esPlagioDeEstaObra :: Bot -> Obra -> Obra -> Bool
esPlagioDeEstaObra bot obra obraOriginal = any (deteccion obra obraOriginal) (formasDePlagio bot)

-- Punto 6

cadenaPlagiadores :: Bot ->  [Autor] -> Bool
cadenaPlagiadores bot [ _] = False
cadenaPlagiadores bot [x1,x2] = plagioA bot x1 x2
cadenaPlagiadores bot (x1:x2:xs) = plagioA bot x1 x2 && cadenaPlagiadores bot (x2:xs)

plagioA :: Bot ->  Autor ->  Autor -> Bool
plagioA bot autor autorOriginal = any (esPlagioDeEsteAutor bot autorOriginal) (obras autor)

esPlagioDeEsteAutor :: Bot -> Autor ->  Obra -> Bool
esPlagioDeEsteAutor bot autorOriginal obra = any (esPlagioDeEstaObra bot obra) (obras autorOriginal)

--Punto 7

aprendio :: Bot -> Autor -> [Autor] -> Bool
aprendio bot autor autores =  length (obrasPlagiadasDelAutor bot autor autores) == 1

obrasPlagiadasDelAutor :: Bot -> Autor -> [Autor] -> [Obra]
obrasPlagiadasDelAutor bot autor autores =  filter (esPlagioDeAlgunoDeEstosAutores bot autores) (obras autor) 

esPlagioDeAlgunoDeEstosAutores :: Bot -> [Autor] -> Obra -> Bool
esPlagioDeAlgunoDeEstosAutores  bot autores obra = any (\autor -> esPlagioDeEsteAutor bot autor obra) autores

--8---------------------------------------------------
obraInfinita :: Obra
obraInfinita = UnaObra (repeat 'a') 2021

--9---------------------------------------------------
obraInfinita2 :: Obra
obraInfinita2 = UnaObra (repeat 'a') 2025

{-

Suponiendo una consulta como: deteccion obraA obraInfinita copiaLiteral
No importa cuál sea la forma de detección, como la fecha de la obra que se pregunta si es plagio es anterior, no se cumple y corta diciendo False, por Lazy evaluation no es necesario seguir evaluando el otro lado del &&.

Ahora veamos los casos donde se cumple que la fecha es posterior:

copiaLiteral:
- Suponiendo la consulta: deteccion obraInfinita obraA copiaLiteral
da False, por Lazy Evaluation. Al evaluar la igualdad de contenidos no necesita evaluar toda la lista infinita para saber que los strings son distintos, con los primeros caracteres alcanza.
- Pero si consulto deteccion obraInfinita2 obraInfinita copiaLiteral, se cuelga, porque para saber si dos strings iguales son iguales necesita recorrerlos todos, aún con lazy evaluation.

empiezaIgual:
- Suponiendo la consulta: deteccion obraInfinita obraA empiezaIgual
Entonces da False, pues verifica con take que sean iguales los contenidos y eso da false. Por Lazy evaluation no es necesario evaluar la lista infinita para el take.
- Suponiendo la consulta: deteccion obraInfinita2 obraInfinita empiezaIgual
Ahí se cuelga, porque nunca llega a comparar las longitudes de los contenidos, aún con lazy evaluation. Es decir, aún si una es infinita y la otra empieza igual, jamás cortará.

leAgregaronIntro:
- Suponiendo la consulta: deteccion obraInfinita obraA leAgregaronIntro
Aún teniendo lazy evaluation, para el calcular el length del contenido de la obra infinita se cuelga, antes de poder hacer el drop.
- Ahora, si hacemos al revés: deteccion obraA obraInfinita leAgregaronIntro
Se colgaría, pues se pide hacer un ultimosElementos, que a su vez necesita el length de la lista infinita, no hay Lazy evaluation que lo salve.

-}


--https://docs.google.com/document/d/1ilESbsH_umXHRznhN0SOOhzjaR7f52qYOy677lczu7s/edit
