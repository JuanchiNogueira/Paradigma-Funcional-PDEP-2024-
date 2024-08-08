module MinecraftParcial where
import PdePreludat
import GHC.Num (Num)

type Material = String

data Jugador =UnJugador{nombre :: String,
                        puntaje :: Number,
                        inventario :: [Material]
                        }deriving Show

data Receta = UnaReceta{producto :: Material,
                        materiales :: [Material],
                        tiempo :: Number
                    }deriving(Show)

fogata :: Receta
fogata = UnaReceta "Fogata" ["Madera","Fosforo"] 10

polloAsado :: Receta
polloAsado = UnaReceta "Pollo asado" ["Fogata" , "Pollo"] 300

sueter :: Receta
sueter = UnaReceta "Sueter" ["Lana" , "Agujas" , "Tintura"] 600

alteraInventario :: Jugador -> Jugador
alteraInventario jug = jug{puntaje = puntaje jug - 100}

tieneMateriales :: Receta -> Jugador -> Bool
tieneMateriales receta jug = all (`elem` inventario jug) (materiales receta)

quitarMateriales :: Receta -> Jugador -> Jugador
quitarMateriales receta jug = jug{inventario = foldl quitarMaterialesAux (inventario jug) (materiales receta)}

quitarMaterialesAux :: [Material] -> Material -> [Material]
quitarMaterialesAux [] _ = []
quitarMaterialesAux (x:xs) material 
    |x == material = xs
    |otherwise = x : quitarMaterialesAux xs material

sumarPuntos :: Receta -> Jugador -> Jugador
sumarPuntos receta jug = jug {puntaje = puntaje jug + tiempo receta * 10 }

sumarObjeto :: Receta -> Jugador -> Jugador
sumarObjeto receta jug = jug {inventario = inventario jug ++ [producto receta]}

craftear :: Receta -> Jugador -> Jugador
craftear receta jug 
    |tieneMateriales receta jug = sumarObjeto receta (quitarMateriales receta (sumarPuntos receta jug))
    |otherwise = alteraInventario jug

duplicaPuntaje :: Receta -> Jugador -> Bool
duplicaPuntaje receta personaje = puntaje (sumarPuntos receta personaje) >= puntaje personaje*2 

objetosDuplican :: [Receta] -> Jugador -> [Receta]
objetosDuplican [] _ = []
objetosDuplican (x:xs) jug 
        |tieneMateriales x jug && duplicaPuntaje x jug = x : objetosDuplican xs jug
        |otherwise = objetosDuplican xs jug

crafteoSucesivo :: [Receta] -> Jugador -> Jugador
crafteoSucesivo recetas personaje = foldl (flip craftear) personaje recetas

data Bioma =UnBioma{nombreBioma :: String,
                    condicionMinado :: Material,
                    elementos :: [Material]}

artico :: Bioma
artico =UnBioma "Artico"  "Sueter" ["Hielo", "Iglues", "Lobos"]

type Herramienta = Bioma -> Material

puedeMinar :: [Material] -> Material -> Bool
puedeMinar materiales materialRequerido = materialRequerido `elem` materiales


hacha :: Herramienta
hacha bioma = last (elementos bioma)

espada :: Herramienta
espada bioma = head (elementos bioma)

pico :: Number -> Herramienta
pico posicion bioma = obtenerMaterial posicion (elementos bioma)

obtenerMaterial :: Number -> [Material] -> Material
obtenerMaterial posicion materiales = materiales !! posicion

agregarMaterial :: Material -> Jugador -> Jugador
agregarMaterial material jugador = jugador {inventario = inventario jugador ++ [material]}

cambiarPuntos :: Number -> Jugador -> Jugador
cambiarPuntos cambio jug = jug{puntaje = puntaje jug + cambio}

minar :: Herramienta -> Jugador -> Bioma -> Jugador
minar herramienta jug bioma 
    |puedeMinar (inventario jug) (condicionMinado bioma) = (cambiarPuntos 50.agregarMaterial(herramienta bioma)) jug
    |otherwise = jug

azada :: Herramienta
azada bioma = obtenerMaterial (length (elementos bioma) `div` 2) (elementos bioma)

--https://docs.google.com/document/d/1i9rB5AzRswz_0Z4T1v5IgRhC3UT-d_Ib1K7LUeq5sa0/edit