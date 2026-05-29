module Library where
import PdePreludat

--Datas
data Participante = UnParticipante {
    nombre :: String,
    listaTrucos :: [Trucos],
    especialidad :: Platos
} deriving (Show)

type Trucos = Platos -> Platos
type Ingredientes = [(String, Number)] -- Nombre de Ingrediente, Peso

data Platos = UnPlato {
    dificultad :: Number,
    listaComponentes :: Ingredientes
} deriving (Show, Eq)

--Funciones Apoyo
ingredienteNombre :: String -> (String, Number) -> Bool
ingredienteNombre unNombre (nombre, _) = nombre == unNombre
agregarPeso :: String -> Number -> (String, Number) -> (String, Number)
agregarPeso unIngrediente unPeso (nombre, peso)
    | ingredienteNombre unIngrediente (nombre, peso) = (nombre, peso + unPeso)
    | otherwise = (nombre, peso)
agregarAPlato :: String -> Number -> Platos -> Platos
agregarAPlato unIngrediente unPeso unPlato 
    |any (ingredienteNombre unIngrediente) (listaComponentes unPlato) = unPlato{listaComponentes = map (agregarPeso unIngrediente unPeso)(listaComponentes unPlato)}
    |otherwise = unPlato{listaComponentes = (unIngrediente, unPeso) : listaComponentes unPlato} 
filtrarIngredientes :: (Number -> Bool) -> Ingredientes -> Ingredientes
filtrarIngredientes unaCondicion unosIngredientes = filter(\(_, x) -> unaCondicion x) unosIngredientes --siendo x el peso
tieneIngredientes :: [String] -> Platos -> Bool
tieneIngredientes unosIngredientes unPlato = any (\(x, _) -> elem x unosIngredientes)(listaComponentes unPlato) --siendo x el nombre
aplicarTruco :: Platos -> Trucos -> Platos
aplicarTruco unPlato unTruco = unTruco unPlato -- se va acumulando el plato con los trucos despues con el foldl
sumaPesos :: Platos -> Number
sumaPesos unPlato = sum (map snd(listaComponentes unPlato))

--Trucos Parte A
endulzar :: Number -> Platos -> Platos
endulzar unPeso unPlato = agregarAPlato "Azúcar" unPeso unPlato
salar :: Number -> Platos -> Platos
salar unPeso unPlato = agregarAPlato "Sal" unPeso unPlato
darSabor :: Number -> Number -> Platos -> Platos
darSabor unPesoAzucar unPesoSal unPlato = (endulzar unPesoAzucar).(salar unPesoSal) $ unPlato
duplicarPorcion :: Platos -> Platos
duplicarPorcion unPlato = unPlato{listaComponentes = map (\(x, y) -> (x, y * 2)) (listaComponentes unPlato)} --siendo x el nombre, y el peso
simplificar :: Platos -> Platos
simplificar unPlato
    | esComplejo unPlato = unPlato{dificultad = 5, listaComponentes = filtrarIngredientes(>=10) (listaComponentes unPlato)}
    | otherwise = unPlato

-- Sobre los platos Parte A
esVegano :: Platos -> Bool
esVegano unPlato = not (tieneIngredientes ["Carne", "Huevo", "Leche", "Queso"] unPlato)
esSinTacc :: Platos -> Bool
esSinTacc unPlato = not (tieneIngredientes ["Harina"] unPlato)
esComplejo :: Platos -> Bool
esComplejo unPlato = (dificultad unPlato > 7) && (length (listaComponentes unPlato) > 5)
noAptoHipertension :: Platos -> Bool
noAptoHipertension unPlato = any(\(x, y) -> x == "Sal" && y > 2)(listaComponentes unPlato) --siendo x el nombre, y el peso

-- Modelado Parte B
pepeRonccino :: Participante
pepeRonccino = UnParticipante{
    nombre = "Pepe Ronccino",
    listaTrucos = [(darSabor 2 5),(simplificar),(duplicarPorcion)],
    especialidad = UnPlato {
        dificultad = 10,
        listaComponentes = [("Sal", 5),
        ("Ingrediente2", 1),
        ("Ingrediente3", 2), 
        ("Ingrediente4", 3),
        ("Ingrediente5", 4),
        ("Ingrediente6", 5)]
    }
}

-- Funcionalidades Parte C
cocinar :: Participante -> Platos
cocinar unParticipante = foldl aplicarTruco (especialidad unParticipante) (listaTrucos unParticipante)
esMejorQue :: Platos -> Platos -> Bool
esMejorQue unPlato otroPlato = ((dificultad unPlato) > (dificultad otroPlato)) && ((sumaPesos unPlato) < (sumaPesos otroPlato))
participanteEstrella :: [Participante] -> Participante
participanteEstrella listaParticipantes
    |length listaParticipantes == 1 = head listaParticipantes
    |esMejorQue (cocinar (head listaParticipantes))(cocinar (participanteEstrella (tail listaParticipantes))) = head listaParticipantes
    |otherwise = participanteEstrella (tail listaParticipantes)
-- toma al primero (head) y pregunta de los demas (tail) cual es el mejor, no sigue hasta que contesta
-- cuando contesta, compara el primero con ese ganador, entonces si da true en el primero, head es el nuevo campeon
-- si da falso, entonces tail es el campeon, que se sigue evaluando hasta dar al ganador