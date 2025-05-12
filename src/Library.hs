module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

------------- DATA ---------------
data Auto = UnAuto{
    marca :: String,
    modelo :: String,
    desgasteChasis :: Number,
    desgasteRuedas :: Number,
    velocidadMax :: Number,
    tiempoCarrera :: Number,
    apodo :: [String]
} deriving Show

data Pista = UnaPista{
    
}

----------- ENTREGA 1 ----------------------
--------------------------------------------


---------- MODELACIÓN DE AUTOS ------------
ferrari :: Auto
ferrari = UnAuto{marca = "Ferrari", modelo = "F50", desgasteChasis = 0, desgasteRuedas = 0, velocidadMax = 65, apodo = ["La nave", "El fierro", "Ferrucho"], tiempoCarrera = 0}

lamborghini :: Auto
lamborghini = UnAuto{marca = "Lamborghini", modelo = "Diablo", desgasteChasis = 7, desgasteRuedas = 4, velocidadMax = 73, apodo = ["Lambo", "La bestia"], tiempoCarrera = 0}

fiat :: Auto
fiat = UnAuto{marca = "Fiat", modelo = "600", desgasteChasis = 33, desgasteRuedas = 27, velocidadMax = 44, apodo = ["La Bocha", "La bolita", "Fitito"], tiempoCarrera = 0}

peugeot :: Auto
peugeot = UnAuto{marca = "Peugeot", modelo = "504", desgasteChasis = 0, desgasteRuedas = 0, velocidadMax = 40, apodo = ["El rey del desierto"], tiempoCarrera = 0}

--------- ESTADO DE SALUD DEL AUTO -------------
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis auto < 20
    | otherwise = desgasteChasis auto < 40 && desgasteRuedas auto < 60

-- CASOS DE PRUEBAS --
pruebaPeugeot :: Auto
pruebaPeugeot = UnAuto{}

pruebaLamborghini :: Auto
pruebaLamborghini = UnAuto{}

pruebaFiat :: Auto
pruebaFiat = UnAuto{}

pruebaFerrari1 :: Auto
pruebaFerrari1 = UnAuto{}

pruebaFerrari2 :: Auto
pruebaFerrari2 = UnAuto{}

pruebaFerrari3 :: Auto
pruebaFerrari3 = UnAuto{}
-- Esto habría que hablarlo bien, yo (Tomás) no logro entender del todo estos casos de pruebas