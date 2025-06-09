{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use null" #-}


module Library where
import Data.List (sort)
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

------------- DATA ---------------
data Auto = UnAuto{
    tiempoCarrera :: Number,
    marca :: String,
    modelo :: String,
    desgasteChasis :: Number,
    desgasteRuedas :: Number,
    velocidadMax :: Number,
    apodo :: [String]
} deriving (Show, Ord, Eq)

type Tramo = Auto -> Auto

data Equipo = UnEquipo{
    nombre :: String,
    autos :: [Auto],
    presupuesto :: Number
} deriving Show

data Pista = UnaPista{
    nombrePista :: String,
    pais :: String,
    precioEntrada :: Number,
    recorrido :: [Tramo]
} deriving Show

data Carrera = UnaCarrera{
    pista :: Pista,
    vueltas :: Number
} deriving Show

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

------------------------------ PUNTO 2 ------------------------------
--------------------- ESTADO DE SALUD DEL AUTO ----------------------

------------- Saber si un auto ESTÁ EN BUEN ESTADO-------------
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis auto < 20
    | otherwise = desgasteChasis auto < 40 && desgasteRuedas auto < 60

---------- Saber si un auto NO DA MÁS ----------
autoNoDaMas :: Auto -> Bool
autoNoDaMas auto =
    (coincidePrimerApodo auto && desgasteChasis auto > 80) || desgasteRuedas auto > 80

primerApodo :: Auto -> String
primerApodo auto = head (apodo auto)

coincidePrimerApodo :: Auto -> Bool
coincidePrimerApodo auto = take 2 (primerApodo auto)  == "La"

---------- Saber si un auto ES UN CHICHE ----------
esUnChiche :: Auto -> Bool
esUnChiche auto
    | even (cantidadApodos auto) = desgasteChasis auto < 20
    | otherwise = desgasteChasis auto < 50

cantidadApodos :: Auto -> Number
cantidadApodos auto = length (apodo auto)

---------- Saber si un auto ES UNA JOYA ----------
esUnaJoya :: Auto -> Bool
esUnaJoya auto =
    desgasteChasis auto == 0 &&
    desgasteRuedas auto == 0 &&
    length (apodo auto) <= 1

---------- Conocer el NIVEL DE CHETEZ ----------
nivelDeChetez :: Auto -> Number
nivelDeChetez auto = 20 * cantidadApodos auto * caracteresModelo auto

caracteresModelo :: Auto -> Number
caracteresModelo auto = length (modelo auto)

---------- Determinar capacidad supercalifragilisticaespialidosa ----------
determinarCapacidad :: Auto -> Number
determinarCapacidad auto = length (primerApodo auto)

---------- Calcular qué tan RIESGOSO es un auto ----------
riesgoAuto :: Auto -> Number
riesgoAuto auto
    | not (estaEnBuenEstado auto) = riesgoBase auto * 2
    | otherwise = riesgoBase auto

riesgoBase :: Auto -> Number
riesgoBase auto = velocidadMax auto * (desgasteRuedas auto / 10)

------------------------------ PUNTO 3 ------------------------------
-------------------------- MANOS A LA OBRA --------------------------

---------------------------Reparar Auto------------------------------
porcentajeNecesario :: Number
porcentajeNecesario = 0.15

repararAuto :: Auto -> Auto
repararAuto auto = (bajarChasis porcentajeNecesario auto) {desgasteRuedas = 0} 

bajarChasis :: Number -> Auto -> Auto 
bajarChasis porcentajeNecesario auto = auto {desgasteChasis = porcentajeNecesario * desgasteChasis auto} 

----------------------------Aplicar una penalidad---------------------

aplicarPenalidad :: Number -> Auto -> Auto 
aplicarPenalidad penalidadTiempo auto  = auto {tiempoCarrera = tiempoCarrera auto + penalidadTiempo}

--------------------------Ponerle Nitro a un Auto---------------------
ponerleNitroAUnAuto :: Auto -> Auto
ponerleNitroAUnAuto auto = auto {velocidadMax = velocidadMax auto + porcentaje 20 auto}

porcentaje :: Number -> Auto -> Number
porcentaje porciento auto = (porciento * velocidadMax auto)/ 100 

--------------------------Bautizar un Auto---------------------------

bautizarAuto :: String -> Auto -> Auto 
bautizarAuto apodoNuevo auto = auto {apodo = apodoNuevo : apodo auto} 

------------------------Llevar un auto a un desarmadero--------------

llevarAutoDesarmadero :: Auto -> String -> String -> Auto
llevarAutoDesarmadero auto nuevamarca nuevomodelo = auto {marca = nuevamarca, modelo = nuevomodelo, apodo = ["Nunca Taxi"]}

------------------------------ PUNTO 4 ------------------------------
----------------------------- ¡PISTAS! ------------------------------

--------------------Curvas--------------------
crearCurva :: Number -> Number -> Tramo
crearCurva angulo longitud auto = auto {
    tiempoCarrera = tiempoCarrera auto + (longitud / (velocidadMax auto / 2)),
    desgasteRuedas = desgasteRuedas auto + (3 * (longitud / angulo))
}

curvaPeligrosa :: Tramo
curvaPeligrosa = crearCurva 60 300

curvaTranca :: Tramo
curvaTranca = crearCurva 110 550

--------------------Rectos--------------------
crearRecto :: Number -> Tramo
crearRecto longitud auto = auto {
    tiempoCarrera = tiempoCarrera auto + longitud / velocidadMax auto,
    desgasteChasis = desgasteChasis auto + 0.01 * longitud
}

tramoRectoClassic :: Tramo
tramoRectoClassic = crearRecto 715

tramito :: Tramo
tramito = crearRecto 260

--------------------ZigZag--------------------
crearZigZag :: Number -> Tramo
crearZigZag cambios auto = auto {
    tiempoCarrera = tiempoCarrera auto + cambios * 3,
    desgasteChasis = desgasteChasis auto + 5,
    desgasteRuedas = desgasteRuedas auto + velocidadMax auto * cambios / 10
}

zigZagLoco :: Tramo
zigZagLoco = crearZigZag 5

casiCurva :: Tramo
casiCurva = crearZigZag 1

--------------------Rulos--------------------
crearRulo :: Number -> Tramo
crearRulo diametro auto = auto {
    desgasteRuedas = desgasteRuedas auto + diametro * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro / velocidadMax auto
}

ruloClasico :: Tramo
ruloClasico = crearRulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = crearRulo 26

------------------------------ PUNTO 5 ------------------------------
-------------------------- NIVEL DE JOYEZ ---------------------------

---------Nivel de Joyez-------------------

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez autos = sum (map unidadesDeJoyez (filter esUnaJoya autos))

unidadesDeJoyez :: Auto -> Number
unidadesDeJoyez auto
        | tiempoCarrera auto < 50 = 1
        | otherwise = 2

--------Para entendidos-------------------

paraEntendidos :: [Auto] -> Bool
paraEntendidos autos = all todoOk autos

menorIgualA200SegEnCarrera :: Auto -> Bool
menorIgualA200SegEnCarrera auto = tiempoCarrera auto <=200

todoOk :: Auto -> Bool
todoOk auto = estaEnBuenEstado auto && menorIgualA200SegEnCarrera auto

---------------------------------------------------------------------
------------------------------ Segunda ------------------------------
------------------------------ Entrega ------------------------------
---------------------------------------------------------------------

--funciones credas para los tests
tieneFerrari :: Equipo -> Bool
tieneFerrari equipo = head (autos equipo) == ferrari

tieneFiat :: Equipo -> Bool
tieneFiat equipo = head (autos equipo) == fiat && presupuesto equipo == 6000

------------------------------ PUNTO 1 ------------------------------
---------------------- EQUIPOS DE COMPETICIÓN -----------------------

losAlocados :: Equipo
losAlocados = UnEquipo{nombre = "Los Alocados", autos = [ferrari, ferrari], presupuesto = 70000}

losTranquilos :: Equipo
losTranquilos = UnEquipo{nombre = "Los Tranquilos", autos = [peugeot], presupuesto = 50000}

losAmbiciosos :: Equipo
losAmbiciosos = UnEquipo{nombre = "Los Ambiciosos", autos = [], presupuesto = 70000}

---------- Agregar Auto a Equipo ----------

costoInscripcionPorMS :: Number
costoInscripcionPorMS = 1000

agregarAuto :: Equipo -> Auto ->Equipo
agregarAuto equipo auto
    | inscripcionDentroDelPresupuesto auto (presupuesto equipo) = equipo {autos = auto : autos equipo, presupuesto = presupuesto equipo - costoInscripcion auto} 
    | otherwise = equipo

inscripcionDentroDelPresupuesto :: Auto -> Number -> Bool
inscripcionDentroDelPresupuesto auto presupuestoActual = costoInscripcion auto <= presupuestoActual

costoInscripcion :: Auto -> Number
costoInscripcion auto = velocidadMax auto * costoInscripcionPorMS

---------- Reparar equipo, Optimizar equipo o Ferrarizar equipo ----------

losTops :: Equipo
losTops = UnEquipo{nombre="Los Tops", autos=[peugeot{desgasteChasis=10}, ferrari, lamborghini{desgasteChasis=20}], presupuesto= 20000}

unSueño :: Equipo
unSueño = UnEquipo{nombre="Un Sueño", autos=[fiat{desgasteChasis=50}, peugeot{desgasteChasis=0}], presupuesto= 10000}

muchosFiats :: Equipo
muchosFiats = UnEquipo{nombre="Muchos Fiats", autos=[fiat, fiat, fiat, ferrari, fiat, fiat], presupuesto= 20000}

ferrarizar :: Auto -> Auto
ferrarizar auto = llevarAutoDesarmadero auto "Ferrari" "F50"

---------------- COSTOS ----------------

costoReparacion :: Auto -> Number
costoReparacion auto = desgasteChasis auto * costoPorPuntoChasis * (1 - porcentajeNecesario)

costoPorPuntoChasis :: Number
costoPorPuntoChasis = 500

costoOptimizacion :: Auto -> Number
costoOptimizacion auto = velocidadMax auto * costoNitroPorPuntoVelocidad

costoNitroPorPuntoVelocidad :: Number
costoNitroPorPuntoVelocidad = 100

costoFerrarizar :: Auto -> Number
costoFerrarizar auto
    | marca auto == "Ferrari" = 0
    | otherwise = 3500

costoCambiosPrimerAuto :: String -> Equipo -> Number
costoCambiosPrimerAuto tarea equipo
    | tarea == "reparar" = costoReparacion (head (autos equipo))
    | tarea == "optimizar" = costoOptimizacion (head (autos equipo))
    | tarea == "ferrarizar" = costoFerrarizar (head (autos equipo))

---------------- CAMBIOS ----------------

cambiosAlEquipo :: String -> Equipo -> Equipo
cambiosAlEquipo tarea equipo
    | autos equipo == [] || primeroSinPresupuesto tarea equipo = equipo
    | otherwise = equipo {autos = primerAuto tarea equipo : autos (restoDelEquipo tarea equipo), presupuesto = presupuesto (restoDelEquipo tarea equipo)}

primerAuto :: String -> Equipo -> Auto
primerAuto tarea equipo = cambiosAlAuto tarea (head (autos equipo))

restoDelEquipo :: String -> Equipo -> Equipo
restoDelEquipo tarea equipo = cambiosAlEquipo tarea (equipo {autos = tail (autos equipo), presupuesto = nuevoPresupuesto tarea equipo})

nuevoPresupuesto :: String -> Equipo -> Number
nuevoPresupuesto tarea equipo = presupuesto equipo - costoCambiosPrimerAuto tarea equipo

---------------- CONDICIONES ----------------

primeroSinPresupuesto :: String -> Equipo -> Bool
primeroSinPresupuesto tarea equipo
    | tarea == "reparar" = presupuesto equipo < costoReparacion (head (autos equipo))
    | tarea == "optimizar" = presupuesto equipo < costoOptimizacion (head (autos equipo))
    | tarea == "ferrarizar" = presupuesto equipo < costoFerrarizar (head (autos equipo))

cambiosAlAuto :: String -> Auto -> Auto
cambiosAlAuto tarea auto
    | tarea == "reparar" = repararAuto auto
    | tarea == "optimizar" = ponerleNitroAUnAuto auto
    | tarea == "ferrarizar"  && not (esFerrari auto) = ferrarizar auto
    | otherwise = auto

esFerrari :: Auto -> Bool
esFerrari auto = marca auto == "Ferrari"

---------------- Funciones ----------------

repararEquipo :: Equipo -> Equipo
repararEquipo equipo = cambiosAlEquipo "reparar" equipo

optimizarEquipo :: Equipo -> Equipo
optimizarEquipo equipo = cambiosAlEquipo "optimizar" equipo

ferrarizarEquipo :: Equipo -> Equipo
ferrarizarEquipo equipo = cambiosAlEquipo "ferrarizar" equipo

------------------------------ PUNTO 2 ------------------------------
----------------------- COSTO REPARACIÓN TOTAL ----------------------

costoReparacionEquipo :: Equipo -> Number
costoReparacionEquipo equipo = sum (map costoReparacion (autos equipo))

------------------------------ PUNTO 3 ------------------------------
-------------------------- EQUIPO INFINIA ---------------------------

infinia :: Equipo
infinia = UnEquipo{nombre = "Infinia", autos = equipoInfinito ferrari 1, presupuesto = 5000}

equipoInfinito :: Auto -> Number -> [Auto]
equipoInfinito auto n = auto {desgasteChasis = 1, velocidadMax = velocidadMax auto *n} : equipoInfinito auto (n+1)

{-
    i. Reparar el equipo: para evitar repetición de lógica con Optimizar y Ferrarizar, se utiliza una única función
    llamada "cambiosAlEquipo" que engloba los tres casos posibles; por practicidad vamos a centrarnos en el proceso
    propio de la reparación. Por trabajar con métodos recursivos, al recibir un Equipo cuya lista de autos es infinita,
    esta función no va a mostrar ningún resultado, debido a su incapacidad de procesar toda la información de la lista.
    Sin embargo, al llamarse a la función "repararAuto" en la invocación, se evalúa el costo de reparar cada auto y se
    reparan la cantidad de autos que permita el presupuesto del equipo. Por ende, en el caso del Equipo Infinia se van
    a reparar sus primeros 11 autos, solo que no van a mostrarse por ser una lista infinita.

    ii. Optimización del equipo: este caso es similar al anterior al utilizarse "cambiosAlEquipo" y ser un Equipo cuya
    lista de autos es infinita. No obstante, en este caso llamar a la función "ponerleNitroAUnAuto" (optimizarlo) no va
    a tener efecto alguno en el Equipo Infinia, debido a que este se compone de infinitos ferraris, y para optimizar uno
    solo se necesita un presupuesto mínimo de 6500, contra los 5000 de este equipo. Entonces, en este caso ningún auto se
    podrá optimizar y, aunque alguno pudiera, los resultados no serían mostrados por ser una lista infinita.

    iii. Ferrarizar un equipo: nuevamente, en este caso se utiliza la función "cambiosAlEquipo" y, por ser un Equipo que
    contiene una lista infinita, no van a poder mostrarse sus resultados. Si bien al llamar a la función "ferrarizar" se
    recorrería la lista de autos de Infinia y se cambiarían tanto la marca como el modelo de los autos hasta que no haya
    presupuesto suficiente, en este caso todos los autos son Ferrari, por lo que su invocación no tendría sentido alguno
    al no suponer cambios ni en la lista de autos ni en el presupuesto del equipo.

    iv. costoReparacionEquipo: esta función no podrá mostrar resultado alguno, esto por recibir como entrada un Equipo
    cuya lista de autos es infinita. Por ende, cuando se intente aplicar la función "map costoReparacion", la evaluación
    de dicha función nunca va a completarse, haciendo imposible la evaluación de "sum" por requerir los resultados de
    "map costoReparacion (autos equipo)".
-}

------------------------------ PUNTO 4 ------------------------------
---------------------- MODIFICADORES DE TRAMOS -----------------------

-----------------------TRAMO CON BOXES--------------------------------

conBoxes :: Tramo -> Tramo
conBoxes tramo auto
    | not (estaEnBuenEstado (tramo auto)) = aplicarBoxes (tramo auto)
    | otherwise = tramo auto

aplicarBoxes :: Auto -> Auto
aplicarBoxes auto = repararAuto auto {tiempoCarrera = tiempoCarrera auto + 10}

rectoConBoxes :: Tramo -- Prueba
rectoConBoxes = conBoxes tramoRectoClassic

-----------------------TRAMO MOJADO--------------------------------------

mojado :: Tramo -> Tramo
mojado tramoOriginal = aplicarMojado . tramoOriginal

aplicarMojado :: Auto -> Auto
aplicarMojado auto = auto {tiempoCarrera = tiempoCarrera auto + tiempoCarrera auto * 0.5}

curvaMojada :: Tramo -- Prueba
curvaMojada = mojado curvaTranca

-----------------------------TRAMO CON RIPIO----------------------------------

conRipio :: Tramo -> Tramo
conRipio tramoOriginal = aplicarRipio . tramoOriginal

aplicarRipio :: Auto ->  Auto
aplicarRipio auto = auto {
    tiempoCarrera = tiempoCarrera auto *2,
    desgasteChasis = desgasteChasis auto *2,
    desgasteRuedas = desgasteRuedas auto *2
}

curvaConRipio :: Tramo -- Prueba
curvaConRipio = conRipio curvaTranca 

----------------------------TRAMO OBSTRUCCION-------------------------------

conObstruccion :: Number -> Tramo -> Tramo
conObstruccion metros tramo = aplicarObstruccion metros . tramo

aplicarObstruccion :: Number -> Auto -> Auto 
aplicarObstruccion mtsObstruccion auto = auto {desgasteRuedas = desgasteRuedas auto + 2 * mtsObstruccion}

rectoObstruido :: Tramo -- Prueba
rectoObstruido = conObstruccion 2 tramoRectoClassic

----------------------------TRAMO CON TURBO--------------------------------

conTurbo :: Tramo -> Tramo
conTurbo tramo = devolverVelocidad . tramo . aplicarTurbo

aplicarTurbo :: Auto -> Auto  
aplicarTurbo auto = (auto {velocidadMax = velocidadMax auto *2}) 

devolverVelocidad :: Auto -> Auto 
devolverVelocidad auto = auto {velocidadMax = velocidadMax auto / 2}

rectoConTurbo :: Tramo
rectoConTurbo = conTurbo tramoRectoClassic


------------------------------ PUNTO 5 ------------------------------

pasarPorTramo2 :: Auto -> Tramo -> Auto-- (Tramo -> Auto -> Auto)
pasarPorTramo2 auto tramo
    | autoNoDaMas auto = auto
    | otherwise = tramo auto

------------------------------ PUNTO 6 ------------------------------
------------------------ ATRAVESANDO PISTAS -------------------------

-------------- Vuelta a la Manzana --------------

vueltaALaManzana :: Pista
vueltaALaManzana = UnaPista{nombrePista = "La Manzana", pais = "Italia", precioEntrada = 30, recorrido = [
    rectoManzana, curvaManzana, rectoManzana, curvaManzana, rectoManzana, curvaManzana, rectoManzana, curvaManzana]}

rectoManzana :: Tramo
rectoManzana = crearRecto 130

curvaManzana :: Tramo
curvaManzana = crearCurva 90 13

-------------- Super Pista --------------

superPista :: Pista
superPista = UnaPista{nombrePista = "Super Pista", pais = "Argentina", precioEntrada = 300, recorrido = recorridoSuperPista}

recorridoSuperPista :: [Tramo]
recorridoSuperPista = [tramoRectoClassic, curvaTranca, conTurbo tramito, mojado tramito, crearRulo 10, conObstruccion 2 (crearCurva 80 400), crearCurva 115 650, crearRecto 970,
    curvaPeligrosa, conRipio tramito, conBoxes (crearRecto 800), conObstruccion 5 casiCurva, crearZigZag 2, mojado (conRipio deseoDeMuerte), ruloClasico, zigZagLoco]

--------------- PeganLaVuelta/2 ---------------

peganLaVuelta2 :: Pista -> [Auto] -> [Auto]
peganLaVuelta2 pista conjuntoAutos = map (recorrerUnRecorrido (recorrido pista)) conjuntoAutos

recorrerUnRecorrido :: [Tramo] -> Tramo
recorrerUnRecorrido conjuntoDeTramos auto = foldl pasarPorTramo2 auto conjuntoDeTramos

------------------------------ PUNTO 7 ------------------------------
----------------------- TOUR DE BUENOS AIRES ------------------------

tourDeItalia :: Carrera
tourDeItalia = UnaCarrera { pista=vueltaALaManzana, vueltas=10 }

tourBuenosAires :: Carrera  
tourBuenosAires = UnaCarrera { pista=superPista, vueltas=20 }

sacarNoDaMas :: [Auto] -> [Auto]
sacarNoDaMas autos = filter (not . autoNoDaMas) autos

correrCarrera :: Carrera -> [Auto] -> [Auto]
correrCarrera carrera conjuntoAutos
    | vueltas carrera == 0 || conjuntoAutos == [] = conjuntoAutos
    | otherwise = correrCarrera (carrera{vueltas = vueltas carrera - 1}) (peganLaVuelta2 (pista carrera) (sacarNoDaMas conjuntoAutos))

tiempoParcialPrimero :: Carrera -> Number -> [Auto] -> Number
tiempoParcialPrimero carrera vuelta conjuntoAuto = tiempoCarrera(head (resultadosCarrera carrera{vueltas = vuelta} conjuntoAuto))

cantidadAutosQueTerminaron ::  Carrera -> [Auto] -> Number
cantidadAutosQueTerminaron carrera autos = length (correrCarrera carrera autos)

resultadosCarrera :: Carrera -> [Auto] -> [Auto]
resultadosCarrera carrera autos = sort (correrCarrera carrera autos)

darGanador :: Carrera -> [Auto] -> Auto
darGanador carrera autos = (head . resultadosCarrera carrera) autos

tiempoTotalSegundo :: Carrera -> [Auto] -> Number
tiempoTotalSegundo carrera autos = tiempoCarrera (resultadosCarrera carrera autos !! 1)

{- FUNCIONES PASADAS
--darGanador :: Carrera -> [Auto] -> Auto
--darGanador carrera autos = foldl1 autoDeMejorTiempo (correrCarrera carrera autos)

autoDeMejorTiempo :: Auto -> Auto -> Auto
autoDeMejorTiempo primerAuto segundoAuto
    | tiempoCarrera primerAuto > tiempoCarrera segundoAuto = segundoAuto
    | otherwise = primerAuto

tiempoTotalSegundo :: Carrera -> [Auto] -> Number
tiempoTotalSegundo carrera autos = tiempoCarrera (correrCarrera carrera autos!!1)
-}

{-
costoReparacionPorPuntoChasis :: Number
costoReparacionPorPuntoChasis = 500

repararEquipo :: Equipo -> Equipo
repararEquipo equipo
    | null (autos equipo) = equipo
    | primeroFueraDePresupuestoReparacion equipo = equipo
    | otherwise = equipo{autos = repararAuto(head(autos equipo)) : autos (siguienteReparacionEquipo equipo), presupuesto = presupuesto (siguienteReparacionEquipo equipo)}

siguienteReparacionEquipo :: Equipo -> Equipo
siguienteReparacionEquipo equipo = repararEquipo(equipo{autos= tail(autos equipo), presupuesto = presupuesto equipo - costoReparacionPrimerAutoEquipo equipo })

primeroFueraDePresupuestoReparacion :: Equipo -> Bool
primeroFueraDePresupuestoReparacion equipo = presupuesto equipo < costoReparacionPrimerAutoEquipo equipo

costoReparacionPrimerAutoEquipo :: Equipo -> Number
costoReparacionPrimerAutoEquipo =  costoReparacion . head . autos

costoReparacion :: Auto -> Number
costoReparacion auto = desgasteChasis auto * (1 - porcentajeNecesario) * costoReparacionPorPuntoChasis

---------- Optimizar autos de equipo ----------

costoNitroPorPuntoVelocidad :: Number
costoNitroPorPuntoVelocidad = 100

optimizarEquipo :: Equipo -> Equipo
optimizarEquipo equipo
    | null (autos equipo) = equipo
    | primeroFueraDePresupuestoOptimizacion equipo = equipo
    | otherwise = equipo{autos = ponerleNitroAUnAuto(head(autos equipo)) : autos (siguienteOptimizacionEquipo equipo), presupuesto = presupuesto (siguienteOptimizacionEquipo equipo)}

siguienteOptimizacionEquipo :: Equipo -> Equipo
siguienteOptimizacionEquipo equipo = optimizarEquipo(equipo{autos= tail(autos equipo), presupuesto = presupuesto equipo - costoOptimizacionPrimerAutoEquipo equipo })

primeroFueraDePresupuestoOptimizacion :: Equipo -> Bool
primeroFueraDePresupuestoOptimizacion equipo = presupuesto equipo < costoOptimizacionPrimerAutoEquipo equipo

costoOptimizacionPrimerAutoEquipo :: Equipo -> Number
costoOptimizacionPrimerAutoEquipo =  costoOptimizacion . head . autos

costoOptimizacion :: Auto -> Number
costoOptimizacion auto = velocidadMax auto * costoNitroPorPuntoVelocidad

---- FERRARIZAR EQUIPO ----

ferrarizarEquipo :: Equipo -> Equipo
ferrarizarEquipo equipo
    | null (autos equipo) || presupuesto equipo < costoFerrarizar = equipo
    | primeroFerrari(autos equipo)= equipo{autos = head(autos equipo) : autos(siguienteSiYaFerrari equipo), presupuesto = presupuesto (siguienteSiYaFerrari equipo)}
    | otherwise = equipo{autos = ferrarizar( head(autos equipo) ) : autos (siguienteFerrarizarEquipo equipo), presupuesto = presupuesto (siguienteFerrarizarEquipo equipo)}

ferrarizar :: Auto -> Auto
ferrarizar auto = llevarAutoDesarmadero auto "Ferrari" "F50"

primeroFerrari :: [Auto] -> Bool
primeroFerrari conjuntoAutos = marca (head conjuntoAutos) == "Ferrari"

siguienteSiYaFerrari :: Equipo -> Equipo
siguienteSiYaFerrari equipo = ferrarizarEquipo( equipo{autos=tail(autos equipo)})

siguienteFerrarizarEquipo :: Equipo -> Equipo
siguienteFerrarizarEquipo equipo = ferrarizarEquipo(equipo{autos= tail(autos equipo), presupuesto = presupuesto equipo - costoFerrarizar })

-}