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

data Tramo = UnTramo{
    angulo :: Number,
    longitud :: Number,
    cambios :: Number,
    diametro :: Number
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
curvaPeligrosa :: Tramo
curvaPeligrosa = UnTramo {angulo = 60, longitud = 300, cambios = 0, diametro = 0}

curvaTranca :: Tramo
curvaTranca = UnTramo {angulo = 110, longitud = 550, cambios = 0, diametro = 0}

transitarCurva :: Tramo -> Auto -> Auto
transitarCurva tramo auto = auto {
    desgasteRuedas = desgasteRuedas auto + 3 * longitud tramo / angulo tramo,
    tiempoCarrera = tiempoCarrera auto + longitud tramo / (velocidadMax auto / 2)
}

--------------------Rectos--------------------
tramoRectoClassic :: Tramo
tramoRectoClassic = UnTramo {angulo = 0, longitud = 715, cambios = 0, diametro = 0}

tramito :: Tramo
tramito = UnTramo {angulo = 0, longitud = 260, cambios = 0, diametro = 0}

transitarRecto :: Tramo -> Auto -> Auto
transitarRecto tramo auto = auto {
    tiempoCarrera = tiempoCarrera auto + longitud tramo / velocidadMax auto,
    desgasteChasis = desgasteChasis auto + 0.01 * longitud tramo
}

--------------------ZigZag--------------------
zigZagLoco :: Tramo 
zigZagLoco = UnTramo {angulo = 0, longitud = 0, cambios = 5, diametro = 0}

casiCurva :: Tramo 
casiCurva = UnTramo {angulo = 0, longitud = 0, cambios = 1, diametro = 0}

transitarZigZag :: Tramo -> Auto -> Auto
transitarZigZag tramo auto = auto {
    tiempoCarrera = tiempoCarrera auto + cambios tramo * 3,
    desgasteChasis = desgasteChasis auto + 5,
    desgasteRuedas = desgasteRuedas auto + velocidadMax auto * cambios tramo / 10
}

--------------------Rulos--------------------
ruloClasico :: Tramo
ruloClasico  = UnTramo {angulo = 0, longitud = 0, cambios = 0, diametro = 13}

deseoDeMuerte :: Tramo
deseoDeMuerte  = UnTramo {angulo = 0, longitud = 0, cambios = 0, diametro = 26}

transitarRulo :: Tramo -> Auto -> Auto
transitarRulo tramo auto = auto {
    desgasteRuedas = desgasteRuedas auto + diametro tramo * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro tramo / velocidadMax auto
}


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

------------------------------ PUNTO 1 ------------------------------
---------------------- EQUIPOS DE COMPETICIÓN -----------------------

data Equipo = UnEquipo{
    nombre :: String,
    autos :: [Auto],
    presupuesto :: Number
} deriving Show

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

---------- Realizar una reparación de equipo ----------
{-
(No queda claro si va reparando uno por uno hasta que ya no se lo permite el presupuesto o si 
revisa el coste de todos de una y recíen ahí decide si hacerlo o no)
(Por ahora asumimos la segunda opción)
-}
losTops :: Equipo
losTops = UnEquipo{nombre="Los Tops", autos=[ferrari{desgasteChasis=10}, lamborghini{desgasteChasis=20}], presupuesto= 20000}

unSueño :: Equipo
unSueño = UnEquipo{nombre="Un Sueño", autos=[fiat{desgasteChasis=50}], presupuesto= 10000}

costoReparacionPorPuntoChasis :: Number
costoReparacionPorPuntoChasis = 500

repararEquipo :: Equipo -> Equipo
repararEquipo equipo
    | costoReparacionAutos (autos equipo) <= presupuesto equipo = equipo{autos = map repararAuto (autos equipo), presupuesto = presupuesto equipo - costoReparacionAutos (autos equipo)}
    | otherwise = equipo

costoReparacionAutos :: [Auto] -> Number
costoReparacionAutos grupoAutos = sum( map costoReparacion grupoAutos)

costoReparacion :: Auto -> Number
costoReparacion auto = desgasteChasis auto * (1 - porcentajeNecesario) * costoReparacionPorPuntoChasis

------------------------------ PUNTO 3 ------------------------------
-------------------------- EQUIPO INFINIA ---------------------------

infinia :: Equipo
infinia = UnEquipo{nombre = "Infinia", autos = take 3 (equipoInfinito ferrari 1), presupuesto = 5000}

equipoInfinito :: Auto -> Number -> [Auto]
equipoInfinito auto n = auto {desgasteChasis = 1, velocidadMax = velocidadMax auto *n} : equipoInfinito auto (n+1)

{-
    i. Reparación de Equipo.

    ii. Optimización.

    iii. Ferrarizar.

    iv. Costo de reparación.
-}

{- ENTREGA 2

1. a) hacer un data con la info de los equipos, modelar 2 equipos, uno con 70k de presupuesto, otro con 50k y un peugeot
    agregar auto ::  auto ->  equipo
    

   b) reparacion en equipo :: recibe equipo -> devuelve equipo
    hay plata -> se repara el 85% de c/auto (mientras haya presupuesto)(explora de forma recursiva la lista?)
    c/punto de desgasteChasis = $500


   c) optimizar autos ::  equipo ->  equipo
   otra cosa recursiva que vaya auto por auto poniendo nitro hasta quedarse sin plata.
   El costo de poner nitro a un auto es de la velocidad máxima del auto (antes de poner nitro) * $100

   d) ferrarizar ::  equipo ->  equipo
   va de auto a auto convirtiendolos en ferraris y F50 hasta quedarse sin plata.
   Si un auto del equipo ya era Ferrari, queda igual que como estaba y no hay costo para ese auto.
   El costo de convertir un auto en una Ferrari es de $3500

2. Costo total reparacion :: equipo -> number

3. funcion que haga el equipo infinito

4. a) agregar boxes a Tramo
    boxes :: auto -> auto
    auto entra a boxes si NO estaEnBuenEstado
    si entra a boxes -> se repara -> agregar 10 segundos adicionales a tiempoCarrera

   b) agregar mojado
    suma 50% mas del tiempo de carrera que produce originalmente
   
   c) agregar ripio
   produce el doble de efecto y se tarda el doble en atravesarlo.
   No hay boxes con ripio

   d) agregar obstruccion
   2 puntos de desgaste en las ruedas por metro que este obstruido

   e) agregar turbo
   doble de velocidad maxima, luego de atravesar el tramo, se pierde el turbo

5.  pasarPorTramo/2 :: tramo -> auto -> auto
Va pasando por los tramos, pero primero analiza si su durabilidad se lo permite (con funcion "noDaMas").
Si no puede, no le afecta.

6. definir las pistas vueltaALaManzana, superPista y la funcion peganLaVuelta/2 (Pista -> [auto] -> [auto]?)

esto ya esta definido arriba de todo, pero esta incompleto. poner esta

data Pista = UnaPista{
    nombre :: String,
    pais :: String,
    precioEntrada :: Number,
    recorrido :: [Tramo]
} deriving Show


7.
data Carrera = unaCarrera{
    pista :: Pista,
    vueltas :: Number
} deriving Show
hacer una funcion "correrCarrera" que reciba una carrera y lista de autos o equipos y que simule la carrera.


-----

data Tramo = UnTramo{
    angulo :: Number,
    longitud :: Number,
    cambios :: Number,
    diametro :: Number,

    -- Lo nuevo
    boxes :: Bool,
    ripio :: Bool,
    mojado :: Bool,
    obstruccion :: Bool,
    turbo :: Bool
} deriving Show




-------------------
Un prototipo feo
-------------------
repararEquipo :: Equipo -> Equipo
repararEquipo equipo
    | null (autos equipo)= equipo
    | otherwise = repararEquipo(equipo{autos=})

intentarRepararPrimero :: Equipo -> Equipo
intentarRepararPrimero equipo
    | primeroFueraDePresupuestoReparacion equipo = equipo
    | otherwise = equipo{}

primeroFueraDePresupuestoReparacion :: Equipo -> Bool
primeroFueraDePresupuestoReparacion equipo = presupuesto equipo < costoReparacion(head(autos equipo))
-}