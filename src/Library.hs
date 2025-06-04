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
    diametro :: Number,
    condiciones:: Condiciones 
} deriving Show

data Condiciones = Condiciones{
    boxes :: Bool,
    ripio :: Bool,
    mojado :: Bool,
    obstruccion :: Number, 
    turbo :: Bool
}deriving Show

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
losTops :: Equipo
losTops = UnEquipo{nombre="Los Tops", autos=[ferrari{desgasteChasis=10}, lamborghini{desgasteChasis=20}], presupuesto= 20000}

unSueño :: Equipo
unSueño = UnEquipo{nombre="Un Sueño", autos=[fiat{desgasteChasis=50}], presupuesto= 10000}
{- --Equipo para tests más detallados. Aunque el presupuesto sería suficiente para reparar los dos ferraris, debe parar al encontrar al fiat
unSueño' :: Equipo
unSueño' = UnEquipo{nombre="Un Sueño", autos=[ferrari{desgasteChasis=10}, fiat{desgasteChasis=50},ferrari{desgasteChasis=10}], presupuesto= 10000}
-}

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

---------- Optimizar autos de equipo ----------
muchosFiats :: Equipo
muchosFiats = UnEquipo{nombre="Muchos Fiats", autos=[fiat, fiat, fiat, ferrari, fiat, fiat], presupuesto= 20000}

costoFerrarizar :: Number
costoFerrarizar = 3500

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
    i. repararEquipo: al recibir como entrada un Equipo cuya lista de autos es infinita, esta función no podrá
    mostrar ningún resultado al nunca terminar de procesar la información. Sin embargo, en "repararEquipo" se
    evalúa el costo de reparación de cada auto y se los repara siempre y cuando el presupuesto sea suficiente.
    Entonces, "repararEquipo" va a tener efectos en la lista de autos del equipo Infinia, específicamente va a
    reparar los primeros 11 autos, pero por ser una lista infinita no va a poder mostrarla.

    ii. optimizarEquipo: el caso de esta función será similar a "repararEquipo", en cuanto a que no podrá mostrar
    ningún resultado porque recibe como entrada un Equipo que contiene una lista de autos infinita, la cual nunca
    finalizará de procesar. Sin embargo, "optimizarEquipo" tampoco tiene efectos en el equipo Infinia, ya que ni
    siquiera el primer auto de la lista podrá ser optimizado por falta de presupuesto. Para que al menos un auto
    pudiera optimizarse, el presupuesto debería ser mínimo de 6500 (velocidadMax * costoNitroPorPuntoVelocidad).

    iii. ferrarizar:

    iv. costoReparacionEquipo: esta función no podrá mostrar resultado alguno, esto por recibir como entrada un Equipo
    cuya lista de autos es infinita. Por ende, cuando se intente aplicar la función "map costoReparacion", la evaluación
    de dicha función nunca va a completarse, haciendo imposible la evaluación de "sum" por requerir los resultados de
    "map costoReparacion (autos equipo)".
-}

------------------------------ PUNTO 4 ------------------------------
---------------------- MODIFICADORES DE TRAMOS -----------------------

-----------------------TRAMO CON BOXES--------------------------------

aplicarBoxes :: Auto -> Tramo -> Auto 
aplicarBoxes auto tramo 
    |boxes (condiciones tramo) = entrarONoABoxes auto
    |otherwise = auto

entrarONoABoxes :: Auto -> Auto
entrarONoABoxes auto
    |not(estaEnBuenEstado auto) = repararAuto auto {tiempoCarrera = tiempoCarrera auto + 10}
    |otherwise = auto

-----------------------TRAMO MOJADO--------------------------------------
aplicarMojado ::  Auto -> Tramo -> Auto
aplicarMojado auto tramo 
    |mojado (condiciones tramo) = auto {tiempoCarrera = tiempoCarrera auto * 1.5}
    |otherwise = auto

-----------------------------TRAMO CON RIPIO----------------------------------MODIF

--Para esta forma, ripio necesita el autoModificado y el auto anterior de todas formas, porque calcula la diferencia
--NOTA: se podria controlar el paso a Boxes. Si ya tiene ripio le podriamos impedir el paso a Boxes...

aplicarRipio :: Auto -> Auto -> Tramo -> Auto 
aplicarRipio  auto autoAntesDelTramo tramo
    |ripio (condiciones tramo) = duplicarEfecto auto autoAntesDelTramo
    |otherwise = auto

duplicarEfecto :: Auto -> Auto ->  Auto
duplicarEfecto autoModificado auto = auto  {
    tiempoCarrera = tiempoCarrera auto + (tiempoCarrera autoModificado - tiempoCarrera auto) *2 + tiempoCarrera autoModificado *2,
    desgasteChasis = desgasteChasis auto + (desgasteChasis autoModificado - desgasteChasis auto) *2,
    desgasteRuedas = desgasteRuedas auto + (desgasteRuedas autoModificado - desgasteRuedas auto) *2
} 

--REVISAR: Simplificar tiempoCarrera, desgasteChasis y desgasteRuedas

----------------------------TRAMO OBSTRUCCION-------------------------------MODIF
aplicarObstruccion :: Auto -> Tramo -> Auto 
aplicarObstruccion auto tramo 
    |obstruccion (condiciones tramo) /= 0 =  auto {desgasteRuedas = desgasteRuedas auto + 2 * obstruccion (condiciones tramo)}
    |otherwise = auto

--REVISAR: metrosObstruidos (pasarlo como parametro o meterlo dentro de data Tramo)
----------------------------TRAMO CON TURBO--------------------------------MODIF

aplicarTurbo :: Auto -> Tramo -> Auto  
aplicarTurbo auto tramo 
    |turbo (condiciones tramo) = auto {velocidadMax = velocidadMax auto*2} {velocidadMax = velocidadMax auto} 
    |otherwise = auto

---------------------------------
aplicarEfectosTramo :: Auto -> Tramo -> Auto 
aplicarEfectosTramo auto tramo 
    |angulo tramo /= 0 &&  longitud tramo /= 0 = transitarCurva tramo auto
    |cambios tramo /= 0 = transitarZigZag tramo auto
    |diametro tramo /= 0 = transitarRulo tramo auto
    |otherwise = transitarRecto tramo auto

verificarModificadoresTramo :: Auto -> Tramo -> Auto
verificarModificadoresTramo auto tramo = aplicarBoxes (aplicarMojado (aplicarRipio (aplicarObstruccion(aplicarTurbo (aplicarEfectosTramo auto tramo) tramo)tramo) auto tramo) tramo) tramo

{-
Explicacion funcion Mamushka: 

funcionEstrella auto tramo  --> elige el tramo que se va recorrer y se lo aplica. Devuelve el autoLuegoDelTramo

aplicarTurbo automodificado tramo --> Se fija si dentro del tramo el turbo esta activado. Si esta en true, lo modifica, sino lo deja igual

aplicarObstruccion automodificadoQueDevolvioAplicarTurbo tramo -->verifica si le estan pasando una cantidad de metros. Si es distinto de 0 es porque hay obstruccion y le aplica los cambios. Sino, lo deja pasar

aplicarRipio autoModificadoQueDevolvioObstruccion auto(el que te ingresan por consola, sin aplicar efectos de tramo) tramo  --> En esta version de aplicar ripio, necesita calcula la diferencia y para eso necesita el auto antes del tramo. Luego, lo mismo de siempre, verifica si el tramo tiene en True el ripio, si SI le aplica los cambios. Si NO lo deja pasar

aplicarMojado autoModificadoQueDovolvioRipio tramo --> Verifica si el tramo tiene en True el mojado, si SI le aplica los cambios. Si NO, lo deja pasar 

aplicarBoxes autoModificadoQueDevolvioMojado tramo --> Verifica si el tramo tiene en True Boxes. Si lo tiene, le aplica al auto la funcion de ver si el auto puede entrar o no a Boxes (y esa segunda funcion verifica si puede entrar o no). Si no lo tiene en True, lo deja salir sin cambios
-}


{-Pregunta[1]: Podria hacer que la funcionMamushka reciba el Number de MtsObstruccion, pero cada vez que se use funcionMamushka deberia pasarle un numero, independientemente que se use o no. Ejemplos de como seria el "pasaje por consola"
Ej si se usa Obstruccion: autitoX tramoX 20  (aca le paso el auto, el tramo y los metros de obstruccion, porque si aplico obstruccion)
Ej si NO se usa obstruccion: autitoX tramoX 0 (tendria que pasarle mtsObstruccion 0)

Por eso directamente estoy haciendo que en el modelado del tramo, en condiciones, Obstruccion no sea un Bool, sino un Number y en caso de que no haya obstruccion que la obstruccion sea =0 y si SI hay que sea /= 0
-}


{--data Tramo = UnTramo{
    angulo :: Number,
    longitud :: Number,
    cambios :: Number,
    diametro :: Number,
    condiciones:: Condiciones 
} deriving Show

data Condiciones = Condiciones{
    boxes :: Bool,
    ripio :: Bool,
    mojado :: Bool,
    obstruccion :: Number, REVISAR[1]
    turbo :: Bool
}deriving Show
-}

------------------------------ PUNTO 5 ------------------------------

pasarPorTramo2 :: (Tramo -> Auto -> Auto) -> Tramo -> Auto -> Auto
pasarPorTramo2 recorrerTramo tramo auto
    | autoNoDaMas auto = auto
    | otherwise = recorrerTramo tramo auto
    

--Realizar la función que haga pasarPorTramo/2 que, dado un tramo y un auto, hace que el auto atraviese el tramo, siempre y cuando
-- no pase que no da más al inicio del tramo. Si el escenario es que no da más, entonces el auto no recibe ningún efecto, ya que no pasa por el tramo.

------------------------------ PUNTO 6 ------------------------------
------------------------ ATRAVESANDO PISTAS -------------------------
data Pista = UnaPista{
    nombrePista :: String,
    pais :: String,
    precioEntrada :: Number,
    recorrido :: [Tramo]
} deriving Show

------------------ VUELTA A LA MANZANA ------------------------ 
vueltaALaManzana :: Pista
vueltaALaManzana = UnaPista{nombrePista = "La Manzana", pais = "Italia", precioEntrada = 30, recorrido = recorridoManzana}

recto130 :: Tramo
recto130 = UnTramo{angulo = 0, longitud = 130, cambios = 0, diametro = 0}

curva90 :: Tramo
curva90 = UnTramo{longitud = 13, angulo = 90, cambios = 0, diametro = 0}

recorridoManzana :: [Tramo]
recorridoManzana = [recto130, curva90, recto130, curva90, recto130, curva90, recto130, curva90]

------------------------- SUPER PISTA ---------------------------
superPista :: Pista
superPista = UnaPista{nombrePista = "Super Pista", pais = "Argentina", precioEntrada = 300, recorrido = recorridoSuperPista}

recorridoSuperPista :: [Tramo]
recorridoSuperPista = []

--recorridoSuperPista :: [Tramo]
--recorridoSuperPista = [tramoRectoClassic, curvaTranca, tramitoMojado, tramitoTurbo, ruloDe10, curvaObstruccion, curva115, recto970, curvaPeligrosa, tramitoRipio, rectoBoxes, casiCurvaObstruccion, ZigZag2, deseoDeMuerteMojadoRipio, ruloClasico, zigZagLoco]

-- FALTA MODELAR: tramitoMojado - tramitoTurbo - ruloDe10 - curvaObstruccion - curva115 - recto970 - tramitoRipio - rectoBoxes - casiCurvaObstruccion - zigZag2 - deseoMuerteMojadoRipio - 


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