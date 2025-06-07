module Spec where
import PdePreludat
import Library
import Test.Hspec
import Data.List (sort)

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
    -- punto 2 --
  describe "\n------ Casos de Prueba PUNTO 2a ------" $ do
    it "Un auto marca Peugeot NO está en buen estado" $ do
      estaEnBuenEstado peugeot `shouldBe` False
    it "Un auto marca Lamborghini, con tiempo en pista 99 segundos y desgaste chasis 7: está en buen estado" $ do
      estaEnBuenEstado lamborghini {tiempoCarrera = 99, desgasteChasis = 7} `shouldBe` True
    it "Un auto marca Fiat, con tiempo en pista de 99 segundos y desgaste de chasis 33: NO está en buen estado" $ do
      estaEnBuenEstado fiat {tiempoCarrera = 99, desgasteChasis = 33} `shouldBe` False
    it "Un auto marca Ferrari que tiene 130 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 30: está en buen estado" $ do
      estaEnBuenEstado ferrari {tiempoCarrera = 130, desgasteChasis = 30, desgasteRuedas = 50} `shouldBe` True
    it "Un auto marca Ferrari que tiene 15 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 45: NO está en buen estado" $ do
      estaEnBuenEstado ferrari {tiempoCarrera = 130, desgasteChasis = 45, desgasteRuedas = 50} `shouldBe` False
    it "Un auto marca Ferrari que tiene 150 segundos de tiempo en pista con desgaste de ruedas 70 y chasis 30: NO está en buen estado" $ do
      estaEnBuenEstado ferrari {tiempoCarrera = 130, desgasteChasis = 30, desgasteRuedas = 70} `shouldBe` False
  describe "\n------ Casos de Prueba PUNTO 2b ------" $ do
    it "Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90: No da más" $ do
      autoNoDaMas ferrari {desgasteChasis = 90, desgasteRuedas = 20, apodo = ["La Ferrari"]} `shouldBe` True
    it "Un auto de marca Ferrari con desgaste de chasis 20: Da para más" $ do
      autoNoDaMas ferrari {desgasteChasis = 20} `shouldBe` False
    it "Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20: No da más" $ do
      autoNoDaMas lamborghini {desgasteChasis = 20, desgasteRuedas = 90} `shouldBe` True
    it "Un auto de marca Lamborghini: Da para más" $ do
      autoNoDaMas lamborghini `shouldBe` False
  describe "\n------ Casos de Prueba PUNTO 2c ------" $ do
    it "Un auto de marca Lamborghini: Es un chice" $ do
      esUnChiche lamborghini `shouldBe` True
    it "Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20: NO es un chice" $ do
      esUnChiche lamborghini {desgasteChasis = 20, desgasteRuedas = 90} `shouldBe` False
    it "Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90: NO es un chice" $ do
      esUnChiche ferrari {desgasteRuedas = 20, desgasteChasis = 90} `shouldBe` False
    it "Un auto de marca ferrari: Es un chice" $ do
      esUnChiche ferrari `shouldBe` True
  describe "\n------ Casos de Prueba PUNTO 2d ------" $ do
    it "Un auto marca Peugeot: Es una joya" $ do
      esUnaJoya peugeot `shouldBe` True
    it "Un auto marca Ferrari: NO es una joya" $ do
      esUnaJoya ferrari `shouldBe` False
  describe "\n------ Casos de Prueba PUNTO 2e ------" $ do
    it "Un auto marca ferrari: valor de chetez debe ser igual a 180" $ do
      nivelDeChetez ferrari `shouldBe` 180
  describe "\n------ Casos de Prueba PUNTO 2f ------" $ do
    it "Un auto de marca Ferrari: valor de su capacidad debe ser igual a 7" $ do
      determinarCapacidad ferrari `shouldBe` 7
  describe "\n------ Casos de Prueba PUNTO 2g ------" $ do
    it "Un auto de marca Lamborghini: valor de riesgo debe ser igual a 29.2" $ do
      riesgoAuto lamborghini `shouldBe` 29.2
    it "Un auto de marca Fiat: valor de riesgo debe ser igual a 237.6" $ do
      riesgoAuto fiat `shouldBe` 237.6
  ---- punto 3 ----
  describe "\n ------ Casos de Prueba PUNTO 3a ------" $ do 
    it "Reparar un auto de marca Fiat debería tener  4,95 en chasis (33 * 0,15)" $ do
        desgasteChasis (repararAuto fiat) `shouldBe` 4.95
    it "Reparar un auto de marca Fiat debería tener  0 en ruedas" $ do
        desgasteRuedas (repararAuto fiat) `shouldBe` 0
    
    it "Reparar un auto de marca Ferrari debería mantener sus desgastes en de chasis en 0 " $ do
        desgasteChasis (repararAuto ferrari) `shouldBe` 0
    it "Reparar un auto de marca Ferrari debería mantener sus desgastes de ruedas 0" $ do
        desgasteRuedas (repararAuto ferrari) `shouldBe` 0
  describe "\n ------ Casos de Prueba PUNTO 3b ------" $ do
    it "Aplicar penalidad de 20 segundos un auto de marca ferrari con tiempo 10 segundos en pista debería subir su tiempo en pista a 30" $ do
        tiempoCarrera(aplicarPenalidad 20 ferrari{tiempoCarrera=10}) `shouldBe` 30
    it "Aplicar penalidad de 0 segundos a un auto de marca ferrari con tiempo 10 segundos en pista debería mantener su tiempo pista en 10" $ do
        tiempoCarrera(aplicarPenalidad 0 ferrari{tiempoCarrera=10}) `shouldBe` 10

  describe "\n ------ Casos de Prueba PUNTO 3c ------" $ do
    it "Ponerle nitro a un fiat debería dar velocidad máxima de 52,8" $ do
        velocidadMax(ponerleNitroAUnAuto fiat) `shouldBe` 52.8
    it "Ponerle nitro a un fiat con velocidad máxima 0 debería mantener la velocidad máxima en 0" $ do
        velocidadMax(ponerleNitroAUnAuto ferrari{velocidadMax=0}) `shouldBe` 0   
  
  describe "\n ------ Casos de Prueba PUNTO 3d ------" $ do
    it "Bautizar `El diablo` a un auto marca Lamborghini debe contener en sus apodos El diablo" $ do
        apodo(bautizarAuto "El diablo" lamborghini) `shouldBe` [ "El diablo", "Lambo", "La bestia" ]
    it "Bautizar `El diablo` a un auto marca Lamborghini sin apodos debe sólo tener el apodo `El diablo`" $ do
        apodo(bautizarAuto "El diablo" lamborghini{apodo=[]}) `shouldBe` [ "El diablo" ]
  
  describe "\n ------ Casos de Prueba PUNTO 3e ------" $ do
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca `Tesla` modelo `X`, la marca debería ser `Tesla`" $ do
        marca(llevarAutoDesarmadero fiat "Tesla" "X" ) `shouldBe` "Tesla"
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca `Tesla` modelo `X`, el modelo debería ser `X`" $ do
        modelo(llevarAutoDesarmadero fiat "Tesla" "X" ) `shouldBe` "X"
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca `Tesla` modelo `X`, solo debería tener el apodo `Nunca Taxi`" $ do
        apodo(llevarAutoDesarmadero fiat "Tesla" "X" ) `shouldBe` ["Nunca Taxi"]
        {-
    -- punto 4 --
  describe "\n------ Casos de Prueba PUNTO 4a ------" $ do
    it "Transitar una curva peligrosa con un auto marca Ferrari deja el desgaste de ruedas en 15" $ do
      desgasteRuedas (transitarCurva curvaPeligrosa ferrari) `shouldBe` 15 
    it "Transitar una curva peligrosa  con un auto marca Ferrari mantiene el desgaste de chasis en 0" $ do
      desgasteChasis (transitarCurva curvaPeligrosa ferrari) `shouldBe` 0
    it "Transitar una curva peligrosa  con un auto marca Peugeot el tiempo en pista es de 15" $ do
      tiempoCarrera (transitarCurva curvaPeligrosa peugeot) `shouldBe` 15 
    it "Transitar una curva tranca  con un auto marca Ferrari deja el desgaste de ruedas en 15" $ do
      desgasteRuedas (transitarCurva curvaTranca ferrari) `shouldBe` 15 
    it "Transitar una curva tranca  con un auto marca Ferrari mantiene el desgaste de chasis en 0" $ do
      desgasteChasis (transitarCurva curvaTranca ferrari) `shouldBe` 0
    it "Transitar una curva tranca  con un auto marca Peugeot el tiempo en pista es de 27.5" $ do
      tiempoCarrera (transitarCurva curvaTranca peugeot) `shouldBe` 27.5
  describe "\n------ Casos de Prueba PUNTO 4b ------" $ do
    it "Transitar un tramo retro classic con un auto marca Ferrari tiene un desgaste de chasis de 7.15" $ do
      desgasteChasis (transitarRecto tramoRectoClassic ferrari) `shouldBe` 7.15
    it "Transitar un tramo retro classic con un auto marca Ferrari el tiempo en pista es de 11" $ do
      tiempoCarrera (transitarRecto tramoRectoClassic ferrari) `shouldBe` 11
    it "Transitar un tramito con un auto marca Ferrari tiene un desgaste de chasis de 2.6" $ do
      desgasteChasis (transitarRecto tramito ferrari) `shouldBe` 2.6
    it "Transitar un tramito con un auto marca Ferrari el tiempo en pista es de 4" $ do
      tiempoCarrera (transitarRecto tramito ferrari) `shouldBe` 4
  describe "\n------ Casos de Prueba PUNTO 4c ------" $ do
    it "Transitar un tramo zigZagLoco con un auto marca Ferrari Tiene un desgaste de chasis de 5" $ do
      desgasteChasis (transitarZigZag zigZagLoco ferrari) `shouldBe` 5
    it "Transitar un tramo zigZagLoco con un auto marca Ferrari Tiene un desgaste de ruedas de 32.5" $ do
      desgasteRuedas (transitarZigZag zigZagLoco ferrari) `shouldBe` 32.5
    it "Transitar un tramo zigZagLoco con un auto marca Ferrari El tiempo en pista es de 15" $ do
      tiempoCarrera (transitarZigZag zigZagLoco ferrari) `shouldBe` 15
    it "Transitar un tramo casiCurva con un auto marca Ferrari Tiene un desgaste de chasis de 5" $ do
      desgasteChasis (transitarZigZag casiCurva ferrari) `shouldBe` 5
    it "Transitar un tramo casiCurva con un auto marca Ferrari Tiene un desgaste de ruedas de 6.5" $ do
      desgasteRuedas (transitarZigZag casiCurva ferrari) `shouldBe` 6.5
    it "Transitar un tramo casiCurva con un auto marca Ferrari El tiempo en pista es de 3" $ do
      tiempoCarrera (transitarZigZag casiCurva ferrari) `shouldBe` 3
  describe "\n------ Casos de Prueba PUNTO 4d ------" $ do
    it "Transitar un tramo ruloClasico con un auto marca Ferrari Tiene un desgaste de chasis de 0" $ do
      desgasteChasis (transitarRulo ruloClasico ferrari) `shouldBe` 0
    it "Transitar un tramo ruloClasico con un auto marca Ferrari Tiene un desgaste de ruedas de 19.5" $ do
      desgasteRuedas (transitarRulo ruloClasico ferrari) `shouldBe` 19.5
    it "Transitar un tramo ruloClasico con  un auto marca Ferrari El tiempo en pista es de 1" $ do
      tiempoCarrera (transitarRulo ruloClasico ferrari) `shouldBe` 1
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari Tiene un desgaste de chasis de 0" $ do
      desgasteChasis (transitarRulo deseoDeMuerte ferrari) `shouldBe` 0
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari Tiene un desgaste de ruedas de 39.0" $ do
      desgasteRuedas (transitarRulo deseoDeMuerte ferrari) `shouldBe` 39.0
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari El tiempo en pista es de 2" $ do
      tiempoCarrera (transitarRulo deseoDeMuerte ferrari) `shouldBe` 2
    -- punto 5 --
  describe "\n------ Casos de Prueba PUNTO 5a ------" $ do
    it "El nivel de joyez es de 0 para el Ferrari" $ do
      nivelDeJoyez [ferrari] `shouldBe` 0
    it "El nivel de joyez es de 1 para un Peugeot con un tiempo de carrera de 49" $ do
      nivelDeJoyez [peugeot {tiempoCarrera = 49}] `shouldBe` 1
    it "El nivel de joyez es de 2 para un Peugeot con un tiempo de carrera de 50" $ do
      nivelDeJoyez [peugeot {tiempoCarrera = 50}] `shouldBe` 2
  describe "\n------ Casos de Prueba PUNTO 5b ------" $ do
    it "El grupo con un Ferrari con un tiempo de carrera de 201 y otro con un tiempo de carrera de 200 no es para entendidos" $ do
      paraEntendidos [ferrari {tiempoCarrera = 201}, ferrari {tiempoCarrera = 200}] `shouldBe` False
    it "El grupo con un Ferrari con un tiempo de carrera de 200 y un Peugeot no es para entendidos" $ do
      paraEntendidos [ferrari {tiempoCarrera = 200}, peugeot] `shouldBe` False
    it "El grupo con un Ferrari con un tiempo de carrera de 200 y un Lamborghini con un tiempo de carrera de 200 es para entendidos" $ do
      paraEntendidos [ferrari {tiempoCarrera = 200}, lamborghini {tiempoCarrera = 200}] `shouldBe` True
    -- entrega 2
  describe "\n------ Casos de Prueba PUNTO 1a ------"  $ do
    it "Agregar un Ferrari (costo 65000) a un equipo con presupuesto 70000. El Ferrari se deberia agregar" $ do
     tieneFerrari(agregarAuto losAmbiciosos ferrari) `shouldBe` True
    it "Agregar un Fiat (costo 44000) a un equipo con presupuesto 50000 que ya tiene un Peugeot. El Fiat se agrega al equipo, el presupuesto se reduce en 44000" $ do
     tieneFiat(agregarAuto losTranquilos fiat) `shouldBe` True
  describe "\n------ Casos de Prueba PUNTO 2 ------" $ do
    it "Calcular el costo de reparación de un equipo con Ferrari (chasis 10), Lamborghini (chasis 20)" $ do
      costoReparacionEquipo losTops{autos = [ferrari {desgasteChasis = 10}, lamborghini {desgasteChasis = 20}]} `shouldBe` 12750
    it "Calcular el costo de reparación de un equipo con Fiat (chasis 50), Peugeot (chasis 0)" $ do
      costoReparacionEquipo unSueño{autos = [fiat {desgasteChasis = 50}, peugeot {desgasteChasis = 0}]} `shouldBe` 21250
  describe "\n------ Casos de Prueba PUNTO 6 ------" $ do
    it "Una Ferrari y un Peugeot con desgaste 79 de ruedas pegan la vuelta a una pista vueltaALaManzana" $ do
      tiempoCarrera (head(peganLaVuelta2 vueltaALaManzana [ferrari, peugeot{desgasteRuedas=79}])) `shouldBe` 9.6
    it "Una Ferrari y un Peugeot con desgaste 79 de ruedas pegan la vuelta a una pista vueltaALaManzana" $ do
      tiempoCarrera (last(peganLaVuelta2 vueltaALaManzana [ferrari {desgasteRuedas = 79}, peugeot {desgasteRuedas = 79}])) `shouldBe` 11.7
  describe "\n------ Casos de Prueba PUNTO 7 ------" $ do
    it "Tiempo parcial del primer auto en la lista en completar la segunda vuelta" $ do
      tiempoParcialPrimero tourDeItalia 2 [ferrari, peugeot, lamborghini, fiat] `shouldBe` 19.2
    it "El ganador es el Lamborghini" $ do
      darGanador tourDeItalia [ferrari, peugeot, lamborghini, fiat] `shouldBe` head(correrCarrera tourDeItalia [lamborghini])
    it "Cuantos autos terminaron la carrera" $ do
      cantidadAutosQueTerminaron tourDeItalia [ferrari, peugeot, lamborghini, fiat, fiat{desgasteRuedas=79}] `shouldBe` 4
    it "Tiempo total del segundo en salir" $ do
      tiempoTotalSegundo tourDeItalia [ferrari, peugeot, lamborghini, fiat] `shouldBe` 156
-}