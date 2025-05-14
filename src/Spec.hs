module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

    -- punto 4 --
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
    it "Transitar un tramo retro classic con un auto marca Ferrari tiene un desgaste de chasis de 7.15" $ do
      desgasteChasis (transitarRecto tramoRectoClassic ferrari) `shouldBe` 7.15
    it "Transitar un tramo retro classic con un auto marca Ferrari el tiempo en pista es de 11" $ do
      tiempoCarrera (transitarRecto tramoRectoClassic ferrari) `shouldBe` 11
    it "Transitar un tramito con un auto marca Ferrari tiene un desgaste de chasis de 2.6" $ do
      desgasteChasis (transitarRecto tramito ferrari) `shouldBe` 2.6
    it "Transitar un tramito con un auto marca Ferrari el tiempo en pista es de 4" $ do
      tiempoCarrera (transitarRecto tramito ferrari) `shouldBe` 4
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