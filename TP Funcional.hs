{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Usuario = Usuario {
  nombre::String,
  billetera::Float,
  nivel::Int,
  cantidadDeTransacciones::Int
} deriving(Show, Eq)

cambiarNombre nuevoNombre unUsuario = unUsuario {nombre = nuevoNombre}
cambiarBilletera nuevaBilletera unUsuario = unUsuario {billetera = nuevaBilletera}
aumentarBilletera unaCantidad unUsuario = unUsuario {billetera = billetera unUsuario + unaCantidad}
disminuirBilletera unaCantidad unUsuario = aumentarBilletera (unaCantidad * (-1)) unUsuario
subirDeNivel unUsuario = unUsuario {nivel = nivel unUsuario + 1}

type Evento = Usuario -> Usuario
deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento
tocoYmeVoy :: Evento
ahorranteErrante :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad unUsuario = disminuirBilletera (min unaCantidad (billetera unUsuario)) unUsuario
upgrade unUsuario = aumentarBilletera (min 10 (billetera unUsuario * 0.2))
cierreDeCuenta = cambiarBilletera 0
quedaIgual = id
tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1

testeoDeEventos = hspec $ do
  describe "Tests de eventos" $ do
    it "depositar 10 en una billetera , deberia tener 20 en su billetera" $ (billetera.deposito 10) pepe `shouldBe` 20
    it "extraigo 3 de una billetera , debería quedar con 7" $ (billetera.extraccion 3) pepe `shouldBe` 7
    it "extraigo 15 de una billetera , deberia quedar con 0" $ (billetera.extraccion 15) pepe `shouldBe` 0
    it "hago un upgrade de una billetera , deberia quedar con 12" $ (billetera.upgrade) pepe `shouldBe` 12
    it "cierro la cuenta, billetera deberia quedar en 0" $ (billetera.cierreDeCuenta) pepe `shouldBe` 0
    it "hago un quedaIgual, billetera deberia quedar en 10" $ (billetera.quedaIgual) pepe `shouldBe` 10
    it "deposito y hago un upgrade, billetera deberia quedar en 1020" $ (billetera.upgrade.deposito 1000) pepe `shouldBe` 1020

pepe = Usuario "Jose" 10 0 0
pepe2 = Usuario "Jose" 20 0 0
lucho = Usuario "Luciano" 2 0 0

testeoDeUsuarios = hspec $ do
  describe "Tests de usuarios" $ do
    it "billetera pepe, deberia ser 10" $ billetera pepe `shouldBe` 10
    it "billetera pepe luego de cierre de cuenta, deberia ser 0" $ (billetera.cierreDeCuenta) pepe `shouldBe` 0
    it "billetera de pepe luego de depositar 15, extraer 2 y tener un upgrade deberia ser 27.6" $ (billetera.upgrade.extraccion 2.deposito 15) pepe `shouldBe` 27.6

type Transaccion = Evento
transaccion1 :: Transaccion
transaccion2 :: Transaccion
transaccion3 :: Transaccion
transaccion4 :: Transaccion
transaccion5 :: Transaccion

transaccion1 unUsuario | nombre unUsuario == "Luciano" = cierreDeCuenta unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion2 unUsuario | nombre unUsuario == "Jose" = deposito 5 unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion3 unUsuario | nombre unUsuario == "Luciano" = tocoYmeVoy unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion4 unUsuario | nombre unUsuario == "Luciano" = ahorranteErrante unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion5 unUsuario | nombre unUsuario == "Jose" = extraccion 7 unUsuario
                       | nombre unUsuario == "Luciano" = deposito 7 unUsuario
                       | otherwise = quedaIgual unUsuario

  describe "Tests de transacciones" $ do
    it "transaccion 1 sobre pepe me debería devolver una billetera de 10" $ (billetera.transaccion1) pepe `shouldBe` 10
    it "transaccion 2 sobre pepe me deberiía devolver una billetera de 15" $ (billetera.transaccion2) pepe `shouldBe` 15
    it "transaccion 2 sobre pepe2 me debería devolver billetera de 25" $ (billetera.transaccion2) pepe2 `shouldBe` 25
    it "transaccion 2 sobre pepe2 cuando tiene 50 monedas me debería devolver billetera de 55" $ (billetera.transaccion2.cambiarBilletera 50) pepe2 `shouldBe` 55
  describe "Tests de nuevos eventos" $ do
   it "transaccion 3 sobre lucho cuando tiene 10 monedas me debería devolver una billetera de 0" $ (billetera.transaccion3.cambiarBilletera 10) lucho `shouldBe` 0
   it "transaccion 4 sobre lucho cuando tiene 10 monedas me debería devolver una billetera de 34" $ (billetera.transaccion4.cambiarBilletera 10) lucho `shouldBe` 34
  describe "Tests de pagos entre usuarios" $ do
    it "transaccion 5 sobre pepe me debería devolver una billetera de 3" $ (billetera.transaccion5) pepe `shouldBe` 3
    it "transaccion 5 sobre lucho cuando tiene 10 monedas me debería devolver una billetera de 17" $ (billetera.transaccion5.cambiarBilletera 10) lucho `shouldBe` 17
