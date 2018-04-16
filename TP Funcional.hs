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
disminuirBilletera unaCantidad = aumentarBilletera (unaCantidad * (-1))
subirDeNivel unUsuario = unUsuario {nivel = nivel unUsuario + 1}

pepe = Usuario "Jose" 10 0 0
pepe2 = Usuario "Jose" 20 0 0
lucho = Usuario "Luciano" 2 0

type Evento = Usuario -> Usuario
deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento
tocoYmeVoy :: Evento
ahorranteErrante :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad = disminuirBilletera unaCantidad
--Saqué el min 0  porque estaba mal aplicado, intenté buscarle la vuelta pero todavia no encuentro la manera de aplicarlo correctamente
upgrade unUsuario | nivel unUsuario < 10 = (aumentarBilletera (billetera unUsuario * 0.2) . subirDeNivel) unUsuario
                  | otherwise = quedaIgual unUsuario
--Creo que la lógica de upgrade no está bien pensada
cierreDeCuenta = cambiarBilletera 0
quedaIgual = aumentarBilletera 0
tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1


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
{-
testeo = hspec $ do
  describe "Tests de eventos" $ do
    it "depositar 10 en la cuenta de pepe, deberia tener 20 en su billetera" $ (billetera.deposito 10) pepe ´shouldBe´ 20
    it "extraigo 3, billetera debería quedar con 7" $ (billetera.extraccion 3) pepe ´shouldBe´ 7
    it "extraigo 15, billetera deberia quedar con 0" $ (billetera.extraccion 15) pepe ´shouldBe´ 0
    it "hago un upgrade, billetera deberia quedar con 12" $ (billetera.upgrade) pepe ´shouldBe´ 12
    it "cierro la cuenta, billetera deberia quedar en 0" $ (billetera.cierreDeCuenta) pepe ´shouldBe´ 0
    it "hago un quedaIgual, billetera deberia quedar en 10" $ (billetera.quedaIgual) pepe ´shouldBe´ 10
    it "deposito y hago un upgrade, billetera deberia quedar en 1020" $ (billetera.upgrade.deposito 10) pepe ´shouldBe´ 1020
  describe "Tests de usuarios" $ do
    it "billetera pepe, deberia ser 10" $ billetera pepe ´shouldBe´ 10
    it "billetera pepe luego de cierre de cuenta, deberia ser 0" $ (billetera.cierreDeCuenta) pepe ´shouldBe´ 0
    it "billetera de pepe luego de depositar 15, extraer 2 y tener un upgrade deberia ser 27.6" $ (billetera.upgrade.extraccion 2.deposito 15) pepe ´shouldBe´ 27.6
  describe "Tests de transacciones" $ do
    it "transaccion 1 sobre pepe me debería devolver una billetera de 10" $ (billetera.transaccion1) pepe ´shouldBe´ 10
    it "transaccion 2 sobre pepe me deberiía devolver una billetera de 15" $ (billetera.transaccion2) pepe ´shouldBe´ 15
    it "transaccion 2 sobre pepe2 me debería devolver billetera de 55" $ (billetera.transaccion2) pepe2 ´shouldBe´ 55
  describe "Tests de nuevos eventos" $ do
--    it 14
--    it 15
  describe "Tests de pagos entre usuarios" $ do
    it "transaccion 5 sobre pepe me debería devolver una billetera de 3" $ (billetera.transaccion5) pepe ´shouldBe´ 3
    it "transaccion 5 sobre lucho me debería devolver una billetera de 17" $(billetera.transaccion5) lucho ´shouldBe´ 17
-}
