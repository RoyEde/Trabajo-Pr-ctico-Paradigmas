{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Billetera = Billetera {
  dinero :: Float
} deriving(Show, Eq)

cambiarBilletera :: Float -> Billetera -> Billetera
aumentarBilletera :: Float -> Billetera -> Billetera
disminuirBilletera :: Float -> Billetera -> Billetera

cambiarBilletera nuevaBilletera unaBilletera = unaBilletera {dinero = nuevaBilletera}
aumentarBilletera unaCantidad unaBilletera = unaBilletera {dinero = dinero unaBilletera + unaCantidad}
disminuirBilletera unaCantidad unaBilletera = aumentarBilletera (unaCantidad * (-1)) unaBilletera

cambiarNombre :: String -> Usuario -> Usuario

cambiarNombre nuevoNombre unUsuario = unUsuario {nombre = nuevoNombre}

type Evento = Billetera -> Billetera
deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento
tocoYmeVoy :: Evento
ahorranteErrante :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad unaBilletera = disminuirBilletera (min unaCantidad (dinero unaBilletera)) unaBilletera
upgrade unaBilletera = aumentarBilletera (min 10 (dinero unaBilletera * 0.2)) unaBilletera
cierreDeCuenta = cambiarBilletera 0
quedaIgual = id
tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1

data Usuario = Usuario {
  nombre::String,
  billetera::Billetera
  } deriving (Show, Eq)

billeteraTest = Billetera 10
billeteraTest2 = Billetera 20
billeteraTest3 = Billetera 50

testeoDeEventos = hspec $ do
  describe "Tests de eventos" $ do
    it "depositar 10 en una billetera , deberia tener 20 en su billetera" $ (dinero.deposito 10) billeteraTest `shouldBe` 20
    it "extraigo 3 de una billetera , debería quedar con 7" $ (dinero.extraccion 3) billeteraTest `shouldBe` 7
    it "extraigo 15 de una billetera , deberia quedar con 0" $ (dinero.extraccion 15) billeteraTest `shouldBe` 0
    it "hago un upgrade de una billetera , deberia quedar con 12" $ (dinero.upgrade) billeteraTest `shouldBe` 12
    it "cierro la cuenta, billetera deberia quedar en 0" $ (dinero.cierreDeCuenta) billeteraTest `shouldBe` 0
    it "hago un quedaIgual, billetera deberia quedar en 10" $ (dinero.quedaIgual) billeteraTest `shouldBe` 10
    it "deposito y hago un upgrade, billetera deberia quedar en 1020" $ (dinero.upgrade.deposito 1000) billeteraTest `shouldBe` 1020

pepe = Usuario "Jose" (Billetera 10)
pepe2 = Usuario "Jose" (Billetera 20)
lucho = Usuario "Luciano" (Billetera 2)

testeoDeUsuarios = hspec $ do
  describe "Tests de usuarios" $ do
    it "billetera pepe, deberia ser 10" $ (dinero.billetera) pepe `shouldBe` 10
    it "billetera pepe luego de cierre de cuenta, deberia ser 0" $ (dinero.cierreDeCuenta.billetera) pepe `shouldBe` 0
    it "billetera de pepe luego de depositar 15, extraer 2 y tener un upgrade deberia ser 27.6" $ (dinero.upgrade.extraccion 2.deposito 15.billetera) pepe `shouldBe` 27.6

type Transaccion = Usuario -> Evento
transaccion1 :: Transaccion
transaccion2 :: Transaccion
transaccion3 :: Transaccion
transaccion4 :: Transaccion
transaccion5 :: Transaccion

transaccion1 unUsuario | nombre unUsuario == "Luciano" = cierreDeCuenta
                       | otherwise = quedaIgual

transaccion2 unUsuario | nombre unUsuario == "Jose" = deposito 5
                       | otherwise = quedaIgual

transaccion3 unUsuario | nombre unUsuario == "Luciano" = tocoYmeVoy
                       | otherwise = quedaIgual

transaccion4 unUsuario | nombre unUsuario == "Luciano" = ahorranteErrante
                       | otherwise = quedaIgual

transaccion5 unUsuario | nombre unUsuario == "Jose" = extraccion 7
                       | nombre unUsuario == "Luciano" = deposito 7
                       | otherwise = quedaIgual

testeoDeTransacciones = hspec $ do
  describe "Tests de transacciones" $ do
    it "transaccion 1 sobre pepe a una billetera de 20 monedas me debería devolver una billetera de 20" $ dinero (transaccion1 pepe billeteraTest2) `shouldBe` 20
    it "transaccion 2 sobre pepe a una billetera de 10 monedas me deberiía devolver una billetera de 15" $ dinero (transaccion2 pepe billeteraTest) `shouldBe` 15
    it "transaccion 2 sobre pepe2 a una billetera de 50 monedas me debería devolver billetera de 55" $ dinero (transaccion2 pepe2 billeteraTest3) `shouldBe` 55
  describe "Tests de nuevos eventos" $ do
   it "transaccion 3 sobre lucho a una billetera de 10 monedas me debería devolver una billetera de 0" $ dinero (transaccion3 lucho billeteraTest) `shouldBe` 0
   it "transaccion 4 sobre lucho a una billetera de 10 monedas me debería devolver una billetera de 34" $ dinero (transaccion4 lucho billeteraTest) `shouldBe` 34
  describe "Tests de pagos entre usuarios" $ do
    it "transaccion 5 sobre pepe  a una billetera de 10 monedas me debería devolver una billetera de 3" $ dinero (transaccion5 pepe billeteraTest) `shouldBe` 3
    it "transaccion 5 sobre lucho  a una billetera de 10 monedas me debería devolver una billetera de 17" $ dinero (transaccion5 lucho billeteraTest) `shouldBe` 17

{-testeoLuegoDeTransaccion = hspec $ do
  describe "Testeos sobre usuarios luego de transacciones" $ do
    it "Aplicar transaccion 1 a pepe deberia dejarlo igual" $
    it "Aplicar transaccion 5 a lucho deberia devolverlo con una billetera de 9" $
    it "Aplicar la transaccion 5 y la 2 a pepe deberia devolverlo con una billetera de 8" $-}

--bloque1 = (transaccion1.transaccion2.transaccion2.transaccion2.transaccion3.transaccion4.transaccion5.transaccion3)

{-testeoDeBloque1 = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar el bloque1" $ do
    it "Aplicar bloque 1 a pepe nos devuelve un pepe con una billetera de 18" $
    it "Si aplico el bloque 1 a pepe y a lucho el unico que queda con una billetera >10 es pepe" $
    it "El mas adinerado luego de aplicar el bloque 1 deberia ser pepe" $
    it "El menos adinerado luego de aplicar el bloque 1 deberia ser lucho" $-}
{-
bloque2 = (transaccion2.transaccion2.transaccion2.transaccion2.transaccion2)

blockChain :: Usuario -> Usuario

blockChain = bloque2 . (take 10. repeat) bloque1
-}
{-testeoDeBlockChain = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar blockChain" $ do
    it "El peor bloque para pepe es el bloque 1" $
    it "blockChain aplicada a pepe nos devuelve a pepe con una billetera de 115" $
    it "Tomando los primeros 3 bloques del blockChain y aplicandoselo a pepe nos devuelve a pepe con una billetera de 51" $
    it "Si aplico blockChain a lucho y a pepe la suma de sus billeteras nos deberia dar 115" $-}

--blockChainInfinito = iterate bloque1

{-testeoDeBlockChainInfinito = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar el blockChain infinito" $ do
    it "Para que pepe llegue a 10000 creditos en su billetera, debo aplicar el bloque 1  11 veces" $-}
    
-- Bloque1 => [transaccion1, transaccion2, transaccion2, transaccion2, transaccion3, transaccion4, transaccion5, transaccion3]
-- Bloque2 => [transaccion2, transaccion2, transaccion2, transaccion2, transaccion2]
usuarioLuegoDeTransaccion unUsuario unaTransaccion = actualizarBilletera unUsuario (($) unaTransaccion unUsuario (billetera unUsuario))

actualizarBilletera unUsuario nuevaBilletera = unUsuario {billetera = nuevaBilletera}

unBloque unUsuario transacciones = foldl (\unUsuario transaccion -> usuarioLuegoDeTransaccion unUsuario transaccion) unUsuario transacciones

blockChain unUsuario unBlockChain = foldl (\unUsuario bloque -> unBloque unUsuario bloque ) unUsuario unBlockChain

blockChainInfinito unUsuario bloque unaCantidad 
                                                |dinero(billetera(blockChain unUsuario bloque)) >= unaCantidad = length bloque
												|otherwise = blockChainInfinito unUsuario (iterarBloque bloque) unaCantidad

iterarBloque unBloque = unBloque ++ [concat (replicate 2 (last unBloque))]
