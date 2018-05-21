{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Billetera = Float

aumentarBilletera :: Float -> Billetera -> Billetera
disminuirBilletera :: Float -> Billetera -> Billetera

aumentarBilletera unaCantidad unaBilletera = unaBilletera + unaCantidad
disminuirBilletera unaCantidad unaBilletera = aumentarBilletera (unaCantidad * (-1)) unaBilletera

-- Eventos

type Evento = Billetera -> Billetera

deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad unaBilletera = disminuirBilletera (min unaCantidad  unaBilletera) unaBilletera
upgrade unaBilletera = aumentarBilletera (min 10 (unaBilletera * 0.2)) unaBilletera
cierreDeCuenta _ = 0
quedaIgual = id

billeteraTest :: Billetera
billeteraTest2 :: Billetera
billeteraTest3 :: Billetera

billeteraTest = 10
billeteraTest2 = 20
billeteraTest3 = 50

testeoDeEventos = hspec $ do
  describe "Tests de eventos" $ do
    {-1-} it "depositar 10 en una billetera , deberia tener 20 en su billetera" $ (deposito 10) billeteraTest `shouldBe` 20
    {-2-} it "extraigo 3 de una billetera , debería quedar con 7" $ (extraccion 3) billeteraTest `shouldBe` 7
    {-3-} it "extraigo 15 de una billetera , deberia quedar con 0" $ (extraccion 15) billeteraTest `shouldBe` 0
    {-4-} it "hago un upgrade de una billetera , deberia quedar con 12" $ upgrade billeteraTest `shouldBe` 12
    {-5-} it "cierro la cuenta, billetera deberia quedar en 0" $ cierreDeCuenta billeteraTest `shouldBe` 0
    {-6-} it "hago un quedaIgual, billetera deberia quedar en 10" $ quedaIgual billeteraTest `shouldBe` 10
    {-7-} it "deposito y hago un upgrade, billetera deberia quedar en 1020" $ (upgrade.deposito 1000) billeteraTest `shouldBe` 1020

-- Usuarios

data Usuario = Usuario {
  nombre::String,
  billetera::Billetera
  } deriving (Show, Eq)

pepe = Usuario "Jose" 10
pepe2 = Usuario "Jose" 20
lucho = Usuario "Luciano" 2

testeoDeUsuarios = hspec $ do
  describe "Tests de usuarios" $ do
    {-8-} it "billetera pepe, deberia ser 10" $ billetera pepe `shouldBe` 10
    {-9-} it "billetera pepe luego de cierre de cuenta, deberia ser 0" $ cierreDeCuenta (billetera pepe) `shouldBe` 0
   {-10-} it "billetera de pepe luego de depositar 15, extraer 2 y tener un upgrade deberia ser 27.6" $ (upgrade.extraccion 2.deposito 15.billetera) pepe `shouldBe` 27.6

-- Transacciones

tocoYmeVoy :: Evento
ahorranteErrante :: Evento

tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1

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
  {-11-} it "transaccion 1 sobre pepe a una billetera de 20 monedas me debería devolver una billetera de 20" $ transaccion1 pepe billeteraTest2 `shouldBe` 20
  {-12-} it "transaccion 2 sobre pepe a una billetera de 10 monedas me debería devolver una billetera de 15" $ transaccion2 pepe billeteraTest `shouldBe` 15
  {-13-} it "transaccion 2 sobre pepe2 a una billetera de 50 monedas me debería devolver billetera de 55" $ transaccion2 pepe2 billeteraTest3 `shouldBe` 55
-- Nuevos Eventos
  describe "Tests de nuevos eventos" $ do
  {-14-} it "transaccion 3 sobre lucho a una billetera de 10 monedas me debería devolver una billetera de 0" $ transaccion3 lucho billeteraTest `shouldBe` 0
  {-15-} it "transaccion 4 sobre lucho a una billetera de 10 monedas me debería devolver una billetera de 34" $ transaccion4 lucho billeteraTest `shouldBe` 34
-- Pagos entre Usuarios
  describe "Tests de pagos entre usuarios" $ do
  {-16-} it "transaccion 5 sobre pepe  a una billetera de 10 monedas me debería devolver una billetera de 3" $ transaccion5 pepe billeteraTest `shouldBe` 3
  {-17-} it "transaccion 5 sobre lucho  a una billetera de 10 monedas me debería devolver una billetera de 17" $ transaccion5 lucho billeteraTest `shouldBe` 17

-- Usuario luego de transacción

actualizarBilletera unUsuario nuevaBilletera = unUsuario {billetera = nuevaBilletera}

usuarioLuegoDeTransaccion unUsuario unaTransaccion = actualizarBilletera unUsuario (unaTransaccion unUsuario (billetera unUsuario))

testeoLuegoDeTransaccion = hspec $ do
  describe "Testeos sobre usuarios luego de transacciones" $ do
  {-18-} it "Aplicar transaccion 1 a pepe deberia dejarlo igual" $ usuarioLuegoDeTransaccion pepe transaccion1 `shouldBe` pepe
  {-19-} it "Aplicar transaccion 5 a lucho deberia devolverlo con una billetera de 9" $ usuarioLuegoDeTransaccion lucho transaccion5 `shouldBe` actualizarBilletera lucho 9
  {-20-} it "Aplicar la transaccion 5 y la 2 a pepe deberia devolverlo con una billetera de 8" $ usuarioLuegoDeTransaccion (usuarioLuegoDeTransaccion pepe transaccion5) transaccion2 `shouldBe` actualizarBilletera pepe 8

type Bloque = [Transaccion]

bloque1 :: Bloque

bloque1 = [transaccion1, transaccion2, transaccion2, transaccion2, transaccion3, transaccion4, transaccion5, transaccion3]

unBloque unUsuario transacciones = foldl (\unUsuario transaccion -> usuarioLuegoDeTransaccion unUsuario transaccion) unUsuario transacciones

testeoDeBloque1 = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar el bloque1" $ do
  {-21-} it "Aplicar bloque 1 a pepe nos devuelve un pepe con una billetera de 18" $ (unBloque pepe) bloque1 `shouldBe` actualizarBilletera pepe 18
  {-22-} it "Si aplico el bloque 1 a pepe y a lucho el unico que queda con una billetera >10 es pepe" $ (billetera.unBloque pepe) bloque1 > 10 && (billetera.unBloque lucho) bloque1 < 10 `shouldBe` True
  {-23-} it "El mas adinerado luego de aplicar el bloque 1 deberia ser pepe" $ (billetera.unBloque pepe) bloque1 > (billetera.unBloque lucho) bloque1 `shouldBe` True -- REMPLAZAR CON FUNCIONES QUE HAGAN ESTO
  {-24-} it "El menos adinerado luego de aplicar el bloque 1 deberia ser lucho" $ (billetera.unBloque lucho) bloque1 < (billetera.unBloque pepe) bloque1 `shouldBe` True -- REMPLAZAR CON FUNCIONES QUE HAGAN ESTO

bloque2 :: Bloque

bloque2 = take 5 (repeat transaccion2)

blockChain = bloque2 : take 10 (repeat bloque1)

muchosBloques unUsuario bloques = foldr (\bloque unUsuario  -> unBloque unUsuario bloque) unUsuario bloques

buscarHistorial numeroBloque unUsuario | numeroBloque < length blockChain = muchosBloques unUsuario (take numeroBloque blockChain)
                                       | otherwise = muchosBloques unUsuario blockChain

peorBloque unUsuario bloque otroBloque | (billetera.unBloque unUsuario) bloque < (billetera.unBloque unUsuario) otroBloque = bloque
                                       | otherwise = otroBloque

-- Blockchain

testeoDeBlockChain = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar blockChain" $ do
    {-25-} it "El peor bloque para pepe es el bloque 1" $ unBloque pepe (peorBloque pepe bloque1 bloque2) `shouldBe` actualizarBilletera pepe 18
    {-26-} it "blockChain aplicada a pepe nos devuelve a pepe con una billetera de 115" $ muchosBloques pepe blockChain `shouldBe` actualizarBilletera pepe 115
    {-27-} it "Tomando los primeros 3 bloques del blockChain y aplicandoselo a pepe nos devuelve a pepe con una billetera de 51" $ buscarHistorial 3 pepe `shouldBe` actualizarBilletera pepe 51
    {-28-} it "Si aplico blockChain a lucho y a pepe la suma de sus billeteras nos deberia dar 115" $ (billetera.muchosBloques pepe) blockChain + (billetera.muchosBloques lucho) blockChain `shouldBe` 115

testeoDeBlockChainInfinito = hspec $ do
  describe "Testeos sobre usuarios luego de aplicar el blockChain infinito" $ do
    {-29-} it "Para que pepe llegue a 10000 creditos en su billetera, debo aplicar el bloque 1  11 veces" $ aplicarBlockChainInfinito pepe [bloque1] 10000 `shouldBe` 11

aplicarBlockChainInfinito unUsuario unBloque unaCantidad | billetera(muchosBloques unUsuario unBloque) >= unaCantidad = length unBloque
                                                         | otherwise = aplicarBlockChainInfinito unUsuario (iterarBloque unBloque) unaCantidad

iterarBloque unBloque = unBloque ++ [concat (replicate 2 (last unBloque))]

testearTodo = do
    testeoDeEventos
    testeoDeUsuarios
    testeoDeTransacciones
    testeoLuegoDeTransaccion
    testeoDeBloque1
    testeoDeBlockChain
    testeoDeBlockChainInfinito
