{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import test.Hspec


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

testeo = hspec $ do
  describe "Tests de eventos" $ do
    it 1
    it 2
    it 3
    it 4
    it 5
    it 6
    it 7
  describe "Tests de usuarios" $ do
    it 8
    it 9
    it 10
  describe "Tests de transacciones" $ do
    it 11
    it 12
    it 13
  describe "Tests de nuevos eventos" $ do
    it 14
    it 15
  describe "Tests de pagos entre usuarios" $ do
    it 16
    it 17
