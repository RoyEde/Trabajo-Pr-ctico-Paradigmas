{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe


data Usuario = Usuario {
  nombre::String,
  billetera::Int,
  nivel::Int,
  cantidadDeTransacciones::Int
} deriving(Show,Eq)


cambiarNombre nuevoNombre unUsuario = unUsuario {nombre = nuevoNombre}
cambiarBilletera nuevaBilletera unUsuario = unUsuario {billetera = nuevaBilletera}
aumentarBilletera unaCantidad unUsuario = unUsuario {billetera = billetera unUsuario + unaCantidad}
disminuirBilletera unaCantidad = aumentarBilletera (unaCantidad * (-1))
subirDeNivel unUsuario = unUsuario {nivel = nivel unUsuario + 1}

pepe = Usuario "Jose"  10 0
pepe2 = cambiarBilletera 20 pepe 
lucho = Usuario "Luciano"  2 0

type Evento = Usuario -> Usuario
deposito :: Int -> Evento
extraccion :: Int -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad = (min(0) . disminuirBilletera) unaCantidad
upgrade unUsuario | nivel unUsuario < 10 = (aumentarBilletera (billetera unUsuario * 0.2) . subirDeNivel) unUsuario
                  | otherwise = quedaIgual unUsuario
cierreDeCuenta = cambiarBilletera 0
quedaIgual = aumentarBilletera 0
tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1


type Transaccion = Evento
transaccion1 :: Transaccion
transaccion2 :: Transaccion

transaccion1 unUsuario | nombre unUsuario == "lucho" = cierreDeCuenta unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion2 unUsuario | nombre unUsuario == "pepe" = deposito 5 unUsuario
                       | otherwise = quedaIgual unUsuario

transaccion3 unUsuario | unUsuario == lucho = tocoYmeVoy lucho
                       | otherwise = quedaIgual unUsuario

transaccion4 unUsuario | unUsuario == lucho = ahorranteErrante lucho
                       | otherwise = quedaIgual unUsuario
