{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe


data Usuario = Usuario {nombre::String, billetera::Int} deriving(show,eq)

cambiarNombre nuevoNombre unUsuario = unUsuario {nombre = nuevoUsuario}
cambiarBilletera nuevaBilletera unUsuario = unUsuario {billetera = nuevaBilletera}
aumentarBilletera unaCantidad unUsuario = unUsuario {billetera = billetera + unaCantidad}
disminuirBilletera unaCantidad = aumentarBilletera (unaCantidad * (-1))

pepe = Usuario "Jose"  10
pepe2 = Usuario "Jose" 20  --Solo para pruebas--
lucho = Usuario "Luciano"  2

type Evento = Usuario -> Usuario
deposito :: int -> Evento
extraccion :: int -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento

deposito unaCantidad  = aumentarBilletera unaCantidad
extraccion unaCantidad = min(0).disminuirBilletera unaCantidad
upgrade unUsuario = aumentarBilletera (billetera unUsuario *0.2) unUsuario
cierreDeCuenta = cambiarBilletera 0
quedaIgual = aumentarBilletera 0
tocoYmeVoy = (cierreDeCuenta.upgrade).deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1


type Transaccion = Evento
transaccion1 :: Transaccion
transaccion2 :: Transaccion

transaccion1 unUsuario | unUsuario == lucho = cierreDeCuenta lucho  --Tengo dudas con esto, pero es una idea inicial--
                       | otherwise quedaIgual unUsuario

transaccion2 unUsuario | unUsuario == pepe = deposito 5 pepe
                       | otherwise quedaIgual unUsuario

transaccion3 unUsuario | unUsuario == lucho = tocoYmeVoy lucho
                       | otherwise quedaIgual unUsuario

transaccion4 unUsuario | unUsuario == lucho = ahorranteErrante lucho
                       | otherwise quedaIgual unUsuario
--Repito mucha logica acá arriba, ver si se puede optimizar--
