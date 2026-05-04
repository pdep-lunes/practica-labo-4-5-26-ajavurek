module Parcial where
import Text.Show.Functions()
juguete :: string
data Perro = unPerro {
    raza :: string,
    juguetesFav :: [juguete],
    tiempo :: int,
    energia :: int,
}

jugar :: Perro -> Perro
jugar unPerro = modificarEnergia (-10) unPerro
modificarEnergia :: Int -> Perro -> Perro
modificarEnergia cantidadEnergia unPerro = unPerro {energia = max (energia unPerro + cantidadEnergia) 0}

ladrar :: int -> unPerro -> unPerro
ladrar unladrido unPerro = unPerro {energia = cantLadrido}
cantLadrido :: int -> int
cantLadrido ladrido =  ladrido / 2

regalar :: juguete -> unPerro -> Perro
regalar unPerro = modificarJuguetes (juguetesFav:) unPerro

permanenciaMin :: Perro -> Bool
permanenciaMin unPerro = unPerro {tiempo == 50}
esDeRazaExtravagante :: unPerro -> bool
esDeRazaExtravagante unPerro = unPerro {raza == "Dálmata" || "Pomerania"}
darRegalo :: unPerro -> unPerro
darRegalo unPerro = unPerro {energia = 100 && regalar nuevoJuguete unPerro}

diaDeSpa :: unPerro -> unPerro
diaDeSpa unPerro 
        |permanenciaMin unPerro || esDeRazaExtravagante unPerro = darRegalo unPerro
        |otherwise = unPerro

modificarJuguete :: ([juguete] -> [juguete]) -> Perro -> Perro
modificarJuguete unaFucion unPerro = unPerro {juguetesFav = (unaFuncion.juguetesFav) unPerro}

perdidaJuguete :: Perro -> Perro
perdidaDeJuguete  unPerro = modificarJuguete (drop 1) unPerro

diaDeCampo :: Perro -> Perro
diaDeCampo = (jugar.perdidaDeJuguete) unPerro

zara :: perro
zara = unPerro "Dálmata" ["pelota","mantita"] 90 80

doble :: Int -> Int
doble = (*2)
