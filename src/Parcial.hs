module Parcial where
import Text.Show.Functions()
juguete :: Jtring
data Perro = unPerro {
    raza :: String,
    juguetesFav :: [juguete],
    tiempo :: Int,
    energia :: Int,
}

jugar :: Perro -> Perro
jugar unPerro = modificarEnergia (-10) unPerro
modificarEnergia :: Int -> Perro -> Perro
modificarEnergia cantidadEnergia unPerro = unPerro {energia = max (energia unPerro + cantidadEnergia) 0}

ladrar :: Int -> Perro -> Perro
ladrar unladrido unPerro = unPerro {energia = cantLadrido}
cantLadrido :: Int -> Int
cantLadrido ladrido =  ladrido / 2

regalar :: Juguete -> Perro -> Perro
regalar unPerro = modificarJuguetes (juguetesFav:) unPerro

permanenciaMin :: Perro -> Bool
permanenciaMin unPerro = unPerro {tiempo == 50}
esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = unPerro {raza == "Dálmata" || "Pomerania"}
darRegalo :: Perro -> Perro
darRegalo unPerro = unPerro {energia = 100 && regalar nuevoJuguete unPerro}

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro 
        |permanenciaMin unPerro || esDeRazaExtravagante unPerro = darRegalo unPerro
        |otherwise = unPerro

modificarJuguete :: ([juguete] -> [juguete]) -> Perro -> Perro
modificarJuguete unaFucion unPerro = unPerro {juguetesFav = (unaFuncion.juguetesFav) unPerro}

perdidaJuguete :: Perro -> Perro
perdidaDeJuguete  unPerro = modificarJuguete (drop 1) unPerro

diaDeCampo :: Perro -> Perro
diaDeCampo = (jugar.perdidaDeJuguete) unPerro

zara :: Perro
zara = unPerro "Dálmata" ["pelota","mantita"] 90 80
