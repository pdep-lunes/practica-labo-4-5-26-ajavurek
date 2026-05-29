module Parcial where
import Text.Show.Functions()

data Perro = UnPerro {
    raza :: String,
    juguetesFav :: [String],
    tiempo :: Int,
    energia :: Int
} deriving (Show, Eq)

type Ejercicio = Perro -> Perro

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [Actividad]
} deriving Show

type Actividad = (Ejercicio, Int)

jugar :: Perro -> Perro
jugar = modificarEnergia (-10)

modificarEnergia :: Int -> Perro -> Perro
modificarEnergia cantidadEnergia unPerro = unPerro {energia = max (energia unPerro + cantidadEnergia) 0}

ladrar :: Int -> Perro -> Perro
ladrar unLadrido = modificarEnergia (div unLadrido 2)

regalar :: String -> Ejercicio
regalar unJuguete = modificarJuguete (unJuguete :)

modificarJuguete :: ([String] -> [String]) -> Perro -> Perro
modificarJuguete unaFuncion unPerro = unPerro {juguetesFav = unaFuncion (juguetesFav unPerro)}

permanenciaMin :: Perro -> Bool
permanenciaMin = (>=50) . tiempo

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = raza unPerro == "Dálmata" || raza unPerro == "Pomerania"

darRegalo :: Perro -> Perro
darRegalo unPerro = regalar "Peine de goma" (unPerro { energia = 100})

diaDeSpa :: Ejercicio
diaDeSpa unPerro
        | permanenciaMin unPerro || esDeRazaExtravagante unPerro = darRegalo unPerro
        | otherwise = unPerro

perdidaJuguete :: Perro -> Perro
perdidaJuguete = modificarJuguete (drop 1)

diaDeCampo :: Ejercicio
diaDeCampo = perdidaJuguete . jugar

zara :: Perro
zara = UnPerro {
    raza = "Dálmata" ,
    juguetesFav = ["Pelota", "Mantita"] ,
    tiempo = 90 ,
    energia = 80
}

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia {
    nombre = "PdePerritos" ,
    rutina =  [(jugar, 30), (ladrar 18, 20), (regalar "Pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]
}

tiempoTotalRutina :: Guarderia -> Int
tiempoTotalRutina = sum . map snd . rutina

puedeEstar :: Perro -> Guarderia -> Bool
puedeEstar unPerro unaGuarderia = tiempo unPerro > tiempoTotalRutina unaGuarderia

esPerroResponsable :: Perro -> Bool
esPerroResponsable = (>3) . length . juguetesFav . diaDeCampo
