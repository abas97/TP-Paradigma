import Text.Show.Functions()




main :: IO()
main = return()

type Desgaste = Int
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving (Show)

autoPrueba :: Auto
autoPrueba = Auto  "DFH029" [] 0 0 (20,10,20)

--costoReparacion :: Auto->Int
--costoReparacion auto 

{--Saber el costo de reparación de un auto
si la patente tiene 7 dígitos, es $ 12.500
si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
que es $ 3.000 * la longitud para las patentes que terminen en 4
o $ 20.000 para el resto de las patentes
de lo contrario, se le cobra $ 15000
--}

buscarLetra:: Char->Bool
buscarLetra 'D'=True
buscarLetra 'J'=True
buscarLetra 'N'=True
buscarLetra 'B'=True
buscarLetra _=False

cortarLetras :: Patente->String
cortarLetras numeroPatente = filter buscarLetra numeroPatente

verificacionPatente :: Patente->Bool
verificacionPatente numeroPatente= (( == "DJ").cortarLetras) numeroPatente || (( == "NB").cortarLetras) numeroPatente

calculoPatental :: Patente -> Int
calculoPatental numeroPatente |(((head.reverse)numeroPatente) =='4')= (length numeroPatente)*3000
                              |otherwise = 20000

nuevaPatente :: Patente->Int
nuevaPatente numeroPatente | verificacionPatente numeroPatente = calculoPatental numeroPatente
                           |otherwise =15000

tipoPatente :: Patente->Int
tipoPatente numeroPatente |(length numeroPatente == 7) =12500 
                          | otherwise =nuevaPatente numeroPatente

costoReparacion :: Auto->Int
costoReparacion auto = (tipoPatente.patente) auto

