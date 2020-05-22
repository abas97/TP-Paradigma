import Text.Show.Functions()
import Data.Foldable



main :: IO()
main = return()

type Desgaste = Float
type Patente = String
type Fecha = (Float, Float, Float)
 
-- Definiciones base
anio :: Fecha -> Float
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Float,
 temperaturaAgua :: Float,
 ultimoArreglo :: Fecha
} deriving (Show)

autoPrueba :: Auto
autoPrueba = Auto  "DJV214" [0.1, 0.4, 0.2, 0] 0 0 (20,10,2016)

autoPrueba2 :: Auto
autoPrueba2 = Auto  "DJV214" [0.1, 0.4, 0.2, 0.1] 0 0 (20,10,2016)

autoPrueba3 :: Auto
autoPrueba3 = Auto  "DJV214" [0.1, 0.1, 0.1, 0] 0 0 (20,10,2016)

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
calculoPatental numeroPatente |(( last numeroPatente) =='4')= (length numeroPatente)*3000
                              |otherwise = 20000


costoSegunPatente :: Patente->Int
costoSegunPatente numeroPatente |(length numeroPatente == 7) =12500 
                                | verificacionPatente numeroPatente = calculoPatental numeroPatente
                                |otherwise =15000
                          

costoReparacion :: Auto->Int
costoReparacion auto = (costoSegunPatente.patente) auto

esAutoPeligroso :: Auto->Bool
esAutoPeligroso auto = ((>0.5).desgastePrimerRueda) auto

desgastePrimerRueda :: Auto->Float
desgastePrimerRueda auto = (head.desgasteLlantas) auto

necesitaRevision :: Auto -> Bool
necesitaRevision auto = (<= 2015) . anioDeArreglo $ auto

anioDeArreglo :: Auto -> Float
anioDeArreglo auto = (anio . ultimoArreglo)  auto



trabajadorAlfa :: Auto -> Auto
trabajadorAlfa auto 
     | (>= 2000) . rpm $ auto = bajarRevoluciones auto
     | otherwise            = auto

bajarRevoluciones :: Auto -> Auto
bajarRevoluciones auto = auto { rpm = 2000 }

trabajadorBravo :: Auto -> Auto
trabajadorBravo auto = auto { desgasteLlantas = [0] }

trabajadorCharly :: Auto -> Auto
trabajadorCharly auto = trabajadorAlfa . trabajadorBravo $ auto

trabajadorTango :: Auto -> Auto
trabajadorTango auto = auto

trabajadorZulu :: Auto -> Auto
trabajadorZulu auto = trabajadorLima . modificarTemperatura $ auto

modificarTemperatura :: Auto -> Auto
modificarTemperatura auto = auto { temperaturaAgua = 90 }

trabajadorLima :: Auto -> Auto
trabajadorLima auto = auto {desgasteLlantas = [0.0, 0.0] ++ ((drop 2) . desgasteLlantas $ auto)}

 {--(Común para ambos integrantes) 
Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
los autos ubicados en la posición par deben tener una cantidad de desgaste par
asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10. Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) es conveniente utilizar la función round.

Solamente se puede utilizar recursividad en este punto.

BONUS: Evitar repetición de código.

Casos de prueba a definir
Condición
Qué se espera
Esta lista de autos: un auto con desgaste de cubiertas [0.1, 0.4, 0.2, 0], otro auto con desgaste [0.2, 0.5, 0.6, 0.1], y otro con desgaste [0.1, 0.1, 0.1, 0]
Está ordenado según el criterio del enunciado
Esta lista de autos: un auto con desgaste de cubiertas [0.1, 0.4, 0.2, 0], otro auto con desgaste [0.3, 0.5, 0.6, 0.1], y otro con desgaste [0.1, 0.1, 0.1, 0]
No está ordenado según el criterio del enunciado
Esta lista de autos: un auto con desgaste de cubiertas [0.1, 0.4, 0.2, 0]
Está ordenado según el criterio del enunciado
Esta lista de autos: un auto con desgaste de cubiertas [0.1, 0.4, 0.2, 0.1]
No está ordenado según el criterio del enunciado--}


sumadeDesgaste :: [Auto] -> [Float]
sumadeDesgaste []            = []
sumadeDesgaste (cabeza:cola) = [((*10).sum.desgasteLlantas) cabeza] ++ suma cola


ordenamiento :: [Float]->Bool
ordenamiento []= True
ordenamiento lista | even ((round.head )lista) == (even.length)lista= ordenamiento (tail lista)
                   |otherwise =False

ordenamientoDeAutos :: [Auto]->Bool
ordenamientoDeAutos lista = (ordenamiento.sumadeDesgaste) lista