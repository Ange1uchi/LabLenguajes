-- Tests.hs - Pruebas Unitarias para Tarea 1
-- Usa el framework HUnit. Para compilar y correr:
-- 1. Asegúrate de tener la librería HUnit instalada (e.g., usando cabal o stack).
-- 2. Compila: ghc --make Tests.hs
-- 3. Ejecuta: ./Tests
-------------------------------------------------------------------------------

import Tarea1
import Test.HUnit
import Data.Char (toUpper) -- Necesario para Problema 3 si la implementación usa toUpper

-------------------------------------------------------------------------------
-- 1. Pruebas para esPalindromo
-------------------------------------------------------------------------------

testPalindromo :: Test
testPalindromo = "Palíndromo simple" ~: esPalindromo "oso" ~?= True

testPalindromoVacio :: Test
testPalindromoVacio = "Palíndromo vacío" ~: esPalindromo "" ~?= True

testPalindromoUnCaracter :: Test
testPalindromoUnCaracter = "Palíndromo un carácter" ~: esPalindromo "a" ~?= True

testPalindromoNoPalindromo :: Test
testPalindromoNoPalindromo = "No palíndromo" ~: esPalindromo "casa" ~?= False

testPalindromoGrande :: Test
testPalindromoGrande = "Palíndromo largo" ~: esPalindromo (replicate 1000 'a') ~?= True

-------------------------------------------------------------------------------
-- 2. Pruebas para productoParesRec
-------------------------------------------------------------------------------

testProductoParesRec :: Test
testProductoParesRec = "Solo pares" ~: productoParesRec [2, 4, 6] ~?= 48

testProductoParesRecVacio :: Test
testProductoParesRecVacio = "Lista vacía" ~: productoParesRec [] ~?= 1

testProductoParesRecSinPares :: Test
testProductoParesRecSinPares = "Sin pares" ~: productoParesRec [1,3,5] ~?= 1

testProductoParesRecConCero :: Test
testProductoParesRecConCero = "Incluye cero" ~: productoParesRec [0,2,4] ~?= 0

testProductoParesRecGrande :: Test
testProductoParesRecGrande = "Lista grande" ~: productoParesRec (replicate 100 2) ~?= 2^100

-------------------------------------------------------------------------------
-- 3. Pruebas para parsearCondicional
-------------------------------------------------------------------------------

-- Nota: Para que estas pruebas funcionen correctamente, la implementación
-- del alumno debe usar funciones estándar de Haskell para toUpper y para
-- intentar parsear a Int (e.g., readMaybe).

testParsearCondicional :: Test
testParsearCondicional = "Solo números válidos" ~: parsearCondicional ["42"] ~?= [Right 42]

testParsearCondicionalVacio :: Test
testParsearCondicionalVacio = "Lista vacía" ~: parsearCondicional [] ~?= []

testParsearCondicionalMixto :: Test
testParsearCondicionalMixto = "Mixto" ~: parsearCondicional ["12","abc","34"] ~?= [Right 12, Left "ABC", Right 34]

testParsearCondicionalNegativo :: Test
testParsearCondicionalNegativo = "Negativo" ~: parsearCondicional ["-5"] ~?= [Left "-5"]

-------------------------------------------------------------------------------
-- 4. Pruebas para sumaAcumuladaCondicional
-------------------------------------------------------------------------------

testSumaCondicional :: Test
testSumaCondicional = "Umbral 5.0, todos mayores" ~: sumaAcumuladaCondicional 5.0 [6.0, 7.0, 8.0] ~?= 21.0

testSumaCondicionalVacio :: Test
testSumaCondicionalVacio = "Lista vacía" ~: sumaAcumuladaCondicional 5.0 [] ~?= 0.0

testSumaCondicionalTodosMenores :: Test
testSumaCondicionalTodosMenores = "Todos menores" ~: sumaAcumuladaCondicional 10.0 [1.0,2.0,3.0] ~?= 0.0

testSumaCondicionalMixto :: Test
testSumaCondicionalMixto = "Mixto" ~: sumaAcumuladaCondicional 2.0 [1.0,2.0,3.0,4.0] ~?= 7.0

-------------------------------------------------------------------------------
-- 5. Pruebas para coordenadasImpares
-------------------------------------------------------------------------------

testCoordenadasImpares :: Test
testCoordenadasImpares = "N=2" ~: coordenadasImpares 2 ~?= [(1, 2), (2, 1)]

testCoordenadasImparesN1 :: Test
testCoordenadasImparesN1 = "N=1" ~: coordenadasImpares 1 ~?= []

testCoordenadasImparesN3 :: Test
testCoordenadasImparesN3 = "N=3" ~: coordenadasImpares 3 ~?= [(1,2),(2,1),(2,3),(3,2)]

-------------------------------------------------------------------------------
-- 6. Pruebas para descomponerListaSegura
-------------------------------------------------------------------------------

testDescomponerListaSegura :: Test
testDescomponerListaSegura = "Un elemento" ~: descomponerListaSegura [1] ~?= Just (1, [])

testDescomponerListaSeguraVacia :: Test
testDescomponerListaSeguraVacia = "Lista vacía" ~: descomponerListaSegura ([] :: [Int]) ~?= Nothing

testDescomponerListaSeguraLarga :: Test
testDescomponerListaSeguraLarga = "Lista larga" ~: descomponerListaSegura [1..1000] ~?= Just (1, [2..1000])

-------------------------------------------------------------------------------
-- Ejecución Principal
-------------------------------------------------------------------------------

-- Lista principal de todos los tests
allTests :: Test
allTests = TestList
    [ TestLabel "Problema 1: esPalindromo" testPalindromo
    , TestLabel "Palíndromo vacío" testPalindromoVacio
    , TestLabel "Palíndromo un carácter" testPalindromoUnCaracter
    , TestLabel "No palíndromo" testPalindromoNoPalindromo
    , TestLabel "Palíndromo largo" testPalindromoGrande

    , TestLabel "Problema 2: productoParesRec" testProductoParesRec
    , TestLabel "Producto pares vacía" testProductoParesRecVacio
    , TestLabel "Producto pares sin pares" testProductoParesRecSinPares
    , TestLabel "Producto pares con cero" testProductoParesRecConCero
    , TestLabel "Producto pares grande" testProductoParesRecGrande

    , TestLabel "Problema 3: parsearCondicional" testParsearCondicional
    , TestLabel "Parsear condicional vacío" testParsearCondicionalVacio
    , TestLabel "Parsear condicional mixto" testParsearCondicionalMixto
    , TestLabel "Parsear condicional negativo" testParsearCondicionalNegativo

    , TestLabel "Problema 4: sumaAcumuladaCondicional" testSumaCondicional
    , TestLabel "Suma condicional vacía" testSumaCondicionalVacio
    , TestLabel "Suma condicional todos menores" testSumaCondicionalTodosMenores
    , TestLabel "Suma condicional mixto" testSumaCondicionalMixto

    , TestLabel "Problema 5: coordenadasImpares" testCoordenadasImpares
    , TestLabel "Coordenadas impares N=1" testCoordenadasImparesN1
    , TestLabel "Coordenadas impares N=3" testCoordenadasImparesN3

    , TestLabel "Problema 6: descomponerListaSegura" testDescomponerListaSegura
    , TestLabel "Descomponer lista vacía" testDescomponerListaSeguraVacia
    , TestLabel "Descomponer lista larga" testDescomponerListaSeguraLarga
    ]

main :: IO ()
main = runTestTTAndExit allTests
