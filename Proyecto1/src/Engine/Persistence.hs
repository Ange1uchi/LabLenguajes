module Engine.Persistence (loadWorldData) where

import Engine.Types
import qualified Data.Map as Map
import System.IO
import Data.List.Split (splitOn)
import Data.Char (toLower)
import Control.Exception (catch)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

-- Intento principal: leer un archivo y devolver los mapas de salas y objetos.
-- Uso Either para propagar errores de parsing de forma explícita.
loadWorldData :: FilePath -> IO (Either String (Map.Map RoomName Room, Map.Map ItemName Item))
loadWorldData filePath = do
    result <- safeReadFile filePath
    case result of
        Left err -> return $ Left ("Error al leer el archivo: " ++ err)
        Right content -> return $ parseWorld content

-- Manejo simple de errores de IO con catch. Aprendí que catch requiere especificar el tipo de excepción.
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile fp = do
    catch (do content <- readFile fp
              return (Right content))
          (\e -> return (Left (show (e :: IOError))))

-- Parseo del contenido en bloques y construcción de los mapas.
-- Observo que usare pattern matching sobre listas de resultados para detectar errores.
parseWorld :: String -> Either String (Map.Map RoomName Room, Map.Map ItemName Item)
parseWorld content =
    let
        blocks = filter (not . null) . map trim . splitOn "---" $ content
        (itemBlocks, roomBlocks) = separateBlocks blocks

        parsedItems = map parseItemBlock itemBlocks
        itemsMap = Map.fromList [ (itemName i, i) | Right i <- parsedItems ]

        parsedRooms = map parseRoomBlock roomBlocks
        roomsMap = Map.fromList [ (roomName r, r) | Right r <- parsedRooms ]
    in case (any isLeft parsedItems, any isLeft parsedRooms) of
        (True, _) -> Left "Error(es) en el formato de bloque ITEM."
        (_, True) -> Left "Error(es) en el formato de bloque SALA."
        _         -> Right (roomsMap, itemsMap)

-- Quitar espacios de los extremos. Aprendí a componer funciones para reutilizar comportamiento.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') . dropWhile (== '\n')

-- Detector sencillo de Left en un Either. Útil para filtrar resultados fallidos.
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- Separa bloques que comienzan con "ITEM:" de los que no.
-- Aquí uso foldr y una función auxiliar con pattern matching implícito en if.
separateBlocks :: [String] -> ([String], [String])
separateBlocks = foldr (\b (is, rs) -> 
    if "ITEM:" `isPrefixOf` b 
       then (b:is, rs) 
       else (is, b:rs)
    ) ([], [])
    where isPrefixOf prefix str = take (length prefix) str == prefix

-- Parsea un bloque ITEM. Uso pattern matching con case para extraer nombre y descripción.
parseItemBlock :: String -> Either String Item
parseItemBlock block =
    let lines' = filter (not . null) . map trim . lines $ block
        itemLine = safeLookup "ITEM:" lines'
        descLine = safeLookup "DESC:" lines'
        itemName = case itemLine of { Just s -> trim . drop (length "ITEM:") $ s; _ -> "" }
        itemDesc = case descLine of { Just s -> trim . drop (length "DESC:") $ s; _ -> "" }
    in if null itemName || null itemDesc
       then Left $ "Bloque ITEM mal formado: " ++ block
       else Right (Item itemName itemDesc)

-- Parsea un bloque SALA. Uso mapM para combinar posibles errores al parsear salidas.
parseRoomBlock :: String -> Either String Room
parseRoomBlock block =
    let lines' = filter (not . null) . map trim . lines $ block
        nameLine = safeLookup "SALA:" lines'
        descLine = safeLookup "DESC:" lines'
        
        roomName = case nameLine of { Just s -> trim . drop (length "SALA:") $ s; _ -> "" }
        roomDesc = case descLine of { Just s -> trim . drop (length "DESC:") $ s; _ -> "" }
        
        exitLines = filter ("SALIDA:" `isPrefixOf` ) lines'
        objectLines = filter ("OBJETO:" `isPrefixOf` ) lines'
        
        exitsMapResult = mapM parseExit exitLines
        itemsList = map (trim . drop (length "OBJETO:")) objectLines
        
    in if null roomName || null roomDesc
       then Left $ "Bloque SALA mal formado: " ++ block
       else case exitsMapResult of
           Left err -> Left err
           Right exitsList -> Right (Room roomName roomDesc (Map.fromList exitsList) itemsList)

-- Parsea una línea SALIDA: uso splitOn y luego pattern matching en el resultado.
parseExit :: String -> Either String (Direction, RoomName)
parseExit line =
    let parts = splitOn "->" (trim . drop (length "SALIDA:") $ line)
    in case parts of
        [dirStr, roomStr] ->
            case readDirection (trim dirStr) of
                Just dir -> Right (dir, trim roomStr)
                Nothing  -> Left $ "Dirección inválida en salida: " ++ dirStr
        _ -> Left $ "Formato SALIDA inválido: " ++ line

-- Convierte un String a Direction. Aquí uso pattern matching en el resultado de map toLower.
readDirection :: String -> Maybe Direction
readDirection s = case map toLower s of
    "norte" -> Just Norte
    "sur"   -> Just Sur
    "este"  -> Just Este
    "oeste" -> Just Oeste
    _       -> Nothing

-- Busco la primera línea que comienza con el prefijo dado.
safeLookup :: String -> [String] -> Maybe String
safeLookup prefix = listToMaybe . filter (prefix `isPrefixOf`)
