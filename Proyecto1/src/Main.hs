module Main where

-- Importo funciones para cargar el mundo desde archivos
import Engine.Persistence (loadWorldData)
-- Importo el parser de comandos (parseCommand convierte una línea en un comando)
import Engine.Parser (parseCommand)
-- Lógica principal de procesamiento de comandos
import Engine.Core (processCommand)
-- Tipos usados en el juego (p.ej. GameState, Command, etc.)
import Engine.Types
-- Mapa para representar inventarios y colecciones de habitaciones
import qualified Data.Map as Map
-- Para asegurar que el prompt se muestre antes de leer la entrada
import System.IO (hFlush, stdout)

--- Mensaje de ayuda con los comandos disponibles
availableCommands :: String
availableCommands = unlines
    [ "Comandos disponibles:"
    , "  - ir <direccion> (ej: ir norte)"
    , "  - tomar <objeto> (alias: coger) agarrar objeto"
    , "  - desc <objeto> (ver descripcion del objeto)" 
    , "  - usar <objeto> (usar objeto)" 
    , "  - buscar (para buscar en la habitacion)"
    , "  - mirar (ver objetos y descripcion de la habitacion)"
    , "  - inventario (alias: inv, para ver tu inventario)"
    , "  - salir (para salir del juego)"
    ]

-- Punto de entrada del programa
main :: IO ()
main = do
    -- Mensaje inicial mientras se carga el mundo desde el archivo
    putStrLn "Despertando..."
    putStrLn ("Te despiertas en la sala de estar de un apartamento que conoces, \n" ++
          "pero todo parece salido de una feria: guirnaldas, pistas pegadas por todas partes y un robot que aplaude. \n" ++
          "Tu ex, la brillante y algo excéntrica Dra. Alexa, decidió que la mejor forma de arreglar las cosas era organizarte una \"experiencia inmersiva\": \n" ++
          "su particular \"centro de desintoxicación emocional\" convertido en un extravagante escape room lleno de acertijos, chistes malos y premios cuestionables. \n" ++
          "Si quieres salir de este espectáculo doméstico tendrás que resolver sus puzzles... y, con suerte, llevarte la mejor recompensa: ¡la paz mental!")
    -- Intento cargar los datos del mundo desde el archivo data/mundo.txt
    result <- loadWorldData "Data/mundo.txt"
    case result of
        -- Si ocurre un error al cargar, lo notifico al usuario
        Left err -> putStrLn $ "Error al cargar el mundo: " ++ err
        -- Si existe el mundo, construyo el estado inicial y comienzo el juego
        Right (rooms, itemsDB) -> do
            -- Estado inicial del juego:
            --  - currentRoom: nombre de la habitación donde inicia el jugador
            --  - inventory: inventario del jugador (vacío al inicio)
            --  - world: mapa de habitaciones cargado desde el archivo
            --  - itemDatabase: base de datos de ítems, para referencias y descripciones
            let initialState = GameState { currentRoom = "Sala de Estar"
                                         , inventory = Map.empty
                                         , world = rooms
                                         , itemDatabase = itemsDB
                                         , doorPowered = False
                                         }
            putStrLn "Lévantate y escapa si puedes..."

            putStrLn availableCommands
            putStrLn ""
            -- Inicio el ciclo principal del juego con el estado inicial
            gameLoop initialState

-- Bucle principal que lee comandos del usuario y actualiza el estado
gameLoop :: GameState -> IO ()
gameLoop state = do
    -- Muestro prompt y fuerzo el flush para que el prompt aparezca inmediatamente
    putStr "> "
    hFlush stdout
    -- Leo la línea de entrada del usuario
    input <- getLine
    -- Intento parsear la entrada a un comando conocido
    case parseCommand input of
        Nothing -> do
            -- Si no se reconoce el comando, muestro mensaje y sigo con el mismo estado
            putStrLn "No entiendo ese comando."
            gameLoop state
        -- Si el comando es Salir (asumimos que existe ese constructor en Engine.Types),
        -- finalizo el juego mostrando un mensaje de despedida
        Just Salir -> putStrLn "Gracias por jugar."
        -- Para cualquier otro comando válido, lo proceso y actualizo el estado
        -- Para cualquier otro comando válido, lo proceso y actualizo el estado
        Just cmd -> do
            -- processCommand devuelve un mensaje para mostrar y el nuevo estado del juego
            let (msg, newState) = processCommand cmd state
            putStrLn msg
            
            -- Asegúrate de que el 'if' y el 'else' estén en la misma columna
            if currentRoom newState == "Salida Final"
                then putStrLn ("¡Felicidades! Has ganado y escapado de la loca esa \n" ++ "tranquilo mi rey, ya encontraras a alguien... ")
                else gameLoop newState
