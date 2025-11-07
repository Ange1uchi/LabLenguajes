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
    , "  - tomar <objeto> (alias: coger)"
    , "  - desc <objeto>"
    , "  - usar <objeto>" 
    , "  - buscar"
    , "  - mirar"
    , "  - inventario (alias: inv)"
    , "  - salir"
    ]

-- Punto de entrada del programa
main :: IO ()
main = do
    -- Mensaje inicial mientras se carga el mundo desde el archivo
    putStrLn "Despertando..."
    putStrLn ("Acabas de despertar en la sala de estar de un apartamento que reconoces, \n" ++
          "pero algo ha cambiado: todas las ventanas están selladas y las puertas, bloqueadas. \n" ++
          "Anoche, durante una \"conversación\" particularmente intensa sobre el futuro de la relación, \n" ++
          "tu ex pareja, la brillante y un poco obsesiva Dra. ALexa, decidió que la mejor manera de resolver vuestros problemas era... \n" ++
          "dándote un tiempo a solas. Te ha encerrado en su peculiar \"centro de desintoxicación emocional\" y,\n" ++
          "la única forma de escapar es resolviendo los puzzles que dejó."
         )
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
        Just cmd -> do
            -- processCommand devuelve un mensaje para mostrar y el nuevo estado del juego
            let (msg, newState) = processCommand cmd state
            putStrLn msg
            -- Continúo el bucle con el estado actualizado
            gameLoop newState
