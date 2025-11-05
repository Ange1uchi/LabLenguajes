module Main where

import Engine.Persistence (loadWorldData)
import Engine.Parser (parseCommand)
import Engine.Core (processCommand)
import Engine.Types
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Cargando mundo..."
    result <- loadWorldData "data/mundo.txt"
    case result of
        Left err -> putStrLn $ "Error al cargar el mundo: " ++ err
        Right (rooms, items) -> do
            let initialState = GameState { currentRoom = "Sala de Estar"
                                         , inventory = Map.empty
                                         , world = rooms
                                         }
            putStrLn "¡Bienvenido al juego de aventura!"
            gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case parseCommand input of
        Nothing -> do
            putStrLn "No entiendo ese comando."
            gameLoop state
        Just Salir -> putStrLn "Gracias por jugar."
        Just cmd -> do
            let (msg, newState) = processCommand cmd state
            putStrLn msg
            gameLoop newState
