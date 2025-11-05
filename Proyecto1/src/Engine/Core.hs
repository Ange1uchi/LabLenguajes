module Engine.Core (processCommand) where

import Engine.Types
import qualified Data.Map as Map

processCommand :: Command -> GameState -> (String, GameState)
processCommand (Ir dir) state =
    case Map.lookup (currentRoom state) (world state) of
        Nothing -> ("No sé dónde estás.", state)
        Just room ->
            case Map.lookup dir (exits room) of
                Nothing -> ("No puedes ir en esa dirección.", state)
                Just next ->
                    ("Te mueves hacia " ++ show dir ++ ".", state { currentRoom = next })

processCommand Mirar state =
    case Map.lookup (currentRoom state) (world state) of
        Just room -> (roomDesc room, state)
        Nothing -> ("No hay nada que mirar.", state)

processCommand Inventario state =
    let invList = Map.keys (inventory state)
    in if null invList
       then ("Tu inventario está vacío.", state)
       else ("Tienes: " ++ unwords invList, state)

processCommand Salir state = ("", state)

processCommand _ state = ("Aún no implementado.", state)
