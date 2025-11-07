module Engine.Core (processCommand) where

import Engine.Types
import qualified Data.Map as Map
import Data.List (delete)

-- processCommand recibe un Command y el GameState actual y devuelve un par (String, GameState) con el mensaje y el estado actualizado.
-- En Haskell usamos pattern matching en la firma de la función para distinguir cada constructor de Command (Tomar, Ir, Mirar, ...).
processCommand :: Command -> GameState -> (String, GameState)

-- Aquí hacemos pattern matching por el constructor 'Tomar itemName'. itemName queda enlazado al nombre del objeto que el jugador quiere tomar.
processCommand (Tomar itemName) state =
    -- Map.lookup devuelve Maybe Room; usamos un case para hacer pattern matching
    -- sobre Nothing (la sala no existe) o Just currentRoomData (sí existe).
    case Map.lookup (currentRoom state) (world state) of
        Nothing -> ("Error: La sala actual no existe en el mundo.", state)
        Just currentRoomData ->
            -- Primero verificamos si el item está en la lista de items de la sala.
            -- La expresión (itemName `elem` items currentRoomData) es booleana.
            if itemName `elem` (items currentRoomData)
                then
                    -- Si el item aparece en la sala, buscamos sus detalles en itemDatabase.
                    -- De nuevo Map.lookup produce Maybe; manejamos Nothing y Just.
                    case Map.lookup itemName (itemDatabase state) of
                        Nothing -> ("Error: El objeto '" ++ itemName ++ "' existe en la sala, pero no tiene detalles de item.", state)
                        Just itemDetail ->
                            -- Cuando encontramos los detalles, construimos un nuevo estado:
                            --  - quitamos el item de la lista de la sala con delete
                            --  - actualizamos el mapa world con la sala modificada (Map.insert)
                            --  - añadimos el item al inventario (Map.insert)
                            --  - creamos newState usando record update sobre state
                            let
                                newRoomItems = delete itemName (items currentRoomData)
                                newRoom = currentRoomData { items = newRoomItems }
                                newWorld = Map.insert (roomName currentRoomData) newRoom (world state)
                                newInventory = Map.insert itemName itemDetail (inventory state)
                                newState = state { world = newWorld, inventory = newInventory }
                            in ("Has tomado " ++ itemName ++ ".", newState)
                else ("El objeto " ++ itemName ++ " no está aquí para ser tomado.", state)

-- Pattern matching para moverse: 'Ir dir' enlaza la dirección solicitada.
processCommand (Ir dir) state =
    case Map.lookup (currentRoom state) (world state) of
        Nothing -> ("No sé dónde estás.", state)
        Just room ->
            -- Aquí miramos en el mapa de salidas (exits) de la sala.
            -- Map.lookup sobre exits también devuelve Maybe; usamos pattern matching
            -- para distinguir si hay una sala destino en esa dirección.
            case Map.lookup dir (exits room) of
                Nothing -> ("No puedes ir en esa dirección.", state)
                Just next ->
                    ("Te mueves hacia " ++ show dir ++ ".", state { currentRoom = next })

-- 'Mirar' no tiene parámetros; usamos pattern matching directo en la firma.
processCommand Mirar state =
    -- Otra vez Map.lookup para obtener la sala actual; si existe devolvemos su descripción.
    case Map.lookup (currentRoom state) (world state) of
        Just room -> (roomDesc room, state)
        Nothing -> ("No hay nada que mirar.", state)

-- 'Inventario' lista las claves (nombres de los objetos) del inventario.
processCommand Inventario state =
    let invList = Map.keys (inventory state)
    in if null invList
       then ("Tu inventario está vacío.", state)
       else ("Tienes: " ++ unwords invList, state)

-- 'buscar' muestra los objetos en la sala actual.
processCommand Buscar state =
    case Map.lookup (currentRoom state) (world state) of
        Nothing -> ("No sé dónde estás.", state)
        Just room ->
            case items room of
                [] -> ("Buscas en la sala pero no encuentras nada destacable.", state)
                objs -> ("Puedes ver: " ++ unwords objs, state)

-- 'Desc itemName' describe un objeto en el inventario.
processCommand (Desc itemName) state =
    case Map.lookup itemName (inventory state) of
        Nothing -> ("No tienes " ++ itemName ++ " en tu inventario para describirlo.", state)
        Just item ->
            let
                -- Usamos Pattern Matching para extraer el nombre y la descripción del registro 'item'
                -- Esto evita el uso directo del selector 'itemName' y su colisión.
                Item { itemName = name, itemDesc = desc } = item 
            in ("**" ++ name ++ "**: " ++ desc, state)

-- 'Usar itemName' intenta usar un objeto del inventario.
processCommand (Usar itemName) state
    -- Verificar si el jugador tiene el objeto
    | Map.notMember itemName (inventory state) =
        ("No tienes " ++ itemName ++ " en tu inventario.", state)

    -- Lógica del Puzle (Debe ocurrir en Cuarto de la Salida)
    | currentRoom state == "Cuarto de la Salida" =
        case itemName of
            "llave_maestra" ->
                -- Si usa la llave maestra en la sala de salida
                case Map.lookup "Cuarto de la Salida" (world state) of
                    Nothing -> ("Error interno: Sala de Salida no encontrada.", state)
                    Just exitRoom ->
                        -- Define la nueva salida (Salida Final)
                        let newExits = Map.insert Norte "Salida Final" (exits exitRoom)
                            newExitRoom = exitRoom { roomDesc = "La puerta está desbloqueada.", exits = newExits }
                            newWorld = Map.insert "Cuarto de la Salida" newExitRoom (world state)
                            newState = state { world = newWorld }
                        in ("¡Felicidades! La llave maestra encaja en la cerradura y la puerta se abre.\nAhora puedes ir al Norte.", newState)

            "cable" ->
                -- Objeto incorrecto
                ("El cable no tiene utilidad aquí.", state)

            -- Cualquier otro objeto que no resuelva el puzle
            _ ->
                ("No sabes cómo usar " ++ itemName ++ " en esta sala.", state)

    -- Lógica General (Usar en cualquier otra sala)
    | otherwise =
        ("No hay nada en esta sala en lo que puedas usar " ++ itemName ++ ".", state)

-- 'Salir' termina el juego (aquí solo devolvemos el estado sin cambios).
processCommand Salir state = ("", state)

