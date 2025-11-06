module Engine.Types where

import qualified Data.Map as Map

type RoomName = String
type ItemName = String

data Direction = Norte | Sur | Este | Oeste
    deriving (Show, Eq, Ord)

data Command
    = Ir Direction
    | Mirar
    | Tomar ItemName
    | Inventario
    | Desc ItemName
    | Buscar 
    | Salir
    deriving (Show, Eq)

data Item = Item
    { itemName :: ItemName
    , itemDesc :: String
    } deriving (Show)

data Room = Room
    { roomName :: RoomName
    , roomDesc :: String
    , exits :: Map.Map Direction RoomName
    , items :: [ItemName]
    } deriving (Show)

data GameState = GameState
    { currentRoom :: RoomName
    , inventory :: Map.Map ItemName Item
    , world :: Map.Map RoomName Room
    , itemDatabase :: Map.Map ItemName Item 
    } deriving (Show)
