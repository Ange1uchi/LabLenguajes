module Engine.Persistence (loadWorldData) where

import Engine.Types
import qualified Data.Map as Map
import System.IO

-- Versión simplificada: carga un mundo fijo de ejemplo
loadWorldData :: FilePath -> IO (Either String (Map.Map RoomName Room, Map.Map ItemName Item))
loadWorldData _ = do
    let llave = Item "llaves" "Unas llaves brillantes."
        cuchillo = Item "cuchillo" "Un cuchillo afilado."
        salaEstar = Room "Sala de Estar" "Una acogedora sala de estar. Hay una puerta al norte."
                        (Map.fromList [(Norte, "Cocina")]) ["llaves"]
        cocina = Room "Cocina" "Una cocina desordenada. Hay una puerta al sur."
                        (Map.fromList [(Sur, "Sala de Estar")]) ["cuchillo"]
    return $ Right ( Map.fromList [("Sala de Estar", salaEstar), ("Cocina", cocina)]
                   , Map.fromList [("llaves", llave), ("cuchillo", cuchillo)] )
