module Engine.Parser (parseCommand) where

import Engine.Types

parseCommand :: String -> Maybe Command
parseCommand input =
    case words input of
        ["ir", "norte"] -> Just (Ir Norte)
        ["ir", "sur"]   -> Just (Ir Sur)
        ["ir", "este"]  -> Just (Ir Este)
        ["ir", "oeste"] -> Just (Ir Oeste)
        ["mirar"]       -> Just Mirar
        ["inventario"]  -> Just Inventario
        ["inv"]         -> Just Inventario
        ["salir"]       -> Just Salir
        ["tomar", obj]  -> Just (Tomar obj)
        ["coger", obj]  -> Just (Tomar obj)
        ["desc", obj]   -> Just (Desc obj)  
        ["usar", obj]   -> Just (Usar obj)  
        ["buscar"]      -> Just Buscar
        _               -> Nothing
