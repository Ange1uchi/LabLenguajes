# Proyecto 1 - Motor de Aventura de Texto
**CI3661 - Laboratorio de Lenguajes de Programación**  
**Universidad Simón Bolívar — Septiembre–Diciembre 2025**

---

### Integrantes
Angel Valero    1810436
Gabriel Seijas  1900036

---

## Descripción del Proyecto

Este proyecto consiste en el desarrollo de un **motor de aventura de texto** implementado en **Haskell**, diseñado para ejecutar historias interactivas basadas en texto.  

El objetivo principal es crear un sistema **modular, puro y reutilizable**, donde la lógica del juego esté separada de las operaciones de entrada/salida (I/O).  
El motor carga un archivo `mundo.txt` que define salas, objetos y sus relaciones, permitiendo que cualquier historia con ese formato pueda ejecutarse.

---

## Estructura del Proyecto

Proyecto1/
├── data/
│ └── mundo.txt
├── src/
│ ├── Main.hs
│ └── Engine/
│       └──Types.hs
│       └──Parser.hs
│       └──Core.hs
│       └──Persistence.hs
├── Proyecto1.cabal
├── stack.yaml
├── stack.yaml.lock
├── README.md
└── Setup.hs

---

###  Módulos Principales
| `Engine.Types` | Define los tipos de datos algebraicos y estructuras principales del juego (`GameState`, `Room`, `Item`, `Direction`, `Command`). |
| `Engine.Parser` | Convierte los comandos en texto introducidos por el usuario a valores del tipo `Command`. |
| `Engine.Core` | Contiene la lógica pura del juego. Implementa cómo cambian las salas, el inventario y los mensajes. |
| `Engine.Persistence` | Carga los datos del mundo desde el archivo `mundo.txt`. construyendo los mapas de salas y objetos. |
| `Main.hs` | Gestiona el bucle principal del juego (`gameLoop`) y las interacciones con el usuario (entrada/salida). |

---

## Ejecución del Proyecto

Este proyecto se gestiona con **Stack**. Se debe de tener Stack instalado.

### Pasos para Compilar y Ejecutar (Linux)

 **Compilar y construir:**
    En la raíz del proyecto (donde está el archivo `.cabal`):
    ```bash
    stack build
    ```
 **Ejecutar el juego:**
    ```bash
    stack exec Proyecto1
    ```
El juego se iniciará cargando el mundo desde `data/mundo.txt` y entrará en el bucle interactivo.

---

## Descripción y Justificación del Diseño

Este proyecto consiste en el desarrollo de un **motor de aventura de texto** implementado en **Haskell**, cuyo objetivo principal es crear un sistema **modular, puro y reutilizable**.El motor cumple con el requisito de estar **desacoplado del contenido** , ya que carga la definición completa del mundo (salas, objetos, salidas) desde el archivo `data/mundo.txt`.

## Archivo mundo.txt

El archivo mundo.txt implementa un pequeño "Escape Room" con un puzle de salida, demostrando la capacidad del motor para manejar salidas condicionales mediante el comando usar teniendo como objetivo desbloquear la puerta del Cuarto de la Salida para llegar a la Salida Final.

### Estructura del Proyecto y Separación de I/O
El diseño del proyecto se basa en la estricta **separación entre la lógica pura y los efectos de borde (I/O)**.

| Módulo | Responsabilidad | Naturaleza de la Función |
| :--- | :--- | :--- |
| **`Engine.Types`** | Definiciones de tipos (`GameState`, `Command`, etc.). | Pura |
| **`Engine.Parser`** | `parseCommand :: String -> Maybe Command`. Conversión de texto a comando. | [cite_start]Pura  |
| **`Engine.Core`** | **Corazón de la Lógica:** `processCommand :: Command -> GameState -> (String, GameState)`. Actualiza el estado sin I/O. | **Pura** |
| **`Engine.Persistence`** | Carga y *parsing* del archivo `mundo.txt`. | Híbrida (I/O y Pura)|
| **`Main.hs`** | Implementa el `gameLoop`. Único responsable de `getLine` y `putStrLn`. | [cite_start]Impura (I/O)|

**Separación I/O:** El `Main.hs` es el único que realiza llamadas impuras (`getLine`, `putStrLn`). El `Engine.Core` (que ejecuta el juego) es completamente puro, lo que hace que la lógica sea fácil de probar, predecible y libre de efectos laterales.

### Estructuras de Datos

Se eligió el tipo **`Data.Map`** sobre las listas de asociación simples para gestionar las estructuras de juego que requieren búsquedas rápidas por clave.

| Estructura                   | Tipo Elegido        | Ventaja Clave (Rendimiento) |
| **Mapa del Mundo (`world`)** | `Map RoomName Room` | Permite acceder a cualquier sala del mundo con una eficiencia de **$O(\log n)$**, crucial para transiciones rápidas de sala. |
| **Salidas de Sala (`exits`)**| `Map Direction RoomName` | Permite verificar si una dirección es válida y obtener la sala de destino en **$O(\log n)$**. |
| **Inventario (`inventory`)** | `Map ItemName Item` | Permite verificar la pertenencia y obtener los detalles del ítem en **$O(\log n)$**, necesaria para la lógica de `tomar` y `usar`. |


## Comandos Soportados

El motor soporta los siguientes comandos en español:

* `ir <direccion>` (ej. `ir norte`): Mueve al jugador si la salida existe.
* `mirar`: Vuelve a imprimir la descripción de la sala actual.
* `desc <objeto>`: Describe el objeto desde el inventario.
* `usar <objeto>`: se usa el objeto de la sala en cuestion. 
* `buscar`: te muestra los objetos que te encuentras en la sala.
* `tomar <objeto>` o `coger <objeto>`: Mueve el objeto de la sala al inventario del jugador.
* `inventario` o `inv`: Muestra el contenido del inventario del jugador.
* `salir`: Termina la ejecución del juego.



## Desde WSL se ejecuta de la siguiente forma
```bash
stack build
stack run

---