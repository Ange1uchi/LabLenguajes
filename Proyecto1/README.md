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

###  Módulos Principales
| `Engine.Types` | Define los tipos de datos algebraicos y estructuras principales del juego (`GameState`, `Room`, `Item`, `Direction`, `Command`). |
| `Engine.Parser` | Convierte los comandos en texto introducidos por el usuario a valores del tipo `Command`. |
| `Engine.Core` | Contiene la lógica pura del juego. Implementa cómo cambian las salas, el inventario y los mensajes. |
| `Engine.Persistence` | Carga los datos del mundo desde el archivo `mundo.txt`. Actualmente usa un ejemplo fijo. |
| `Main.hs` | Gestiona el bucle principal del juego (`gameLoop`) y las interacciones con el usuario (entrada/salida). |

### Compilar y ejecutar el proyecto
En la raíz del proyecto (donde está el archivo `.cabal`):

```bash
stack build
stack run
