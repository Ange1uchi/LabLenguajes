# Proyecto 3: Piedra, Papel, Tijera, Lagarto, Spock (Laboratorio de Lenguajes de Programación CI-3661)

Este proyecto implementa en **Ruby**  el juego extendido de Piedra, Papel, Tijera, Lagarto, Spock (Creado en la serie The Big Bang Theory para reducir) modelando su lógica mediante una jerarquía de clases para las jugadas Además, incluye una interfaz gráfica simple construida con la gema Shoes.

CI-3661 Laboratorio de Lenguajes de ProgramaciónUniversidad Simón Bolívar

# Integrantes

    Angel Valero 18-10436
    Gabriel Seijas 19-00036


# Explicación de la Implementación

La implementación sigue la modularización requerida, utilizando las clases Jugada, Estrategia y Partida para encapsular la lógica del juego y la interfaz gráfica (main.rb) para su presentación visual.
## 1. Jerarquía de Jugadas (RPTLS.rb)

**Clase Padre Jugada:** Contiene un mapa estático de las reglas del juego (RULES), donde la clave es la jugada y el valor es un arreglo de las jugadas a las que vence. Implementa el método puntos(contrincante) para determinar el resultado de una ronda y retornar la dupla [ptos_propios, ptos_contrario] (ej: [1, 0] si gana, [0, 1] si pierde, [0, 0] si empata). Incluye métodos de utilidad como desde_simbolo y que_gana_a (útil para la estrategia Pensar).
**Subclases:** Se implementaron las cinco subclases requeridas: Piedra, Papel, Tijera, Lagarto y Spock, que heredan de Jugada.

## 2. Jerarquía de Estrategias (RPTLS.rb)

**Clase Padre Estrategia:** Define la firma del método prox(jugada_anterior_oponente). Utiliza una semilla (@@semillaPadre) para generar un objeto Random interno, permitiendo un comportamiento determinista si se desea.
    Subclases Implementadas:
    
`Manual:` En la GUI (Shoes), utiliza una estrategia Uniforme por defecto, ya que la gema no permite una fácil interacción síncrona de input por consola sin colgar la aplicación. En modo consola, sí solicita la entrada por STDIN.
    
`Uniforme:` Recibe una lista de movimientos (como String separado por comas desde la GUI) y elige una jugada de esa lista con igual probabilidad.

`Sesgada:` Recibe un mapa (como String jugada:peso,...) y selecciona la jugada aleatoriamente respetando la probabilidad definida por los pesos.

`Copiar:` Juega aleatorio en la primera ronda y luego repite la última jugada del oponente.

`Pensar:` Mantiene un historial de jugadas del oponente. Elige la jugada que vence a la opción más frecuente del contrincante (la "más probable").

## 3. Clase Partida y la Interfaz Gráfica (RPTLS.rb y main.rb)

**Clase Partida:** Gestiona el flujo del juego entre dos estrategias.

    El constructor permite configurar nombres, estrategias, el modo de juego (:rondas o :alcanzar) y el valor objetivo (N).
    El método siguiente_ronda ejecuta una iteración, llama a prox en ambas estrategias, calcula el puntaje y actualiza el estado de la partida, devolviendo un Hash con toda la información necesaria para la GUI.

**Interfaz Gráfica**  (main.rb): Se utiliza Shoes.app para la presentación.

    La sección de Configuración permite la selección dinámica de nombres, tipo de estrategia y parámetros (usando edit_line y list_box).

    La sección de Desarrollo de la Partida muestra el puntaje, el estado actual y un contenedor donde se cargan dinámicamente las imágenes de las jugadas para cada ronda, gestionado por el botón "Siguiente ronda". Nota: Requiere la carpeta img/ con las imágenes nombradas como Piedra.png, Papel.png, etc..

# Instrucciones para la Ejecución del programa

El proyecto asume que se está utilizando JRuby con la gema Shoes o siguiendo las instrucciones oficiales de instalación de Shoes para su entorno.

    Instalar dependencias: Asegúrese de tener la gema Shoes instalada en su entorno Ruby (Shoes4).

**Preparar archivos:**

    El archivo principal es main.rb.

    La lógica del juego está en RPTLS.rb.

    Debe existir una carpeta img/ en el mismo directorio con las imágenes requeridas (ej: Piedra.png, Papel.png, Tijera.png, Lagarto.png, Spock.png).

`Ejecutar:` Ejecute el archivo principal usando Ruby: ruby main.rb

`Configurar y Jugar:`

    En la ventana de Shoes, configure los nombres, las estrategias deseadas y el modo de juego/objetivo (N).
    Presione "Iniciar partida".
    Presione "Siguiente ronda" para avanzar en el juego.

**Complicaciones durante el desarrollo del proyecto:**

    Al implementar la lógica del juego y la interfaz gráfica, surgieron desafíos específicos relacionados con la interacción entre Ruby y la librería Shoes. El principal punto de conflicto se encontró en la estrategia Manual, ya que el uso de STDIN.gets (para solicitar la jugada por teclado) bloquearía el hilo principal de la Interfaz Gráfica de Usuario (GUI), congelando la aplicación. Para mitigar esto, se implementó una estrategia de contingencia: si la aplicación detecta que está corriendo dentro de Shoes, la estrategia Manual se comporta temporalmente como una estrategia Uniforme, eligiendo una jugada aleatoria para evitar el bloqueo. Otro reto fue el manejo de los parámetros de estrategias como Uniforme y Sesgada. Dado que la GUI solo proporciona un string simple desde un campo de texto, fue necesario desarrollar lógica de parsing robusta dentro de los constructores de estas clases para convertir esa cadena de texto ("piedra,papel,tijera" o "piedra:2,papel:1") en listas de símbolos o Hashes de pesos, respectivamente, asegurando la normalización de los nombres de las jugadas y el manejo de entradas inválidas o vacías. Finalmente, la clase Partida tuvo que ser diseñada con un estado interno (stateful), almacenando las últimas jugadas de ambos jugadores para alimentar correctamente las estrategias con memoria (como Copiar y Pensar) en cada llamada a siguiente_ronda, lo cual es crucial para gestionar el flujo del juego, el puntaje, y la detección del fin de la partida en sus dos modos (:rondas o :alcanzar).