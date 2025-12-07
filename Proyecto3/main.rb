# main.rb
# Proyecto 3 - Laboratorio de Lenguajes de Programación I
# Piedra, Papel, Tijera, Lagarto, Spock con interfaz gráfica usando Shoes 4

require 'shoes'
require_relative 'RPTLS'

# Lista de jugadas disponibles (para los botones manuales)
$JUGADAS_POSIBLES = [:Piedra, :Papel, :Tijera, :Lagarto, :Spock]

Shoes.app(title: "Piedra, Papel, Tijera, Lagarto, Spock", width: 700, height: 600) do
  background lightgrey

  @partida      = nil
  @estrategias_disponibles = ["Manual", "Uniforme", "Sesgada", "Copiar", "Pensar"]

  @nombre_j1    = nil; @nombre_j2    = nil
  @estrategia_j1 = nil; @estrategia_j2 = nil
  @params_j1    = nil; @params_j2    = nil
  @modo_list    = nil; @input_n      = nil

  @lbl_estado    = nil
  @lbl_puntaje   = nil
  @stack_jugadas = nil
  @msg_final     = nil
  
  # Nuevas referencias para manejar la pausa manual
  @btn_siguiente = nil
  @stack_botones_manual = nil
  @jugador_pausa = nil # Almacena qué jugador está en pausa

  # ---------------- Encabezado ----------------
  stack margin: 10 do
    caption "Piedra, Papel, Tijera, Lagarto, Spock"
    para "Proyecto 3 – Ruby – Interfaz gráfica con Shoes"
  end

  # ---------------- Configuración ----------------
  stack margin: 10 do
    subtitle "Configuración de jugadores"

    flow do
      stack width: 0.5 do
        para "Nombre Jugador 1:"
        @nombre_j1 = edit_line width: 0.9, text: "Jugador 1"

        para "Estrategia Jugador 1:"
        @estrategia_j1 = list_box items: @estrategias_disponibles,
                                  choose: "Manual",
                                  width: 0.9
      end

      stack width: 0.5 do
        para "Nombre Jugador 2:"
        @nombre_j2 = edit_line width: 0.9, text: "Jugador 2"

        para "Estrategia Jugador 2:"
        @estrategia_j2 = list_box items: @estrategias_disponibles,
                                  choose: "Uniforme",
                                  width: 0.9
      end
    end

    para "Parámetros de estrategias (opcional, se interpretan en RPTLS.rb):"
    flow do
      stack width: 0.5 do
        para "Params J1 (ej: piedra,papel,tijera o piedra:2,papel:1):"
        @params_j1 = edit_line width: 0.9
      end

      stack width: 0.5 do
        para "Params J2 (ej: piedra,papel,tijera o piedra:2,papel:1):"
        @params_j2 = edit_line width: 0.9
      end
    end

    subtitle "Modo de juego"

    flow do
      stack width: 0.5 do
        para "Selecciona modo:"
        @modo_list = list_box items: [
                                  "Rondas (N fijas)",
                                  "Alcanzar N puntos"
                                ],
                                choose: "Alcanzar N puntos",
                                width: 0.9
      end

      stack width: 0.5 do
        para "Valor de N:"
        @input_n = edit_line width: 0.5, text: "5"
      end
    end

    flow margin_top: 10 do
      button "Iniciar partida" do
        begin
          # Lógica de lectura de configuración
          nombre1 = @nombre_j1.text.to_s.strip
          nombre2 = @nombre_j2.text.to_s.strip
          nombre1 = "Jugador 1" if nombre1.empty?
          nombre2 = "Jugador 2" if nombre2.empty?

          estrategia_nombre_1 = @estrategia_j1.text.to_s
          estrategia_nombre_2 = @estrategia_j2.text.to_s
          params1 = @params_j1.text.to_s.strip
          params2 = @params_j2.text.to_s.strip

          modo_texto = @modo_list.text.to_s
          modo = (modo_texto == "Rondas (N fijas)") ? :rondas : :alcanzar

          n_int = Integer(@input_n.text.to_s) rescue 5
          n_int = 1 if n_int <= 0

          estrategia1 = construir_estrategia(estrategia_nombre_1, params1)
          estrategia2 = construir_estrategia(estrategia_nombre_2, params2)

          @partida = Partida.new(nombre1, estrategia1, nombre2, estrategia2,
                                 modo, n_int)

          actualizar_estado("Partida iniciada: #{nombre1} vs #{nombre2} (#{modo}, N=#{n_int})")
          actualizar_puntaje("Puntaje: #{nombre1} 0 - 0 #{nombre2}")
          actualizar_msg_final("")

          @stack_jugadas.clear do
            background white
            border black
            para "Haz clic en 'Siguiente ronda' para jugar."
          end
          
          # Asegurar que el botón de siguiente ronda esté visible
          @btn_siguiente.show rescue nil
          @stack_botones_manual.clear rescue nil

        rescue => e
          # Si falla la inicialización (ej: parámetro Sesgada inválido)
          puts "ERROR al iniciar partida: #{e.class} - #{e.message}"
          actualizar_estado("Error al iniciar (revisa consola).")
        end
      end

      @lbl_estado = para "Esperando configuración...", margin_left: 20
    end
  end

  # ---------------- Sección de juego ----------------
  stack margin: 10 do
    subtitle "Desarrollo de la partida"

    @lbl_puntaje = para "Puntaje: -", margin_bottom: 10

    @stack_jugadas = stack do
      background white
      border black
      para "Aquí se mostrarán las jugadas de cada ronda."
    end

    flow margin_top: 10 do
      # Referencia al botón Siguiente Ronda para poder ocultarlo/mostrarlo
      @btn_siguiente = button "Siguiente ronda" do
        if @partida.nil?
          actualizar_estado("Primero debes iniciar una partida.")
          next
        end

        resultado = @partida.siguiente_ronda

        # === LÓGICA DE PAUSA (ESTRATEGIA MANUAL) ===
        if resultado[:pausada]
          @jugador_pausa = resultado[:pausada]
          
          @btn_siguiente.hide # Esconder el botón de Siguiente Ronda
          
          actualizar_estado("Esperando la jugada de #{@jugador_pausa}...")
          
          mostrar_botones_manual(@jugador_pausa) # Mostrar botones de jugada
          next
        end
        # ==========================================

        # --- Lógica de ronda completada ---
        procesar_resultado_ronda(resultado)

      end # Fin del botón "Siguiente ronda"

      button "Reiniciar" do
        @partida = nil
        actualizar_estado("Esperando configuración...")
        actualizar_puntaje("Puntaje: -")
        actualizar_msg_final("")

        @stack_jugadas.clear do
          background white
          border black
          para "Aquí se mostrarán las jugadas de cada ronda."
        end
        @btn_siguiente.show rescue nil
        @stack_botones_manual.clear rescue nil
      end
      
      # Stack para los botones de Jugada Manual (inicialmente vacío)
      @stack_botones_manual = stack do
        # Esto se llenará dinámicamente
      end
    end

    @msg_final = para ""
  end

  # ---------------- Métodos auxiliares ----------------
  def actualizar_estado(texto)
    @lbl_estado.text = texto if @lbl_estado
  end

  def actualizar_puntaje(texto)
    @lbl_puntaje.text = texto if @lbl_puntaje
  end

  def actualizar_msg_final(texto)
    @msg_final.text = texto if @msg_final
  end

  def construir_estrategia(nombre_estrategia, params_str)
    case nombre_estrategia
    when "Manual"
      Manual.new
    when "Uniforme"
      Uniforme.new(params_str.to_s)
    when "Sesgada"
      Sesgada.new(params_str.to_s)
    when "Copiar"
      Copiar.new
    when "Pensar"
      Pensar.new
    else
      Uniforme.new("") # por defecto
    end
  end
  
  # Muestra los botones de jugada manual en el stack
  def mostrar_botones_manual(jugador_pausa)
    @stack_botones_manual.clear do
      subtitle "Turno de #{jugador_pausa}"
      para "Elige tu jugada:"
      flow do
        $JUGADAS_POSIBLES.each do |jugada_simbolo|
          button jugada_simbolo.to_s do
            jugada_seleccionada = Jugada.desde_simbolo(jugada_simbolo)
            
            # Reanudar la partida con la jugada elegida
            reanudar_partida_manual(jugada_seleccionada)
          end
        end
      end
    end
  end

  # Función para reanudar la partida después de una selección manual
  def reanudar_partida_manual(jugada_seleccionada)
    # Limpiar los botones manuales
    @stack_botones_manual.clear
    
    # Reanudar la partida. Partida.siguiente_ronda usa esta jugada
    resultado = @partida.siguiente_ronda(jugada_seleccionada) 

    # Si se pausa de nuevo (ej: J2 era manual), volvemos a mostrar botones
    if resultado[:pausada]
      @jugador_pausa = resultado[:pausada]
      actualizar_estado("Esperando la jugada de #{@jugador_pausa}...")
      mostrar_botones_manual(@jugador_pausa)
    else
      # La ronda se completó. Mostrar el botón Siguiente Ronda y el resultado.
      @btn_siguiente.show
      procesar_resultado_ronda(resultado)
    end
  end
  
  # Centraliza la lógica de actualización del resultado de la ronda
  def procesar_resultado_ronda(resultado)
    jug1  = resultado[:j1]
    jug2  = resultado[:j2]
    p1    = resultado[:p1]
    p2    = resultado[:p2]
    d1    = resultado[:delta1]
    d2    = resultado[:delta2]
    ronda = resultado[:ronda]

    @stack_jugadas.clear do
      background white
      border black
      caption "Ronda #{ronda}"

      # Mostrar jugada del Jugador 1
      flow width: 1.0, margin_bottom: 10 do
        stack width: 0.5 do
          para strong("#{@partida.nombre1} juega: #{jug1}")
          # La imagen debe tener el mismo nombre que la jugada + .png
          # Usamos jug1 (que es el string 'Piedra', 'Papel', etc.)
          image "#{jug1}.png", height: 80, margin: 10 
        end
        # Separador visual
        para strong("VS"), margin_top: 20
        # Mostrar jugada del Jugador 2
        stack width: 0.5 do
          para strong("#{@partida.nombre2} juega: #{jug2}")
          image "#{jug2}.png", height: 80, margin: 10
        end
      end
      
      para "Puntos de la ronda: #{@partida.nombre1} +#{d1}, #{@partida.nombre2} +#{d2}"
    end
    
    # AQUÍ ES DONDE FALTABA EL CIERRE DEL MÉTODO
    actualizar_puntaje(
      "Puntaje: #{@partida.nombre1} #{p1} - #{p2} #{@partida.nombre2}"
    )

    if resultado[:terminado]
      @btn_siguiente.hide
      ganador = resultado[:ganador]
      if ganador
        actualizar_msg_final("¡Ha terminado la partida! Ganador: #{ganador}")
      else
        actualizar_msg_final("¡Ha terminado la partida! Empate.")
      end
      actualizar_estado("Partida finalizada.")
    else
      @btn_siguiente.show 
      actualizar_estado("Ronda #{ronda} jugada. Continúa con la siguiente.")
    end
  end
  
end 