# main.rb - Proyecto 3: Piedra, Papel, Tijera, Lagarto, Spock con GUI usando Shoes 4
# Comentarios escritos como estudiante de Ingeniería en Computación

require 'shoes'
require_relative 'RPTLS'

Shoes.app(title: "Piedra, Papel, Tijera, Lagarto, Spock", width: 700, height: 600) do
  background lightgrey

  @partida      = nil
  @estrategias_disponibles = ["Manual", "Uniforme", "Sesgada", "Copiar", "Pensar"]

  @nombre_j1    = nil
  @nombre_j2    = nil
  @estrategia_j1 = nil
  @estrategia_j2 = nil
  @params_j1    = nil
  @params_j2    = nil
  @modo_list    = nil
  @input_n      = nil

  @lbl_estado    = nil
  @lbl_puntaje   = nil
  @stack_jugadas = nil
  @msg_final     = nil

  # Encabezado: título y descripción del proyecto
  stack margin: 10 do
    caption "Piedra, Papel, Tijera, Lagarto, Spock"
    para "Proyecto 3 – Ruby – Interfaz gráfica con Shoes"
  end

  # Configuración de jugadores: inputs para nombres, estrategias y parámetros
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
        para "Params J1 (ej: piedra,papel,tijera):"
        @params_j1 = edit_line width: 0.9
      end

      stack width: 0.5 do
        para "Params J2 (ej: piedra,papel,tijera):"
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
          # Leer nombres de los edit_lines de forma segura
          nombre1 = if @nombre_j1 && @nombre_j1.respond_to?(:text)
                      @nombre_j1.text.to_s.strip
                    else
                      ""
                    end
          nombre2 = if @nombre_j2 && @nombre_j2.respond_to?(:text)
                      @nombre_j2.text.to_s.strip
                    else
                      ""
                    end

          nombre1 = "Jugador 1" if nombre1.empty?
          nombre2 = "Jugador 2" if nombre2.empty?

          # Leer estrategias seleccionadas
          estrategia_nombre_1 = if @estrategia_j1 && @estrategia_j1.respond_to?(:text)
                                  @estrategia_j1.text.to_s
                                else
                                  "Manual"
                                end

          estrategia_nombre_2 = if @estrategia_j2 && @estrategia_j2.respond_to?(:text)
                                  @estrategia_j2.text.to_s
                                else
                                  "Uniforme"
                                end

          # Leer parámetros opcionales
          params1 = if @params_j1 && @params_j1.respond_to?(:text)
                      @params_j1.text.to_s.strip
                    else
                      ""
                    end

          params2 = if @params_j2 && @params_j2.respond_to?(:text)
                      @params_j2.text.to_s.strip
                    else
                      ""
                    end

          # Determinar modo de juego elegido
          modo_texto = if @modo_list && @modo_list.respond_to?(:text)
                         @modo_list.text.to_s
                       else
                         "Alcanzar N puntos"
                       end

          modo = (modo_texto == "Rondas (N fijas)") ? :rondas : :alcanzar

          # Parsear N con fallback a 5 (validar entero y positivo)
          n_str = if @input_n && @input_n.respond_to?(:text)
                    @input_n.text.to_s
                  else
                    "5"
                  end

          begin
            n_int = Integer(n_str)
            n_int = 1 if n_int <= 0
          rescue ArgumentError
            n_int = 5
            if @input_n && @input_n.respond_to?(:text=)
              @input_n.text = "5"
            end
          end

          # Construir objetos de estrategia según selección
          estrategia1 = construir_estrategia(estrategia_nombre_1, params1)
          estrategia2 = construir_estrategia(estrategia_nombre_2, params2)

          # Crear la partida con los datos leídos
          @partida = Partida.new(nombre1, estrategia1, nombre2, estrategia2,
                                 modo, n_int)

          actualizar_estado("Partida iniciada: #{nombre1} vs #{nombre2} (#{modo}, N=#{n_int})")
          actualizar_puntaje("Puntaje: #{nombre1} 0 - 0 #{nombre2}")
          actualizar_msg_final("")

          # Limpiar zona de jugadas y mostrar mensaje inicial
          @stack_jugadas.clear do
            background white
            border black
            para "Haz clic en 'Siguiente ronda' para jugar."
          end
        rescue => e
          # Si algo falla, imprimo en consola y actualizo estado en la GUI
          puts "ERROR al iniciar partida: #{e.class} - #{e.message}"
          puts e.backtrace
          actualizar_estado("Error al iniciar (revisa consola).")
        end
      end

      @lbl_estado = para "Esperando configuración...", margin_left: 20
    end
  end

  # Sección de juego: mostrar puntajes, jugadas y controles
  stack margin: 10 do
    subtitle "Desarrollo de la partida"

    @lbl_puntaje = para "Puntaje: -", margin_bottom: 10

    @stack_jugadas = stack do
      background white
      border black
      para "Aquí se mostrarán las jugadas de cada ronda."
    end

    flow margin_top: 10 do
      button "Siguiente ronda" do
        if @partida.nil?
          actualizar_estado("Primero debes iniciar una partida.")
          next
        end

        # Ejecutar la siguiente ronda y obtener resultado
        resultado = @partida.siguiente_ronda

        jug1  = resultado[:j1]
        jug2  = resultado[:j2]
        p1    = resultado[:p1]
        p2    = resultado[:p2]
        d1    = resultado[:delta1]
        d2    = resultado[:delta2]
        ronda = resultado[:ronda]

        # Mostrar imágenes y textos de la ronda actual
        @stack_jugadas.clear do
          background white
          border black
          caption "Ronda #{ronda}"
          flow do
            stack width: 0.5, align: "center" do
              para "#{@partida.nombre1} juega:"
              # Nota: las imágenes están en la carpeta relativa 'img/'
              image "img/#{jug1}.png", width: 100, height: 100
              para jug1
            end
            stack width: 0.5, align: "center" do
              para "#{@partida.nombre2} juega:"
              # Nota: las imágenes están en la carpeta relativa 'img/'
              image "img/#{jug2}.png", width: 100, height: 100
              para jug2
            end
          end
          para "Puntos de la ronda: #{@partida.nombre1} +#{d1}, #{@partida.nombre2} +#{d2}"
        end

        actualizar_puntaje(
          "Puntaje: #{@partida.nombre1} #{p1} - #{p2} #{@partida.nombre2}"
        )

        # Si la partida terminó, deshabilitar siguiente y mostrar ganador/empate
        if resultado[:terminado]
          @btn_siguiente.style(state: "disabled") rescue nil
          ganador = resultado[:ganador]
          if ganador
            actualizar_msg_final("¡Ha terminado la partida! Ganador: #{ganador}")
          else
            actualizar_msg_final("¡Ha terminado la partida! Empate.")
          end
          actualizar_estado("Partida finalizada.")
        else
          actualizar_estado("Ronda #{ronda} jugada. Continúa con la siguiente.")
        end
      end

      button "Reiniciar" do
        # Reset básico de la interfaz
        @partida = nil
        actualizar_estado("Esperando configuración...")
        actualizar_puntaje("Puntaje: -")
        actualizar_msg_final("")

        @stack_jugadas.clear do
          background white
          border black
          para "Aquí se mostrarán las jugadas de cada ronda."
        end
      end
    end

    @msg_final = para ""
  end

  # Métodos auxiliares para actualizar la GUI y construir estrategias
  def actualizar_estado(texto)
    if @lbl_estado && @lbl_estado.respond_to?(:text=)
      @lbl_estado.text = texto
    end
  end

  def actualizar_puntaje(texto)
    if @lbl_puntaje && @lbl_puntaje.respond_to?(:text=)
      @lbl_puntaje.text = texto
    end
  end

  def actualizar_msg_final(texto)
    if @msg_final && @msg_final.respond_to?(:text=)
      @msg_final.text = texto
    end
  end

  # Mapear nombre de estrategia a su clase correspondiente (se asume RPTLS.rb define estas clases)
  def construir_estrategia(nombre_estrategia, params_str)
    case nombre_estrategia
    when "Manual"
      Manual.new
    when "Uniforme"
      # Paso el string tal cual; la clase Uniforme se encarga de parsearlo
      Uniforme.new(params_str.to_s)
    when "Sesgada"
      Sesgada.new(params_str.to_s)
    when "Copiar"
      Copiar.new
    when "Pensar"
      Pensar.new
    else
      Uniforme.new("") # fallback: estrategia uniforme por defecto
    end
  end

end
# Fin de main.rb