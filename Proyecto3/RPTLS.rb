# RPTLS.rb
# Lógica del juego Piedra, Papel, Tijera, Lagarto, Spock
# Proyecto 3 - Laboratorio de Lenguajes de Programación I.

# ============================================================
# 1. Jerarquía de Jugadas
# ============================================================

class Jugada
  attr_reader :tipo

  # Mapa de qué le gana a qué:
  # CLAVE = jugada, VALOR = arreglo de jugadas a las que vence
  RULES = {
    Piedra:  [:Tijera, :Lagarto],
    Papel:   [:Piedra, :Spock],
    Tijera:  [:Papel, :Lagarto],
    Lagarto: [:Papel, :Spock],
    Spock:   [:Piedra, :Tijera]
  }

  def initialize(tipo)
    @tipo = tipo.to_sym
  end

  def to_s
    @tipo.to_s
  end

  # puntos(contrincante) -> [ptos_propios, ptos_contrincante]
  def puntos(contrincante)
    tipo_contra = contrincante.tipo

    if tipo_contra == @tipo
      return [0, 0]  # Empate
    end

    if RULES[@tipo].include?(tipo_contra)
      [1, 0]
    else
      [0, 1]
    end
  end

  # Fábrica de jugadas desde un símbolo o string
  def self.desde_simbolo(sim)
    s = normalizar_simbolo(sim)
    case s
    when :Piedra  then Piedra.new
    when :Papel   then Papel.new
    when :Tijera  then Tijera.new
    when :Lagarto then Lagarto.new
    when :Spock   then Spock.new
    else
      raise ArgumentError, "Jugada desconocida: #{sim}"
    end
  end

  # Normaliza cosas como "piedra", :piedra, "PIEDRA" -> :Piedra
  def self.normalizar_simbolo(sim)
    return sim if sim.is_a?(Symbol) && RULES.key?(sim)

    str = sim.to_s.strip.downcase
    case str
    when "piedra"  then :Piedra
    when "papel"   then :Papel
    when "tijera"  then :Tijera
    when "lagarto" then :Lagarto
    when "spock"   then :Spock
    else
      raise ArgumentError, "Nombre de jugada inválido: #{sim}"
    end
  end

  # Dada la jugada probable del oponente, devuelve una jugada que la derrote.
  def self.que_gana_a(sim, rng = Random.new)
    objetivo = normalizar_simbolo(sim)
    vencedores = RULES.select { |_k, v| v.include?(objetivo) }.keys
    raise "No hay vencedores para #{objetivo}" if vencedores.empty?

    desde_simbolo(vencedores.sample(random: rng))
  end
end

# Subclases de Jugada (requeridas)
class Piedra < Jugada
  def initialize
    super(:Piedra)
  end
end

class Papel < Jugada
  def initialize
    super(:Papel)
  end
end

class Tijera < Jugada
  def initialize
    super(:Tijera)
  end
end

class Lagarto < Jugada
  def initialize
    super(:Lagarto)
  end
end

class Spock < Jugada
  def initialize
    super(:Spock)
  end
end

# ============================================================
# 2. Jerarquía de Estrategias
# ============================================================

class Estrategia
  @@semillaPadre = 42

  def initialize
    @rng = Random.new(@@semillaPadre)
    @@semillaPadre += 1
  end

  def prox(j = nil)
    raise NotImplementedError, "Debe implementarse en las subclases"
  end
end

# ------------------------------------------------------------
# 2.1 Estrategia Manual (CORREGIDO PARA GUI)
# ------------------------------------------------------------
class Manual < Estrategia
  # Retorna el símbolo especial :EsperandoJugada si está en modo GUI
  def prox(_jugada_anterior_oponente = nil)
    if defined?(Shoes)
      return :EsperandoJugada
    end

    # ---- Modo consola real ----
    loop do
      puts "Elige jugada (piedra, papel, tijera, lagarto, spock): "
      entrada = STDIN.gets&.chomp
      begin
        return Jugada.desde_simbolo(entrada)
      rescue ArgumentError
        puts "Entrada inválida, intenta de nuevo."
      end
    end
  end
end

# ------------------------------------------------------------
# 2.2 Estrategia Uniforme
# ------------------------------------------------------------
class Uniforme < Estrategia
  def initialize(lista_movimientos)
    super()

    if lista_movimientos.is_a?(String)
      lista_movimientos = lista_movimientos.split(",")
    end

    @movimientos = lista_movimientos.map do |m|
      begin
        Jugada.normalizar_simbolo(m)
      rescue ArgumentError
        nil # Ignorar jugadas inválidas
      end
    end.compact.uniq

    if @movimientos.empty?
      @movimientos = [:Piedra, :Papel, :Tijera, :Lagarto, :Spock]
    end
  end

  def prox(_jugada_anterior_oponente = nil)
    sim = @movimientos.sample(random: @rng)
    Jugada.desde_simbolo(sim)
  end
end

# ------------------------------------------------------------
# 2.3 Estrategia Sesgada
# ------------------------------------------------------------
class Sesgada < Estrategia
  def initialize(pesos)
    super()
    @pesos = {}

    if pesos.is_a?(Hash)
      pesos.each do |k, v|
        sim = Jugada.normalizar_simbolo(k)
        @pesos[sim] = v.to_f
      end
    elsif pesos.is_a?(String)
      # Formato esperado: "piedra:2,papel:1,spock:3"
      pesos.split(",").each do |par|
        nombre, peso_str = par.split(":")
        next if nombre.nil? || peso_str.nil?

        begin
          sim = Jugada.normalizar_simbolo(nombre)
          @pesos[sim] = peso_str.to_f
        rescue ArgumentError
          next
        end
      end
    end

    if @pesos.empty? || @pesos.values.all? { |v| v <= 0 }
      @pesos = {
        Piedra: 1.0, Papel: 1.0, Tijera: 1.0, Lagarto: 1.0, Spock: 1.0
      }
    end
  end

  def prox(_jugada_anterior_oponente = nil)
    total = @pesos.values.sum
    umbral = @rng.rand * total
    acumulado = 0.0

    @pesos.each do |sim, w|
      acumulado += w
      if umbral <= acumulado
        return Jugada.desde_simbolo(sim)
      end
    end

    # Fallback si hay error numérico o pesos son iguales
    Jugada.desde_simbolo(@pesos.keys.sample(random: @rng))
  end
end

# ------------------------------------------------------------
# 2.4 Estrategia Copiar (CORREGIDO: Uso de Uniforme.prox con @rng)
# ------------------------------------------------------------
class Copiar < Estrategia
  def initialize
    super()
    @primera = true
  end

  def prox(jugada_anterior_oponente = nil)
    # Primera jugada → aleatorio
    if @primera || jugada_anterior_oponente.nil?
      @primera = false
      # Crear una instancia de Uniforme para la primera jugada aleatoria
      # Nota: Usamos una instancia temporal ya que Uniforme necesita su propio RNG para la muestra.
      return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox
    end

    # Rondas siguientes → copiar jugada del rival
    return Jugada.desde_simbolo(jugada_anterior_oponente.tipo)
  end
end


# ------------------------------------------------------------
# 2.5 Estrategia Pensar
# ------------------------------------------------------------
class Pensar < Estrategia
  def initialize
    super()
    @historial = Hash.new(0) # { :Piedra => frecuencia, ... }
  end

  def prox(jugada_anterior_oponente = nil)
    # Actualizamos historial con la jugada previa del oponente
    if jugada_anterior_oponente
      sim = jugada_anterior_oponente.tipo
      @historial[sim] += 1
    end

    # Si aún no tenemos datos, jugamos uniforme
    if @historial.empty?
      return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox
    end

    # Buscamos la jugada más frecuente del oponente
    mas_probable, _freq = @historial.max_by { |_k, v| v }

    # Elegimos una jugada que le gane a la más probable
    Jugada.que_gana_a(mas_probable, @rng)
  end
end

# ============================================================
# 3. Clase Partida (CORREGIDO PARA MANEJAR PAUSA)
# ============================================================

class Partida
  attr_reader :nombre1, :nombre2, :modo, :objetivo, :ronda_actual,
              :puntos1, :puntos2, :estrategia1, :estrategia2 # Agregado para depuración

  def initialize(*args)
    if args.size == 1 && args[0].is_a?(Hash)
      config = args[0]
      @nombre1    = "Jugador1"
      @nombre2    = "Jugador2"
      @estrategia1 = config[:Jugador1]
      @estrategia2 = config[:Jugador2]
      @modo       = :rondas
      @objetivo   = 5
    elsif args.size == 6
      @nombre1, @estrategia1,
      @nombre2, @estrategia2,
      @modo, @objetivo = args
    else
      raise ArgumentError, "Parámetros inválidos para Partida.new"
    end

    @modo      = @modo.to_sym
    @objetivo  = @objetivo.to_i
    @objetivo  = 1 if @objetivo <= 0

    @puntos1   = 0
    @puntos2   = 0
    @ronda_actual = 0
    @terminado    = false

    @ultima_jugada_j1 = nil
    @ultima_jugada_j2 = nil
  end

  def terminado?
    @terminado
  end

  # Juega UNA ronda o pausa si un jugador es manual.
  # jugada_manual es la instancia de Jugada del jugador que reanuda la partida.
  def siguiente_ronda(jugada_manual = nil)
    if @terminado
      return {
        j1: @ultima_jugada_j1&.to_s, j2: @ultima_jugada_j2&.to_s,
        p1: @puntos1, p2: @puntos2,
        delta1: 0, delta2: 0, ronda: @ronda_actual,
        terminado: true, ganador: ganador_final
      }
    end

    # 1. Obtener jugadas: Juega o Pausa
    if jugada_manual
      # Caso de reanudación: J1 jugó manualmente.
      # NOTA: Simplificamos asumiendo que Partida solo se reanuda después de que J1 pausó.
      # La lógica de J2 pausando y reanudando es más compleja y excede la simplicidad del flujo GUI.
      jug1 = jugada_manual
      jug2 = @estrategia2.prox(@ultima_jugada_j1)
    else
      # Llamar prox() para J1
      jug1 = @estrategia1.prox(@ultima_jugada_j2)
      if jug1 == :EsperandoJugada
        return { pausada: @nombre1 }
      end

      # Llamar prox() para J2
      jug2 = @estrategia2.prox(@ultima_jugada_j1)
      if jug2 == :EsperandoJugada
        return { pausada: @nombre2 }
      end
    end

    # Si llegamos aquí, ambas jugadas (jug1 y jug2) son instancias de Jugada.
    @ronda_actual += 1

    @ultima_jugada_j1 = jug1
    @ultima_jugada_j2 = jug2

    delta1, delta2 = jug1.puntos(jug2)
    @puntos1 += delta1
    @puntos2 += delta2

    # ---------- Lógica de terminación de partida ----------
    case @modo
    when :rondas
      @terminado = true if @ronda_actual >= @objetivo
    when :alcanzar
      @terminado = true if @puntos1 >= @objetivo || @puntos2 >= @objetivo
    end
    # ------------------------------------------------------

    {
      j1: jug1.to_s, j2: jug2.to_s,
      p1: @puntos1, p2: @puntos2,
      delta1: delta1, delta2: delta2,
      ronda: @ronda_actual,
      terminado: @terminado,
      ganador: (@terminado ? ganador_final : nil)
    }
  end

  private

  def ganador_final
    if @puntos1 > @puntos2
      @nombre1
    elsif @puntos2 > @puntos1
      @nombre2
    else
      nil
    end
  end
end