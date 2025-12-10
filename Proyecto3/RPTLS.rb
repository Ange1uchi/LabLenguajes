# RPTLS.rb
# Implementación de Piedra, Papel, Tijera, Lagarto, Spock
# Comentarios escritos en estilo de estudiante de ingeniería en computación:
# explicaciones breves y claras sobre propósito y funcionamiento.

# ============================================================
# 1. Jerarquía de Jugadas
# ============================================================

class Jugada
  attr_reader :tipo

  # Mapa de qué vence a qué. Las claves son símbolos que representan
  # cada jugada y los valores son arreglos con las jugadas vencidas.
  RULES = {
    Piedra:  [:Tijera, :Lagarto],
    Papel:   [:Piedra, :Spock],
    Tijera:  [:Papel, :Lagarto],
    Lagarto: [:Papel, :Spock],
    Spock:   [:Piedra, :Tijera]
  }

  # Inicializa una jugada a partir de un tipo (string o símbolo).
  def initialize(tipo)
    @tipo = tipo.to_sym
  end

  def to_s
    @tipo.to_s
  end

  # Calcula los puntos de esta jugada contra otra jugada.
  # Devuelve un par [puntos_propios, puntos_contrincante].
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

  # Crea una instancia de Jugada a partir de un símbolo o string.
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

  # Convierte entradas como "piedra", :piedra, "PIEDRA" -> :Piedra
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

  # Devuelve el símbolo asociado a una instancia de Jugada.
  def self.simbolo_de(jugada)
    jugada.tipo
  end

  # Dada una jugada objetivo, devuelve una jugada que la derrote.
  # Si hay dos opciones, elige una aleatoriamente usando rng.
  def self.que_gana_a(sim, rng = Random.new)
    objetivo = normalizar_simbolo(sim)
    vencedores = RULES.select { |_k, v| v.include?(objetivo) }.keys
    raise "No hay vencedores para #{objetivo}" if vencedores.empty?

    desde_simbolo(vencedores.sample(random: rng))
  end
end

# Clases concretas que representan cada tipo de jugada.
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
  # Semilla compartida para crear RNG reproducible entre instancias.
  @@semillaPadre = 42

  def initialize
    @rng = Random.new(@@semillaPadre)
    @@semillaPadre += 1
  end

  # Método abstracto: debe retornar una Jugada. Puede recibir la jugada
  # previa del oponente (o nil si no hay).
  def prox(j = nil)
    raise NotImplementedError, "Debe implementarse en las subclases"
  end
end

# Estrategia Manual:
# - En consola pide entrada al usuario.
# - En GUI (Shoes) evita bloquear y elige aleatoriamente.
class Manual < Estrategia
  def prox(_jugada_anterior_oponente = nil)
    if defined?(Shoes)
      puts "[AVISO] Estrategia 'Manual' en GUI se juega aleatoria (no hay input por ventana)."
      return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox
    end

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

# Estrategia Uniforme:
# - Recibe una lista (o string) de movimientos permitidos.
# - Elige uniformemente entre ellos.
class Uniforme < Estrategia
  def initialize(lista_movimientos)
    super()

    # Acepta string tipo "piedra,papel" y lo convierte en array limpio.
    if lista_movimientos.is_a?(String)
      lista_movimientos = lista_movimientos.split(",").map(&:strip).reject(&:empty?)
    end

    # Normaliza entradas a símbolos válidos y elimina duplicados.
    @movimientos = lista_movimientos.filter_map do |m|
      begin
        Jugada.normalizar_simbolo(m)
      rescue ArgumentError
        nil
      end
    end.uniq

    # Si no quedó nada válido, usar todas las jugadas por defecto.
    if @movimientos.empty?
      @movimientos = [:Piedra, :Papel, :Tijera, :Lagarto, :Spock]
    end

    puts "[Uniforme] Movimientos permitidos: #{@movimientos.inspect}"
  end

  def prox(_jugada_anterior_oponente = nil)
    sim = @movimientos.sample(random: @rng)
    Jugada.desde_simbolo(sim)
  end
end

# Estrategia Sesgada:
# - Recibe pesos por jugada (hash o string "piedra:2,papel:1").
# - Selecciona según distribución discreta definida por los pesos.
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
      pesos.split(",").each do |par|
        nombre, peso_str = par.split(":")
        next if nombre.nil? || peso_str.nil?

        sim = Jugada.normalizar_simbolo(nombre)
        @pesos[sim] = peso_str.to_f
      end
    else
      raise ArgumentError, "Formato de pesos no soportado"
    end

    # Si no hay pesos válidos, usar distribución uniforme por defecto.
    if @pesos.empty? || @pesos.values.all? { |v| v <= 0 }
      @pesos = {
        Piedra: 1.0,
        Papel: 1.0,
        Tijera: 1.0,
        Lagarto: 1.0,
        Spock: 1.0
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

    # Fallback si hay error numérico: devolver la última clave.
    Jugada.desde_simbolo(@pesos.keys.last)
  end
end

# Estrategia Copiar:
# - Primera ronda elige aleatorio.
# - Luego copia la última jugada del oponente.
class Copiar < Estrategia
  def initialize
    super()
    @primera = true
  end

  def prox(jugada_anterior_oponente = nil)
    if @primera
      @primera = false
      return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox
    end

    return Jugada.desde_simbolo(jugada_anterior_oponente.tipo)
  end
end

# Estrategia Pensar:
# - Mantiene un historial de la frecuencia de jugadas del oponente.
# - Elige la jugada que vence a la más frecuente.
class Pensar < Estrategia
  def initialize
    super()
    @historial = Hash.new(0)
  end

  def prox(jugada_anterior_oponente = nil)
    # Actualizamos historial con la última jugada del oponente si existe.
    if jugada_anterior_oponente
      sim = jugada_anterior_oponente.tipo
      @historial[sim] += 1
    end

    # Si no hay datos, jugar uniforme.
    if @historial.empty?
      return Uniforme.new([:Piedra, :Papel, :Tijera, :Lagarto, :Spock]).prox
    end

    # Determinar la jugada más frecuente del rival.
    mas_probable, _freq = @historial.max_by { |_k, v| v }

    # Escoger una jugada que le gane a la más probable.
    Jugada.que_gana_a(mas_probable, @rng)
  end
end

# ============================================================
# 3. Clase Partida
# ============================================================

class Partida
  attr_reader :nombre1, :nombre2, :modo, :objetivo, :ronda_actual,
              :puntos1, :puntos2

  # Constructor flexible:
  # - Puede recibir un hash { :Jugador1 => estr1, :Jugador2 => estr2 }
  # - O recibir los 6 parámetros: nombre1, estr1, nombre2, estr2, modo, objetivo
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

  # Indica si la partida terminó.
  def terminado?
    @terminado
  end

  # Juega una ronda y devuelve un hash con información del resultado.
  # Actualiza puntajes y estado interno.
  def siguiente_ronda
    if @terminado
      return {
        j1: @ultima_jugada_j1&.to_s,
        j2: @ultima_jugada_j2&.to_s,
        p1: @puntos1,
        p2: @puntos2,
        delta1: 0,
        delta2: 0,
        ronda: @ronda_actual,
        terminado: true,
        ganador: ganador_final
      }
    end

    @ronda_actual += 1

    jug1 = @estrategia1.prox(@ultima_jugada_j2)
    jug2 = @estrategia2.prox(@ultima_jugada_j1)

    @ultima_jugada_j1 = jug1
    @ultima_jugada_j2 = jug2

    delta1, delta2 = jug1.puntos(jug2)
    @puntos1 += delta1
    @puntos2 += delta2

    # Modo de terminación:
    # - :rondas -> jugar exactamente N rondas.
    # - :alcanzar -> jugar hasta que alguien alcance N puntos.
    case @modo
    when :rondas
      @terminado = true if @ronda_actual >= @objetivo
    when :alcanzar
      @terminado = true if @puntos1 >= @objetivo || @puntos2 >= @objetivo
    else
      @terminado = true if @ronda_actual >= @objetivo
    end

    {
      j1: jug1.to_s,
      j2: jug2.to_s,
      p1: @puntos1,
      p2: @puntos2,
      delta1: delta1,
      delta2: delta2,
      ronda: @ronda_actual,
      terminado: @terminado,
      ganador: (@terminado ? ganador_final : nil)
    }
  end

  private

  # Determina el ganador final por puntaje o nil si empate.
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
