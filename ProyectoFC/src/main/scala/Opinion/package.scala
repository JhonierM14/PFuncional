import Comete._
import common._

import scala.collection.parallel.CollectionConverters._

package object Opinion {
  type SpecificBelief = Vector[Double]
  type GenericBeliefConf = Int => SpecificBelief
  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double


  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {

    def aux(beliefs: SpecificBelief, distribution: DistributionValues) : Double = {
      //I0 = [0, 0.125]

      val k = distribution.length
      val numAgents = beliefs.length

      //d valores de distribuccion posibles
      //valores de distribucciones posibles

      val intervalos: Vector[(Double,Double)] = (0 until k).map(i => i match {
        // Primer intervalo: I₀ = [d₀, d₁/2)
        case 0 => (0.0, distribution(1) / 2)

        // Último intervalo: cuando i sea igual a n
        case n if n == k - 1 => ((distribution(k-2) + 1)/2, 1.0)

        // Intervalos intermedios:
        case i => ((distribution(i) + distribution(i-1))/2, (distribution(i) + distribution(i+1)) / 2)
        }
      ).toVector


      val frequencia: Vector[Double] = for {
              (s, t) <- intervalos
              cantidadIntervalo = for {
                w <- beliefs
                if (w >= s) && (t match {
                  case 1.0 => w <= t // Último intervalo, usar <=
                  case _ => w < t    // Intervalos intermedios, usar <
                })
              } yield w
            } yield cantidadIntervalo.length.toDouble / numAgents

      val rho_Gnt = rhoCMT_Gen(alpha, beta)
      val n = normalizar(rho_Gnt)
      val resultado = n(frequencia, distribution)
      BigDecimal(resultado).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    aux
  }

  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph)=> SpecificBelief

  //genera una matriz de n filas y n columnas
  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (wg, nags) = swg

    // Función auxiliar para construir una fila inmutablemente
    def construirFila(i: Int, nags: Int): IndexedSeq[Double] = {
      (0 until nags).map(j => wg(i, j))
    }

    //Unimos todas las filas
    (0 until nags).map(i => construirFila(i, nags))
  }

  def confBiasUpdate(b: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val (wg, _) = swg
//    val matriz = showWeightedGraph(swg) // matriz

    def actualizarRecursivo(index: Int, beliefsActualizados: Vector[Double]): Vector[Double] = {
      if (index >= b.length) {
        beliefsActualizados // Caso base: hemos procesado todos los agentes
      } else {
        // Generar la fila `index` de la matriz dinámicamente
        val fila = (0 until b.length).map(j => wg(j,index))

        // Calcular `Ai` y la suma de influencias para el agente actual
        val Ai = (0 until b.length).filter(j => fila(j) > 0)
        val influenceSum = Ai.map { j =>
          (1.0 - math.abs(b(j) - b(index))) * fila(j) * (b(j) - b(index))
        }.sum

        val beliefUpdate = if (Ai.isEmpty) b(index) else b(index) + influenceSum / Ai.length

        // Llamada recursiva con el siguiente índice
        actualizarRecursivo(index + 1, beliefsActualizados :+ beliefUpdate)
      }
    }

    actualizarRecursivo(0, Vector.empty[Double]) // Iniciar recursión
  }

  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {
    val beliefs = IndexedSeq()

    def createSequence(unidad_tiempo: Int, b_i: SpecificBelief, beliefs_seq: IndexedSeq[SpecificBelief]): IndexedSeq[SpecificBelief] ={
      if (unidad_tiempo < 0) beliefs_seq else{
        val beliefs_new = beliefs_seq :+ b_i
        val b_new = fu(b_i, swg)
        createSequence(unidad_tiempo-1, b_new,beliefs_new)
      }
    }

    createSequence(t, b0, beliefs)
  }

  //Versiones paralelas
  def rhopar(alpha: Double, beta: Double): AgentsPolMeasure = {

    def aux(creenciasEspecificas: SpecificBelief, distribuccion: DistributionValues): Double = {

      val numAgentes = creenciasEspecificas.length
      val k = distribuccion.length

      // Calculo de intervalos
      val intervalos: Vector[(Double, Double)] = (0 until k).map { i =>
        i match {
          case 0 => (0.0, distribuccion(1) / 2)
          case n if n == k - 1 => ((distribuccion(k - 2) + 1) / 2, 1.0)
          case i => ((distribuccion(i) + distribuccion(i - 1)) / 2, (distribuccion(i) + distribuccion(i + 1)) / 2)
        }
      }.toVector

      val frequencia = calcularFrecuencia(intervalos, creenciasEspecificas, numAgentes)

      val distribucion: Distribution = (frequencia, distribuccion)

      val medida_norm = normalizar(rhoCMT_Gen(alpha, beta))
      val medidaPolarizacion = medida_norm(distribucion)
      val resultado = medidaPolarizacion

      BigDecimal(resultado).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    }


    def calcularFrecuencia(intervalos: Vector[(Double, Double)], creenciasEspecificas: SpecificBelief, nAgentes: Int): Vector[Double] = {

      if(creenciasEspecificas.length<=256){
        for {
          (s, t) <- intervalos
          cantidadIntervalo = for {
            w <- creenciasEspecificas
            if (w >= s && (t match {
              case 1.0 => w <= t // Último intervalo, usar <=
              case _ => w < t    // Intervalos intermedios, usar <
            }))
          } yield w
        } yield cantidadIntervalo.length.toDouble / nAgentes
      } else {
          val mitad = creenciasEspecificas.length/2
          val b1 = creenciasEspecificas.slice(0,mitad)
          val b2 = creenciasEspecificas.slice(mitad,creenciasEspecificas.length)
          val (frecuenciaIzq,frecuenciaDer) = parallel(calcularFrecuencia(intervalos,b1, nAgentes),
            calcularFrecuencia(intervalos, b2,nAgentes))
          frecuenciaIzq.zip(frecuenciaDer).map { case (f1, f2) => f1 + f2}
      }
    }
    aux
  }

def confBiasUpdateParRecursivo(b: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
  val (wg, _) = swg

  // Función para calcular los beliefs actualizados para un rango específico
  def actualizarBelief(indices: Range): Vector[Double] = {
    indices.map { index =>
      val fila = (0 until b.length).par.map(j => wg(j, index))
      val Ai = (0 until fila.length).filter(j => fila(j) > 0)
      val influenceSum = Ai.map { j =>
        (1.0 - math.abs(b(j) - b(index))) * fila(j) * (b(j) - b(index))
      }.sum
      if (Ai.isEmpty) b(index) else b(index) + influenceSum / Ai.length
    }.toVector
  }

  // Función recursiva para dividir y procesar
  def dividirYProcesar(indices: Range): Vector[Double] = {
    if (indices.size <= 256) { // Caso base: rango con un solo índice
      actualizarBelief(indices)
    } else {
      val mid = indices.start + indices.size / 2
      val (izq, der) = parallel(
        dividirYProcesar(indices.start until mid),
        dividirYProcesar(mid until indices.end)
      )
      izq ++ der
    }
  }

  dividirYProcesar(0 until b.length)
}

}
