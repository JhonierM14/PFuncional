import scala.math.{abs, pow}

package object Comete {
  type DistributionValues = Vector[Double]

  type Frequency = Vector[Double]

  type Distribution = (Frequency, DistributionValues)

  type PolMeasure = Distribution => Double

  def minp(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    val listPuntos = List.range(0, 11, 1)
    val list = listPuntos.map(x => min + x * (max - min) / 10)

    val resultadosList = list.map(x => f(x))

    val minimoFuncion = resultadosList.min

    def posicion(x: Double, list: List[Double], n: Int = 0): Int = {
      def auxiliar(x: Double, list: List[Double], n: Int): Int = {
        if (list.head == x) {
          n
        } else {
          auxiliar(x, list.tail, n + 1)
        }
      }

      auxiliar(x, list, n)
    }

    val posicionValor = posicion(minimoFuncion, resultadosList)

    val newMin = if (posicionValor > 0) list(posicionValor - 1) else min
    val newMax = if (posicionValor < list.length - 1) list(posicionValor + 1) else max

    if (newMax - newMin < prec) {
      (newMin + newMax) / 2
    } else {
      minp(f, newMin, newMax, prec)
    }
  }

  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {

    def minRhoAux(distribution: Distribution): Double = {

      def rhoAux(p: Double): Double = {
        val (phi, y) = distribution

        (phi zip y).map { case (a, b) =>
          pow(a, alpha) * pow(abs(b - p), beta)
        }.sum
      }
      val resultadoPmin = minp(rhoAux, 0, 1, 0.0001)

      BigDecimal(rhoAux(resultadoPmin)).setScale(4,BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    minRhoAux
  }

  def normalizar(m: PolMeasure ) : PolMeasure = {
    def aux(f:Distribution) : Double = {
      val (_, x) = f
      val X = x.length
      val new_phi : Vector[Double] = (0 until X).map {
        case 0 => 0.5
        case i if i == X - 1 => 0.5
        case _ => 0.0
      }.toVector

      val a = m(f)
      val b = m(new_phi, x)
      a/b
    }
    aux
  }
}
