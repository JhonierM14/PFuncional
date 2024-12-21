import Comete.DistributionValues
import Opinion._
import org.scalameter._
import plotly._
import layout._
import Plotly._

package object Benchmark {
  def tiempoDe[T](body: => T) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (body)
    timeA1
  }

  // Construcción de Beliefs
  // Build uniform belief state.
  def uniformBelief(nags:Int):SpecificBelief= {
    Vector.tabulate(nags)((i:Int) => (i+1).toDouble/nags.toDouble)
  }

  // Builds mildly polarized belief state, in which
  //    half of agents has belief decreasing from 0.25, and
  //    half has belief increasing from 0.75, all by the given step.
  def midlyBelief(nags:Int):SpecificBelief= {
    val middle= nags/2
    Vector.tabulate(nags)((i:Int) => if (i < middle) math.max(0.25 - 0.01*(middle-i-1),0)
    else math.min(0.75 - 0.01*(middle-i),1))
  }

  //Builds extreme polarized belief state, in which half
  //    of the agents has belief 0, and half has belief 1.
  def allExtremeBelief(nags:Int):SpecificBelief= {
    val middle= nags/2
    Vector.tabulate(nags)((i:Int) => if (i < middle) 0.0 else 1.0)
  }

  // Builds three-pole belief state, in which each
  //    one third of the agents has belief 0, one third has belief 0.5,
  //    and one third has belief 1.

  def allTripleBelief(nags:Int):SpecificBelief= {
    val oneThird= nags/3
    val twoThird = (nags/3)*2
    Vector.tabulate(nags)((i:Int) => if (i < oneThird) 0.0 else if (i>=twoThird) 1.0 else 0.5)
  }

  // Builds consensus belief state, in which each
  //    all agents have same belief.
  def consensusBelief(b:Double)(nags:Int):SpecificBelief= {
    Vector.tabulate(nags)((i:Int) => b)
  }

  // Construcción de grafos de influencia
  def i1(nags:Int):SpecificWeightedGraph ={
    ((i:Int, j:Int) => if (i==j) 1.0
    else if (i<j) 1.0/(j-i).toDouble
    else 0.0,nags)
  }

  def i2(nags:Int):SpecificWeightedGraph ={
    ((i:Int, j:Int) => if (i==j) 1.0
    else if (i<j) (j-i).toDouble/nags.toDouble
    else (nags-(i-j)).toDouble/nags.toDouble, nags)
  }

  // Comparador de tiempos de medidas de polarización secuencial y paralela
  def compararMedidasPol(sb:Seq[SpecificBelief], dist:DistributionValues,
                         pol1:AgentsPolMeasure, pol2:AgentsPolMeasure) = {
    // Recibe una secuencia de opiniones iniciales de agentes sb, de cualquiere tamaño,
    // Unos valores de distribución dist y dos funciones para calcular
    // la misma medida de polarización (la primera secuencial y la segunda concurrente)
    // y devuelve una secuencia de sextuplas de valores de la forma
    // (n, p1, p2, t1, t2, t1/t2) donde n es el tamaño de la secuencia de opiniones inicial,
    // p1 y p2 las polarizaciones de la creencia calculadas con pol1 y pol2,
    // t1 es el tiempo que se demoró en calcular la polarización con pol1,
    // t2 es el tiempo que se demoró en calcular la polarización con pol2,
    // y t1/t2 es la aceleración de pol2 con respecto a pol1
    for {
      b <- sb
      p1 = pol1(b,dist)
      p2 = pol2(b,dist)
      t1 = tiempoDe(pol1(b,dist))
      t2 = tiempoDe(pol2(b,dist))
    } yield (b.length, p1, p2, t1, t2, t1.value/t2.value)
  }

  // Comparador de tiempos de funciones de actualizacion secuencial y paralela
  def compararFuncionesAct(sb:Seq[SpecificBelief], swg:SpecificWeightedGraph,
                           f1:FunctionUpdate, f2:FunctionUpdate) = {
    // Recibe una secuencia de opiniones iniciales de agentes sb, de cualquiere tamaño,
    // Un grafo de influencias para la mayor cantidad de agentes en una opinión
    // y dos funciones para actualización de la opinión
    // (la primera secuencial y la segunda concurrente)
    // y devuelve una secuencia de cuádruplas de valores de la forma
    // (n, t1, t2, t1/t2) donde n es el tamaño de la secuencia de opiniones inicial,
    // t1 es el tiempo que se demoró en calcular la actualización de opinión con f1,
    // t2 es el tiempo que se demoró en calcular la actualización de opinión con f2,
    // y t1/t2 es la aceleración de f2 con respecto a f1
    for {
      b <- sb
      t1 = tiempoDe(f1(b,swg))
      t2 = tiempoDe(f2(b,swg))
    } yield (b.length, t1, t2, t1.value/t2.value)
  }


  // Simulador de evolución de opinión
  def simEvolucion(sb:Seq[SpecificBelief],swg:SpecificWeightedGraph,
                    tiempoSim:Int, pol:AgentsPolMeasure, fu:FunctionUpdate,
                   dist:DistributionValues,name:String) = {
    // Recibe una secuencia de opiniones iniciales de agentes sb,
    // todas las opiniones del mismo tamaño, un grafo de influencia swg,
    // El número de pasos de la simulación, tiempoSim,
    // una medida de polarización de agentes, pol, una función de actualización
    // de la oopinión, fu, unos valores de distribución, dist y
    // un nombre,name, para el archivo html de salida,
    // y dibuja en un archivo la evolución en el rtiempo de la polarización
    // de los agentes iniciando en cada una de las opiniones iniciales
    val ejet = 1 to tiempoSim
    val evolPols = for {
      i <- 0 until sb.length
    } yield {for {
      b <- simulate(fu,swg,sb(i),tiempoSim)
    } yield pol(b,dist)
    }
    val plotSim = for {
      i <- 0 until sb.length
    } yield Scatter(ejet, evolPols(i)).withName(name ++ "-" ++ i.toString)

    val laySimSeq = Layout().withTitle(name)
    plotSim.plot("simulEvol.html", laySimSeq)
    evolPols
  }
}
