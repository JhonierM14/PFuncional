import Opinion._
import Benchmark._

//def allExtremeBelief(nags:Int): SpecificBelief = {
//  val middle = nags/2
//  Vector.tabulate(nags)((i: Int) => if(i < middle) 0.0 else 1.0)
//}

//val sb_ext = allExtremeBelief(100)
//val sb_cons = consensusBelief(0.2)(100)
//val sb_unif = uniformBelief(100)
//val sb_triple = allTripleBelief(100)
//val sb_midly = midlyBelief(100)
//
val rho1 = rho(1.2,1.2)
val rho2 = rho(2.0,1.0)
//
val dist1 = Vector(0.0, 0.25,0.50,0.75,1.0)
val dist2 = Vector(0.0,0.2,0.4,0.6,0.8,1.0)
//
rho1(sb_ext,dist1)
rho2(sb_ext,dist1)
rho1(sb_ext,dist2)
rho2(sb_ext,dist2)
//
//rho1(sb_cons,dist1)
//rho2(sb_cons,dist1)
//rho1(sb_cons,dist2)
//rho2(sb_cons,dist2)
//
//rho1(sb_unif,dist1)
//rho2(sb_unif,dist1)
//rho1(sb_unif,dist2)
//rho2(sb_unif,dist2)
//
//rho1(sb_triple,dist1)
//rho2(sb_triple,dist1)
//rho1(sb_triple,dist2)
//rho2(sb_triple,dist2)
//
//rho1(sb_midly,dist1)
//rho2(sb_midly,dist1)
//rho1(sb_midly,dist2)
//rho2(sb_midly,dist2)

val i1_10 = i1(10)
val i2_10 = i2(10)
val i3_65k = i1(655)

//showWeightedGraph(i1_10)
//showWeightedGraph(i2_10)

//val sbu_10 = uniformBelief(10)
//confBiasUpdatePar(sbu_10,i1_10)
//rho1(sbu_10,dist1)
//rho1(confBiasUpdatePar(sbu_10,i1_10),dist1)
//
//val smb_10 = midlyBelief(10)
//confBiasUpdate(smb_10,i1_10)
//rho1(smb_10,dist1)
//rho1(confBiasUpdate(smb_10,i1_10),dist1)



for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10,2)
}yield (b, rho1(b,dist1))
//
//for {
//  b <- simulate(confBiasUpdate, i1_10, smb_10,2)
//}yield (b, rho1(b, dist1))

val i2_32768 = i2(32768)
//
val dist_1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
//
val sbms = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)
//
val sbes = for {
  n <- 2 until 16
  nags = math.pow(2,n).toInt
} yield allExtremeBelief(nags)
//
val sbts = for {
  n <- 2 until 16
  nags = math.pow(2,n).toInt
} yield allTripleBelief(nags)
//
val polSec = rho(1.2, 1.2)
//
val evolsSec = for {
  i <- 0 until sbms.length
} yield {
  simEvolucion(Seq(sbms(i),sbes(i),sbts(i)), i2_32768, 10, polSec, confBiasUpdate, likert5,
    "Simulacion_Secuencial_" ++ i.toString ++ "-" ++ sbms(i).length.toString)}



