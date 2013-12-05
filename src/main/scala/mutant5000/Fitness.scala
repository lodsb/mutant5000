package mutant5000

import scala.Predef._

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-04 :: 22:20
    >>  Origin: mutant5000
    >>
  +3>>
    >>  Copyright (c) 2013:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */


/** *
  * score should be: lowest -> best
  *
  */
trait ScoreFunction[T <: Assessable] {
  def assess(a: T) : Double
}

class GeneScoreFunctions {
  private var fitnessFunctions = Map[String, ScoreFunction[Gene]]()

  def set(map: Map[String, ScoreFunction[Gene]]) = {
    fitnessFunctions = map
  }

  def apply(gene: Gene) : Option[Double] = {
    val fitnessFunction = fitnessFunctions.get(gene.typename)
    var ret : Option[Double] = None

    if(fitnessFunction.isDefined) {
      ret = Some(fitnessFunction.get.assess(gene))
    }

    ret
  }

  def add(s: String, f: ScoreFunction[Gene]) = {
    fitnessFunctions = fitnessFunctions + (s -> f)
  }
}

class SimpleChromosomeScore(geneFitness: GeneScoreFunctions) extends ScoreFunction[Chromosome] {
  def assess(a: Chromosome): Double = {
    val geneFitnesses = a.genes.map( x => geneFitness(x).getOrElse(0.0))

    geneFitnesses.sum / geneFitnesses.length
  }
}

// levinstein?

object LevensteinGeneScore {
  import scala.math.min

  // taken from rosettacode
  // http://rosettacode.org/wiki/Levenshtein_distance#Scala
  def minimum(i1: Int, i2: Int, i3: Int)=min(min(i1, i2), i3)
  def distance(s1:String, s2:String)={
    val dist=Array.tabulate(s2.length+1, s1.length+1){(j,i)=>if(j==0) i else if (i==0) j else 0}

    for(j: Int<-(1 to s2.length); i: Int <-(1 to s1.length))
      dist(j)(i)=if(s2(j-1)==s1(i-1)) {
        dist(j-1)(i-1)
      } else {
        minimum(dist(j-1)(i)+1, dist(j)(i-1)+1, dist(j-1)(i-1)+1)
      }

    dist(s2.length)(s1.length)
  }

  def apply(s: String) : ScoreFunction[Gene] = {
    new ScoreFunction[Gene] {
      def assess(a: Gene): Double = {
        var characterArray = List[Char]()

        a.sequence.foreach{ enc =>
          enc match {
            case x:CharacterEncoding => {
              characterArray = characterArray :+ x.v
            }
            case _ =>
          }
        }
        distance(characterArray.mkString, s).toDouble / s.length.toDouble
      }
    }
  }
}

object EqualityGeneScore {
  def apply[T](seq: Seq[T]) : ScoreFunction[Gene] = {
    new ScoreFunction[Gene] {
      def assess(a: Gene): Double = {
        var intList = List[Int]()

        a.sequence.foreach { enc =>
          enc match {
            case x: Encoding => {
              intList = intList :+ x.toInt
            }
          }
        }

        // not really exact...
        (intList.zip(seq).count {case(x,y) => x.toString != y.toString}) / seq.length
      }
    }
  }
}

