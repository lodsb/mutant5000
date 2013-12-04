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


trait Fitnessfunction[T <: Assessable] {
  def assess(a: T) : Double
}

object GeneFitnessFunctions {
  private var fitnessFunctions = Map[String, Fitnessfunction[Gene]]()

  def set(map: Map[String, Fitnessfunction[Gene]]) = {
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
}

object SimpleChromosomeFitness extends Fitnessfunction[Chromosome] {
  def assess(a: Chromosome): Double = {
    val geneFitnessess = a.genes.map( x => GeneFitnessFunctions(x).getOrElse(0.0))

    geneFitnessess.sum / geneFitnessess.length
  }
}

