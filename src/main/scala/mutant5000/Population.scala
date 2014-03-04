package mutant5000

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-04 :: 22:18
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

import scala.math._

class Population(initialPopulation : Seq[Chromosome], private val scoreFunc: Option[ScoreFunction[Chromosome]]) {
  private var population = List[(Double, Chromosome)]()
  private var random = scala.util.Random



  initialPopulation.foreach {x => this.add(x)}

  def add(c: Chromosome) = {
    if(scoreFunc.isDefined) {
      population = population :+ (scoreFunc.get.assess(c),c)
      population = population.sortBy(x => x._1)
    } else {
      new Exception("Missing Score function!")
    }
  }

  def add(c: Chromosome, score: Double) = {
    population = population :+ (score,c)
    population = population.sortBy(x => x._1)
  }

  def add(seq: Seq[Chromosome]) = {
    if(scoreFunc.isDefined) {
    seq.foreach({ c =>
      population = population :+ (scoreFunc.get.assess(c),c)
    })

    population = population.sortBy(x => x._1)
    } else {
      new Exception("Missing Score function!")
    }


  }

  def keepBest(num: Int) = {
    if (population.size > num) {
      population = population.slice(0, num)
    }
  }

  def degradePopulationScoresBy(subtract: Double) = {
    population = population.map({ x =>
      (x._1-subtract, x._2)
    })
  }

  def mates(crossOverProbability: Double, elitism: Double) : Option[Seq[(Chromosome, Chromosome)]] = {
    var ret : Option[Seq[(Chromosome, Chromosome)]] = None
    if (population.size > 2) {
      val elitismCount = (population.size*elitism).toInt

      val plebs = population.slice(elitismCount, population.size-1)

      if (plebs.size > 2) {
        var mates = Seq[(Chromosome, Chromosome)]()

        // create a mating population
        plebs.foreach({ x =>
          if (random.nextDouble() <= crossOverProbability) {
            val v1 = monteCarlo(plebs)
            val v2 = monteCarlo(plebs)

            mates = mates :+ (v1, v2)
          }
        })

        ret = Some(mates)
      }
    }

    ret
  }
  def size = population.size

  private def monteCarlo[A](weightedList: List[(Double,A)]): A = {
    weightedList(random.nextInt(weightedList.length)) match {
      case (f,s) if (1-f) > random.nextFloat => s
      case _ => monteCarlo(weightedList)
    }
  }

  def bestMates : Option[(Chromosome, Chromosome)] = {
    var ret : Option[(Chromosome, Chromosome)] = None

    if (population.size > 1) {
      ret = Some(population(0)._2, population(1)._2)
    }

    ret
  }

  def best : Option[(Double, Chromosome)] = {
    if (!population.isEmpty) {
      Some(population(0))
    } else {
      None
    }
  }

  def bestScore = best.map(x => x._1)
  def bestSpecimen = best.map(x => x._2)

  override def toString(): String = {
    "**** Population ****\n" + population.map(x => "Score: "+x._1+"\n"+x._2.toString())
  }
}