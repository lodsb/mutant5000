import mutant5000.{Fitnessfunction, Chromosome}

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

class Population(private val fitnessFunc: Fitnessfunction[Chromosome]) {
  private var population = List[(Double, Chromosome)]()

  def add(c: Chromosome) = {
    population = population :+ (fitnessFunc.assess(c),c)
    population = population.sortBy(x => x._1)
  }

  def keepBest(num: Int) = {
    if (population.size >= num) {
      population = population.slice(0, num-1)
    }
  }

  def bestMates : Option[(Chromosome, Chromosome)] = {
    var ret : Option[(Chromosome, Chromosome)] = None

    if (population.size > 1) {
      ret = Some(population(0)._2, population(1)._2)
    }

    ret
  }
}