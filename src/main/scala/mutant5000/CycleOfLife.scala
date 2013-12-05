package mutant5000

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-03 :: 11:26
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

class CycleOfLife(population: Population,
                  elitism: Double,
                  crossOverProbability: Double,
                  mutationProbability: Double) {

  def evolve(num: Int, keepBest: Int = 200, prettyPrint: Boolean = true, earlyStop: Boolean = true) = {
    var generation = 0
    var done = false
    while (generation <= num && (!(done && earlyStop))) {
      val mates = population.mates(crossOverProbability, elitism)

      if (mates.isDefined) {
        var offspring = mates.get.map {
          couple =>
            couple._1 |+| couple._2
        }

        if (mutationProbability > 0) {
          offspring = offspring.map {
            o =>
              o.mutate(mutationProbability)
          }
        }

        population.add(offspring)
        population.keepBest(keepBest)

        if (prettyPrint) {
          println("****\nCycle: " + generation + "\nPopulation Size: " + population.size + "\nBest: " + population.best + "\n")
        }

        if (population.bestScore.get == 0.0) {
          done = true
        }

      }

      generation = generation + 1
    }
  }
}
