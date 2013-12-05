import mutant5000._

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

object Test extends App {

  val objectiveString = "Hello, World..."
  val objectiveInts = Seq(1,2,11,14)

  val stringGeneName = "StringGene"
  val intSeqGeneName = "IntSeqGene"
  // create population

  val populationSequence = Seq.fill[Chromosome](100) {
    // random characterstrings
    val stringGene = GeneBuilder(objectiveString.length, stringGeneName,
                      SimpleGeneMutation, ConcatenatingGeneCombination){
                        Encoding.CharacterEncoding.random
                    }

    // random bitstrings with length 4
    val intGene = GeneBuilder(objectiveInts.length, intSeqGeneName,
                       SimpleGeneMutation, WholeGeneCombination){
                          Encoding.BitEncoding.random(4, Xor)
                    }

    new Chromosome(Seq(stringGene,intGene))
  }

  // create the score functions (inverse fitness)
  val geneScorefs = new GeneScoreFunctions
  geneScorefs.set(
    Map(
      stringGeneName -> LevensteinGeneScore(objectiveString),
      intSeqGeneName -> EqualityGeneScore(objectiveInts)
    )
  )

  val chromosomeScore = new SimpleChromosomeScore(geneScorefs)

  val initialPopulation = new Population(populationSequence, chromosomeScore)

  val cycleOfLife = new CycleOfLife(initialPopulation,
                                    elitism=0.1,
                                    crossOverProbability = 0.8,
                                    mutationProbability = 0.9)

  cycleOfLife.evolve(10000, prettyPrint = true)

  println(initialPopulation.best)
}
