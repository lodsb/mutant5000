mutant5000
==========

simple library for GAs:
* allows different types of genes (encodings) to be mixed within a chromosome (specimen) (binaries, characters,...)
* supports several fitness functions for each (sub-)gene and chromosome
* mutation and combination can be altered per gene and encoding

```scala

import mutant5000._

object Test extends App {

  val objectiveString = "Hello, World..."
  val objectiveInts = Seq(1,2,11,14)

  val stringGeneName = "StringGene"
  val intSeqGeneName = "IntSeqGene"

  // create population
  val populationSequence = Seq.fill[Chromosome](1000) {
    // random characterstrings
    val stringGene = GeneBuilder(objectiveString.length-1, stringGeneName,
                      SimpleGeneMutation, ConcatenatingGeneCombination){
                        Encoding.CharacterEncoding.random
                    }

    // random bitstrings with length 4
    val intGene = GeneBuilder(objectiveInts.length-1, intSeqGeneName,
                       SimpleGeneMutation, WholeGeneCombination){
                          Encoding.BitEncoding.random(4, Or)
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
                                    elitism=0.001,
                                    crossOverProbability = 0.99,
                                    mutationProbability = 0.8)

  // keepBest reduces the population after each generation
  // earlyStop stops the evolution once a score of 0.0 (best) has been
  // reached
  cycleOfLife.evolve(1000, keepBest = 100,
                           prettyPrint = true,
                           earlyStop = true)

  println(initialPopulation.best)
}
```
