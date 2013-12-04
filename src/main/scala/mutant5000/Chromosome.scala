package mutant5000
/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-02 :: 19:50
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

class Chromosome(protected[mutant5000] val genes : Seq[Gene] = Seq.empty,
                 val name:String ="PierreGabrielle",
                 val mutation : ChromosomeMutation = SimpleChromosomeMutation)
  extends Genetic[Chromosome] with Assessable {

  def |+|(that: Chromosome): Chromosome = {
    val s1 = genes
    val s2 = that.genes

    val s1s2 = s1.zip(s2)

    val scombination = s1s2.map( x => x._1.|+|(x._2))

    new Chromosome(scombination)
  }

  def mutate(prob: Double): Chromosome = {
      mutation(this, prob)
  }
}

