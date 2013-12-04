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

abstract class Mutation[T] extends ((T, Double) => T)

abstract class ChromosomeMutation extends Mutation[Chromosome]

abstract class GeneMutation[T] extends Mutation[Gene]

abstract class EncodingMutation extends Mutation[Encoding]


object BitEncodingMutation extends EncodingMutation {
  def apply(that: Encoding, prob: Double): Encoding = {

    that match {
      case v1: BitEncoding => {
        val probability = prob / v1.length

        var currentValue = v1.v

        (0 to v1.length-1).foreach { x =>
          if (Probability.coin(probability)) {
            currentValue = currentValue.flipBit(x)
          }
        }

        BitEncoding(currentValue, v1.op, v1.length)
      }

      case _ => that
    }
  }
}

object SimpleGeneMutation extends GeneMutation {
  def apply(v1: Gene, v2: Double): Gene = {
    val probability = v2/v1.sequence.length

    val smuta = v1.sequence.map(x => x.mutate(probability))

    Gene(v1.typename, smuta)
  }
}

object SimpleChromosomeMutation extends ChromosomeMutation {
  def apply(v1: Chromosome, prob: Double): Chromosome = {

    val probability = prob / v1.genes.length
    val smuta = v1.genes.map(x => x.mutate(probability))

    new Chromosome(smuta)
  }
}

