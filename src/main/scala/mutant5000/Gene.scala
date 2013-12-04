package mutant5000

import scala._


/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-02 :: 19:07
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

// create random -> encoding

class Gene(typename: String, seq: Seq[Encoding])
  extends Genetic[Gene] with Assessable {

  def sequence = seq

  def |+|(that: Gene): Gene = {
    val s1 = this.sequence
    val s2 = that.sequence

    val s1s2 = s1.zip(s2)

    val scombination = s1s2.map({ x =>
      x._1|+|(x._2)
    })

    new Gene(typename, scombination)
  }

  def mutate(prob: Double): Gene = {
    val probability = prob/this.sequence.length

    val smuta = this.sequence.map(x => x.mutate(probability))

    new Gene(this.typename, smuta)
  }
}

