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

case class Gene(val typename: String, encodings: Seq[Encoding],
            mutation: GeneMutation = SimpleGeneMutation,
            combination: GeneCombination = WholeGeneCombination)
  extends Genetic[Gene] with Assessable {

  def sequence = encodings

  def |+|(that: Gene): Gene = {
    combination.combine(this, that)
  }

  def mutate(prob: Double): Gene = {
    mutation(this,prob)
  }
}

object GeneBuilder {
  def apply(num: Int, name: String, mutation: GeneMutation = SimpleGeneMutation,
             combination: GeneCombination = WholeGeneCombination)(func: => Encoding) : Gene = {
    val s = (0 to num).map( x => func)

    Gene(name, s, mutation, combination)
  }
}

