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

trait Combination[T] {
  def combine(x:T, y:T) : T
}

abstract class GeneCombination extends Combination[Gene]

/**
 * Combines (mates) all encodings of two genes
 */
object WholeGeneCombination extends GeneCombination {
  def combine(x: Gene, y: Gene): Gene = {
    val s1 = x.sequence
    val s2 = y.sequence

    val s1s2 = s1.zip(s2)

    val scombination = s1s2.map({ x =>
      x._1|+|(x._2)
    })

    x.copy(encodings = scombination)
  }
}

/**
 * Concatenates genes at random position without mating the encodings
 */
object ConcatenatingGeneCombination extends GeneCombination {
  private val random = scala.util.Random

  def combine(x: Gene, y: Gene): Gene = {
    val pivot = random.nextInt(x.sequence.length)

    val scombination = x.sequence.slice(0, pivot) ++ y.sequence.slice(pivot, y.sequence.length)

    x.copy(encodings = scombination)
  }
}
