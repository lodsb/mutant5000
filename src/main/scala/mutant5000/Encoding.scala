package mutant5000

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-02 :: 18:14
    >>  Origin: 
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

abstract class Encoding[T <: Encoding[_]] extends Genetic[T]{
}

class BitEncoding(protected val v: BigInt , op: BitOp, length: Int) extends Encoding[BitEncoding] {

  def |+|(that: BitEncoding): BitEncoding = {
  new BitEncoding(op(this.v, that.v), op, length)
}

  def mutate(prob: Double): BitEncoding = {
    val probability = prob / length

    var currentValue = this.v

    (0 to length-1).foreach { x =>
      if (Probability.coin(probability)) {
        currentValue = currentValue.flipBit(x)
      }
    }

    new BitEncoding(currentValue, op, this.length)
  }
}

