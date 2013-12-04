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

abstract class Encoding extends Genetic[Encoding]{
}

case class BrokenEncoding[T](v: T) extends Encoding {
  def |+|(that: Encoding): Encoding = that
  def mutate(prob: Double): Encoding = this
}

case class BitEncoding(protected[mutant5000] val v: BigInt ,
                  protected[mutant5000] val op: BitOp,
                  protected[mutant5000] val length: Int,
                  mutation: EncodingMutation = BitEncodingMutation) extends Encoding {

  def |+|(that: Encoding): Encoding = {
  that match {
    case x:BitEncoding => BitEncoding(op(this.v, x.v), op, length)
    case _ => that
  }
}

  def mutate(prob: Double): Encoding = {
    mutation(this, prob)
  }
}

// dann einem Gene übergeben
abstract class EncodingCreator {
  def create // parameters???
}

object Encoding {
  object BitEncoding extends EncodingCreator {
    def create {}
  }
}

