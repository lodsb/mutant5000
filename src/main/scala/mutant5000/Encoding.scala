package mutant5000

import util.Random


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
  def toInt: Int
}

case class BrokenEncoding[T](v: T) extends Encoding {
  def |+|(that: Encoding): Encoding = that
  def mutate(prob: Double): Encoding = this

  def toInt = Integer.MAX_VALUE // argh
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

  def toInt: Int = v.toInt
}

case class CharacterEncoding(protected[mutant5000] val v: Char, mutation: EncodingMutation = CharacterEncodingMutation)
  extends Encoding {
  def |+|(that: Encoding): Encoding = this

  def mutate(prob: Double): Encoding = {
    mutation(this, prob)
  }

  def toInt: Int = v.toInt
}

case class IntegerEncoding(protected[mutant5000] val v: Int, mutation: EncodingMutation)
  extends Encoding {
  def |+|(that: Encoding): Encoding = this

  def mutate(prob: Double): Encoding = {
    mutation(this, prob)
  }

  def toInt: Int = v.toInt
}

object Encoding {
  private val r = Random

  object BitEncoding {
    // randomize bitop as well?
    def random(length: Int, op: BitOp ,  mutation: EncodingMutation = BitEncodingMutation) : BitEncoding = {
      val max = scala.math.pow(2, length).toLong
      val v = r.nextLong() % max

      new BitEncoding(BigInt(v), op , length, mutation)
    }
  }

  object CharacterEncoding {
    def random : CharacterEncoding =  {
      val v = (r.nextInt(90) + 32).toChar

      new CharacterEncoding(v)
    }
  }

  object IntegerEncoding {
    def init(value: Int, mutation : EncodingMutation) : IntegerEncoding = {
      new IntegerEncoding(value, mutation);
    }
  }

}

