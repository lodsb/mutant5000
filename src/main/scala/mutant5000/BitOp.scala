package mutant5000
/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-12-02 :: 18:26
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

abstract class BitOp {
  def apply(a1 : BigInt, a2: BigInt) : BigInt
}

object Or extends BitOp {
  def apply(a1: BigInt, a2: BigInt): BigInt = a1 | a2
}

object And extends BitOp {
  def apply(a1: BigInt, a2: BigInt): BigInt = a1 & a2
}

object Xor extends BitOp {
  def apply(a1: BigInt, a2: BigInt): BigInt = a1 ^ a2
}
/*
object Nor extends BitOp {
  def apply(a1: BigInt, a2: BigInt): BigInt = !(a1 | a2)
}

object Nand extends BitOp {
  def apply(a1: BigInt, a2: BigInt): BigInt = !(a1 & a2)
}
  */




