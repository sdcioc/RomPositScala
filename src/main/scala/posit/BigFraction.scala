package posit

import java.math.BigInteger

class BigFraction(val numerator: BigInt = 0, val denominator: BigInt = 1) {
  var isZero: Boolean = numerator == 0
  var isDividedByZero: Boolean = denominator == 0

  /*
  def gcd(a : BigInt, b : BigInt) : BigInt = {
    b match {
      case x if x==0 => a
      case x if x>a => gcd(x,a)
      case x => gcd(x, a%x)
    }
  }
  */

  def gcd(a: BigInt, b: BigInt): BigInt = {
    var a_aux: BigInt = a
    var b_aux: BigInt = b
    var aux: BigInt = 0
    while (b_aux != 0) {
      if (b_aux > a_aux) {
        aux = b_aux
        b_aux = a_aux
        a_aux = aux
      } else {
        aux = a_aux
        a_aux = b_aux
        b_aux = aux % b_aux
      }
    }
    return a_aux
  }

  def normalize(return_numerator: BigInt, return_denominator: BigInt): BigFraction = {
    var return_gcd: BigInt = new BigInteger("1")
    if (return_numerator != 0 && return_numerator.abs != 1 && return_denominator != 0 && return_denominator.abs != 1) {
      return_gcd = gcd(return_numerator.abs, return_denominator.abs)
    }
    if (return_numerator == 0) {
      return new BigFraction()
    }
    if (return_denominator < 0) {
      return new BigFraction(return_numerator / return_gcd * -1, return_denominator / return_gcd * -1)
    } else {
      return new BigFraction(return_numerator / return_gcd, return_denominator / return_gcd)
    }
  }


  //BigFraction addition
  def +(that: BigFraction): BigFraction = {
    var return_numerator: BigInt = this.numerator * that.denominator + this.denominator * that.numerator
    var return_denominator: BigInt = this.denominator * that.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }


  //BigFraction substition
  def -(that: BigFraction): BigFraction = {
    var return_numerator: BigInt = this.numerator * that.denominator - this.denominator * that.numerator
    var return_denominator: BigInt = this.denominator * that.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction multiply
  def *(that: BigFraction): BigFraction = {
    var return_numerator: BigInt = this.numerator * that.numerator
    var return_denominator: BigInt = this.denominator * that.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide
  def /(that: BigFraction): BigFraction = {
    var return_numerator: BigInt = this.numerator * that.denominator
    var return_denominator: BigInt = this.denominator * that.numerator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide
  def inverse(): BigFraction = {
    var return_numerator: BigInt = this.denominator
    var return_denominator: BigInt = this.numerator
    var return_value: BigFraction = new BigFraction(return_numerator, return_denominator)
    return_value.isZero = return_value.numerator == 0
    return_value.isDividedByZero = return_value.denominator == 0
    return return_value
  }

  //BigFraction addition with Int
  def +(that: Int): BigFraction = {
    var return_numerator: BigInt = this.numerator + this.denominator * that
    var return_denominator: BigInt = this.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction substition with Int
  def -(that: Int): BigFraction = {
    var return_numerator: BigInt = this.numerator - this.denominator * that
    var return_denominator: BigInt = this.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction multiply with Int
  def *(that: Int): BigFraction = {
    var return_numerator: BigInt = this.numerator * that
    var return_denominator: BigInt = this.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide with Int
  def /(that: Int): BigFraction = {
    var return_numerator: BigInt = this.numerator / that
    var return_denominator: BigInt = this.denominator
    var return_value: BigFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction ==
  def ==(that: BigFraction): Boolean = {
    return (this.numerator * that.denominator) == (this.denominator * that.numerator)
  }

  //BigFraction <
  def <(that: BigFraction): Boolean = {
    return (this.numerator * that.denominator) < (this.denominator * that.numerator)
  }

  //BigFraction >
  def >(that: BigFraction): Boolean = {
    return (this.numerator * that.denominator) > (this.denominator * that.numerator)
  }

  def isInteger(): Boolean = {
    return (this.denominator == 1)
  }

  def toIntFraction(): IntFraction = {
    return new IntFraction(this.numerator.toInt, this.denominator.toInt)
  }

  // Big Int to string
  override def toString: String = {
    return this.numerator.toString() + "/" + this.denominator.toString()
  }
}
