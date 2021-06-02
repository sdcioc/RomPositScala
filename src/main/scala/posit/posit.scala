package posit

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.math.BigInteger
import scala.Array._




// TODO : AICI SE SCHIMBA IN INTț

@SerialVersionUID(124L)
class IntFraction (val numerator : Int = 0, val denominator : Int = 1) extends Serializable {
  var isZero : Boolean = numerator == 0
  var isDividedByZero : Boolean = denominator == 0

  def gcd(a : Int,b : Int) : Int = {
    var a_aux : Int = a
    var b_aux : Int = b
    var aux : Int = 0
    while(b_aux != 0) {
      if (b_aux  > a_aux) {
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

  def normalize(return_numerator : Int, return_denominator : Int) : IntFraction = {
    var return_gcd : Int = 1
    if(return_numerator!=0 && return_numerator.abs!=1 && return_denominator!=0 && return_denominator.abs!=1) {
      return_gcd = gcd(return_numerator.abs, return_denominator.abs)
    }
    if (return_numerator == 0) {
      return new IntFraction()
    }
    if(return_denominator < 0) {
      return new IntFraction(return_numerator/return_gcd * -1, return_denominator/return_gcd * -1)
    } else {
      return new IntFraction(return_numerator/return_gcd, return_denominator/return_gcd)
    }
  }



  //BigFraction addition
  def +(that : IntFraction) : IntFraction = {
    var return_numerator : Int = this.numerator * that.denominator + this.denominator * that.numerator
    var return_denominator : Int = this.denominator * that.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }


  //BigFraction substition
  def -(that : IntFraction) : IntFraction = {
    var return_numerator : Int = this.numerator * that.denominator - this.denominator * that.numerator
    var return_denominator : Int = this.denominator * that.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction multiply
  def *(that : IntFraction) : IntFraction = {
    var return_numerator : Int = this.numerator * that.numerator
    var return_denominator : Int = this.denominator * that.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide
  def /(that : IntFraction) : IntFraction = {
    var return_numerator : Int = this.numerator * that.denominator
    var return_denominator : Int = this.denominator * that.numerator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide
  def inverse() : IntFraction = {
    var return_numerator : Int = this.denominator
    var return_denominator : Int = this.numerator
    var return_value : IntFraction = new IntFraction(return_numerator, return_denominator)
    return return_value
  }


  //BigFraction negate
  def -() : IntFraction = {
    var return_numerator : Int = this.numerator * -1
    var return_denominator : Int = this.denominator
    var return_value : IntFraction = new IntFraction(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction addition with Int
  def +(that : Int) : IntFraction = {
    var return_numerator : Int = this.numerator + this.denominator * that
    var return_denominator : Int = this.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction substition with Int
  def -(that : Int) : IntFraction = {
    var return_numerator : Int = this.numerator - this.denominator * that
    var return_denominator : Int = this.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction multiply with Int
  def *(that : Int) : IntFraction = {
    var return_numerator : Int = this.numerator * that
    var return_denominator : Int = this.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction divide with Int
  def /(that : Int) : IntFraction = {
    var return_numerator : Int = this.numerator / that
    var return_denominator : Int = this.denominator
    var return_value : IntFraction = normalize(return_numerator, return_denominator)
    return return_value
  }

  //BigFraction ==
  def ==(that : IntFraction) : Boolean = {
    return (this.numerator * that.denominator) == (this.denominator * that.numerator)
  }

  //BigFraction <
  def <(that : IntFraction) : Boolean = {
    var val_1_1 : BigInt = new BigInteger(this.numerator.toString)
    var val_1_2 : BigInt = new BigInteger(that.denominator.toString)
    var val_2_1 : BigInt = new BigInteger(this.denominator.toString)
    var val_2_2 : BigInt = new BigInteger(that.numerator.toString)
    return (val_1_1 * val_1_2) < (val_2_1 * val_2_2)
  }

  //BigFraction >
  def >(that : IntFraction) : Boolean = {
    return (this.numerator * that.denominator) > (this.denominator * that.numerator)
  }

  def toBigFraction() : BigFraction = {
    return new BigFraction(this.numerator, this.denominator)
  }


  def isInteger() : Boolean = {
    return (this.denominator == 1)
  }

  def toNextIntegerFraction() : IntFraction = {
    if(this.isInteger()) {
      return new IntFraction(this.numerator)
    } else {
      return new IntFraction( (this.numerator/this.denominator)+1)
    }
  }

  // Big Int to string
  override def toString: String = {
    return this.numerator.toString() + "/" + this.denominator.toString()
  }
}




class BigHomogeneousSystem (val equationSystem :Array[Array[Int]]) {
  var bigfractionSystem : Array[Array[BigFraction]] = ofDim(equationSystem.length, equationSystem(0).length)
  val numberOfVariables : Int = equationSystem(0).length
  var numberOfFreeVariables : Int = 0

  def makeSystem() = {
    for(index_1 <- 0 until equationSystem.length) {
      for(index_2 <- 0 until equationSystem(index_1).length) {
        bigfractionSystem(index_1)(index_2) = new BigFraction(equationSystem(index_1)(index_2))
      }
    }

    //this.displaySystem()
    println("sistem încarcat")

    for (variable <- 0 until numberOfVariables) {
      println("first calc variable: " + variable + " of " + numberOfVariables)
      var sem: Boolean = false
      var index_1 : Int = variable - numberOfFreeVariables //the current index
      var index_2 : Int = index_1 //the index to go trough matrix
      //find the row with the variable
      while ( (sem == false) && (index_2 < bigfractionSystem.length)) {
        if( !(bigfractionSystem(index_2)(variable).isZero)) {
          sem = true
          if(index_2 != index_1) {
            //change the rows
            var aux_array : Array[BigFraction] = new Array[BigFraction](numberOfVariables)
            bigfractionSystem(index_1).copyToArray(aux_array)
            bigfractionSystem(index_2).copyToArray(bigfractionSystem(index_1))
            aux_array.copyToArray(bigfractionSystem(index_2))
          }
        } else {
          index_2 = index_2 + 1
        }
      }

      if(sem == true) {
        var jindex_1 : Int = variable
        //make the cell to have the value 1
        var to_multiply : BigFraction =  bigfractionSystem(index_1)(variable).inverse()
        //println(to_multiply.toString)
        for(jindex_1 <- variable until numberOfVariables) {
          bigfractionSystem(index_1)(jindex_1) = bigfractionSystem(index_1)(jindex_1) * to_multiply
        }


        //this.displaySystem()

        //substract the value from other equations
        for(index_2 <- 0 until equationSystem.length) {
          if(!bigfractionSystem(index_2)(variable).isZero && (index_2 != index_1)) {
            to_multiply = bigfractionSystem(index_2)(variable)
            //println("index: " + index_2 + " value: " + to_multiply.toString)
            for(jindex_1 <- variable until numberOfVariables) {
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_1)(jindex_1).toString)
              //println("jindex_1: " + jindex_1 + " value: " + (to_multiply * bigfractionSystem(index_1)(jindex_1)).toString)
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_2)(jindex_1).toString)
              bigfractionSystem(index_2)(jindex_1) = bigfractionSystem(index_2)(jindex_1) - (to_multiply * bigfractionSystem(index_1)(jindex_1))
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_2)(jindex_1).toString)
            }
          }
        }

      } else {
        numberOfFreeVariables = numberOfFreeVariables + 1
      }

      //this.displaySystem()
    }

    println("Ecuați finale")
    //this.displaySystem()

    //VERY BAD
    if(numberOfFreeVariables == 0) {
      println("sistem cu soluție trivială")
    } else {
      //aranajare frumos a variabilelor
      println("sistem cu soluție non trivială")
      for (variable <- 0 until numberOfVariables) {
        println("rearanje - variable: " + variable + " of " + numberOfVariables)
        if ((bigfractionSystem(variable)(variable).isZero)) {
          //se da mai jos totul
          //coloana curenta devine nula
          var prev_array : Array[BigFraction] = new Array[BigFraction](numberOfVariables)
          var aux_array : Array[BigFraction] = new Array[BigFraction](numberOfVariables)
          var zero_array : Array[BigFraction] = new Array[BigFraction](numberOfVariables)
          for(index <- 0 until numberOfVariables) {
            zero_array(index) = new BigFraction()
          }
          bigfractionSystem(variable).copyToArray(prev_array)
          zero_array.copyToArray(bigfractionSystem(variable))
          //transpunem toate coaloanele mai jos
          for(index <- (variable + 1) until numberOfVariables) {
            bigfractionSystem(index).copyToArray(aux_array)
            prev_array.copyToArray( bigfractionSystem(index))
            aux_array.copyToArray(prev_array)
          }
        }

        println("Rearanajare varaibile")
        //this.displaySystem()
      }


      var solutionVectors : Array[Array[BigFraction]] = ofDim(numberOfFreeVariables, numberOfVariables)
      for(index_1 <- 0 until numberOfFreeVariables) {
        for(index_2 <- 0 until numberOfVariables) {
          solutionVectors(index_1)(index_2) = new BigFraction(0)
        }
      }

      println("calculare solutii")
      var nr_2 : Int = 0
      for (variable <- 0 until numberOfVariables) {
        println("calcsol - variable: " + variable + " of " + numberOfVariables)
        if(bigfractionSystem(variable)(variable).isZero) {
          for(jindex_1 <- 0 until variable) {
            solutionVectors(nr_2)(jindex_1) = bigfractionSystem(jindex_1)(variable) * -1
          }
          solutionVectors(nr_2)(variable) = new BigFraction(1)
          nr_2 = nr_2 + 1
        }
      }


        /*
      var nr_2 : Int = 0
      for (variable <- 0 until numberOfVariables) {
        var sem: Boolean = false
        var index_1: Int = variable - nr_2 //the current index
        var index_2: Int = index_1 //the index to go trough matrix
        //find the row with the variable
        while ((sem == false) && (index_2 < bigfractionSystem.length)) {
          if (!(bigfractionSystem(index_2)(variable).isZero)) {
            sem = true
          } else {
            index_2 = index_2 + 1
          }
        }

        if(sem==false) {
          for(jindex_1 <- 0 until index_1) {
            solutionVectors(nr_2)(jindex_1) = bigfractionSystem(jindex_1)(variable) * -1
          }
          solutionVectors(nr_2)(variable) = new BigFraction(1)
          nr_2 = nr_2 + 1
        }
      }

         */


      println("Vectorii varaibilelor libere finale")
      for(index_1 <- 0 until numberOfFreeVariables) {
        for(jindex_1 <- 0 until numberOfVariables) {
          print(solutionVectors(index_1)(jindex_1).toString + " ")
        }
        println()
      }
    }
    //TODO: maybe return this vector
  }

  def displaySystem() = {
    for(index_1 <- 0 until numberOfVariables) {
      for(jindex_1 <- 0 until numberOfVariables) {
        print(bigfractionSystem(index_1)(jindex_1).toString + " ")
      }
      println()
    }
  }

  def gcd(a : BigInt, b : BigInt) : BigInt = {
    val zero : BigInt = new BigInteger("0")
    b match {
      case zero => a
      case x if x>a => gcd(x,a)
      case x => gcd(x, a%x)
    }
  }

}


// TODO : Merge pe INT

class IntHomogeneousSystem (val equationSystem :Array[Array[Int]]) {
  var bigfractionSystem : Array[Array[IntFraction]] = ofDim(equationSystem.length, equationSystem(0).length)
  val numberOfVariables : Int = equationSystem(0).length
  var numberOfFreeVariables : Int = 0

  def makeSystem() : Array[Array[IntFraction]] = {
    for(index_1 <- 0 until equationSystem.length) {
      for(index_2 <- 0 until equationSystem(index_1).length) {
        bigfractionSystem(index_1)(index_2) = new IntFraction(equationSystem(index_1)(index_2))
      }
    }

    //this.displaySystem()
    println("sistem încarcat")

    for (variable <- 0 until numberOfVariables) {
      println("first calc variable: " + variable + " of " + numberOfVariables)
      var sem: Boolean = false
      var index_1 : Int = variable - numberOfFreeVariables //the current index
      var index_2 : Int = index_1 //the index to go trough matrix
      //find the row with the variable
      while ( (sem == false) && (index_2 < bigfractionSystem.length)) {
        if( !(bigfractionSystem(index_2)(variable).isZero)) {
          sem = true
          if(index_2 != index_1) {
            //change the rows
            var aux_array : Array[IntFraction] = new Array[IntFraction](numberOfVariables)
            bigfractionSystem(index_1).copyToArray(aux_array)
            bigfractionSystem(index_2).copyToArray(bigfractionSystem(index_1))
            aux_array.copyToArray(bigfractionSystem(index_2))
          }
        } else {
          index_2 = index_2 + 1
        }
      }

      if(sem == true) {
        var jindex_1 : Int = variable
        //make the cell to have the value 1
        var to_multiply : IntFraction =  bigfractionSystem(index_1)(variable).inverse()
        //println(to_multiply.toString)
        for(jindex_1 <- variable until numberOfVariables) {
          bigfractionSystem(index_1)(jindex_1) = bigfractionSystem(index_1)(jindex_1) * to_multiply
        }


        //this.displaySystem()

        //substract the value from other equations
        for(index_2 <- 0 until equationSystem.length) {
          if(!bigfractionSystem(index_2)(variable).isZero && (index_2 != index_1)) {
            to_multiply = bigfractionSystem(index_2)(variable)
            //println("index: " + index_2 + " value: " + to_multiply.toString)
            for(jindex_1 <- variable until numberOfVariables) {
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_1)(jindex_1).toString)
              //println("jindex_1: " + jindex_1 + " value: " + (to_multiply * bigfractionSystem(index_1)(jindex_1)).toString)
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_2)(jindex_1).toString)
              bigfractionSystem(index_2)(jindex_1) = bigfractionSystem(index_2)(jindex_1) - (to_multiply * bigfractionSystem(index_1)(jindex_1))
              //println("jindex_1: " + jindex_1 + " value: " + bigfractionSystem(index_2)(jindex_1).toString)
            }
          }
        }

      } else {
        numberOfFreeVariables = numberOfFreeVariables + 1
      }

      //this.displaySystem()
    }

    println("Ecuați finale")
    //this.displaySystem()

    //VERY BAD
    if(numberOfFreeVariables == 0) {
      println("sistem cu soluție trivială")
      new Array[Array[IntFraction]](1)
    } else {
      //aranajare frumos a variabilelor
      println("sistem cu soluție non trivială")
      for (variable <- 0 until numberOfVariables) {
        println("rearanje - variable: " + variable + " of " + numberOfVariables)
        if ((bigfractionSystem(variable)(variable).isZero)) {
          //se da mai jos totul
          //coloana curenta devine nula
          var prev_array : Array[IntFraction] = new Array[IntFraction](numberOfVariables)
          var aux_array : Array[IntFraction] = new Array[IntFraction](numberOfVariables)
          var zero_array : Array[IntFraction] = new Array[IntFraction](numberOfVariables)
          for(index <- 0 until numberOfVariables) {
            zero_array(index) = new IntFraction()
          }
          bigfractionSystem(variable).copyToArray(prev_array)
          zero_array.copyToArray(bigfractionSystem(variable))
          //transpunem toate coaloanele mai jos
          for(index <- (variable + 1) until numberOfVariables) {
            bigfractionSystem(index).copyToArray(aux_array)
            prev_array.copyToArray( bigfractionSystem(index))
            aux_array.copyToArray(prev_array)
          }
        }

        println("Rearanajare varaibile")
        //this.displaySystem()
      }


      var solutionVectors : Array[Array[IntFraction]] = ofDim(numberOfFreeVariables, numberOfVariables)
      for(index_1 <- 0 until numberOfFreeVariables) {
        for(index_2 <- 0 until numberOfVariables) {
          solutionVectors(index_1)(index_2) = new IntFraction(0)
        }
      }

      println("calculare solutii")
      var nr_2 : Int = 0
      for (variable <- 0 until numberOfVariables) {
        println("calcsol - variable: " + variable + " of " + numberOfVariables)
        if(bigfractionSystem(variable)(variable).isZero) {
          for(jindex_1 <- 0 until variable) {
            solutionVectors(nr_2)(jindex_1) = bigfractionSystem(jindex_1)(variable) * -1
          }
          solutionVectors(nr_2)(variable) = new IntFraction(1)
          nr_2 = nr_2 + 1
        }
      }



      println("Vectorii varaibilelor libere finale")
      for(index_1 <- 0 until numberOfFreeVariables) {
        for(jindex_1 <- 0 until numberOfVariables) {
          print(solutionVectors(index_1)(jindex_1).toString + " ")
        }
        println()
      }

      return solutionVectors
    }
    //TODO: maybe return this vector
  }

  def displaySystem() = {
    for(index_1 <- 0 until numberOfVariables) {
      for(jindex_1 <- 0 until numberOfVariables) {
        print(bigfractionSystem(index_1)(jindex_1).toString + " ")
      }
      println()
    }
  }
}

@SerialVersionUID(125L)
class TestPosit (val size : Int) extends Serializable {
  var sign: Int = 0
  var special_number: Int = 0
  var regime: Int = 0
  var exponent: BigInt = 0
  var fraction: BigInt = 0
  var exponent_size: Int = 0
  var fraction_size: Int = 0
  var b_m: Int = 0


  def min(that : TestPosit) : TestPosit = {
    if(this < that)
      return this
    else
      return that
  }

  def max(that : TestPosit) : TestPosit = {
    if(this < that)
      return that
    else
      return this
  }

  def sgnj(that : TestPosit, op : Int) : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    my_posit.sign = op match {
      case 0 => that.sign //sgnj
      case 1 => 1 - that.sign //sgnjn
      case 2 => this.sign ^ that.sign //sgnjx
      case _ => this.sign
    }
    my_posit.special_number = this.special_number
    my_posit.regime = this.regime
    my_posit.exponent = this.exponent
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction = this.fraction
    my_posit.fraction_size = this.fraction_size
    my_posit.b_m = this.b_m

    return my_posit

  }

  def displayValue() : String = {
    return "s:" + this.sign.toString() + " sn:" + this.special_number.toString() + " k:" + this.regime.toString() + " e:" + this.exponent.toString() + " f:" + this.fraction.toString() + " es:" + this.exponent_size.toString() + " fs:" + this.fraction_size.toString() + " bm:" + this.b_m.toString()
  }

  def toDouble() : Double = {
    var return_value: Double = 0

    if(this.special_number == 1) {
      return 0
    }

    return_value =  scala.math.pow(scala.math.pow(2,scala.math.pow(2,this.exponent_size).toInt).toInt, this.regime).toDouble
    return_value =  return_value * scala.math.pow(2,this.exponent.toInt).toDouble

    return_value = return_value * (this.fraction.toDouble / (scala.math.pow(2,this.fraction_size).toDouble))

    if(this.sign == 1) {
      return_value = return_value * -1
    } else {
      return_value = return_value
    }
    return return_value
  }

  def toBigInt(is : Int) : BigInt = {
    var exponent : BigInt = 0
    var my_int : BigInt = 0
    var aux : BigInt = 0
    if( (this.special_number == 1) && (this.sign == 1)) {
      my_int = 1
      my_int = (my_int << (is - 1)) -1
      return my_int
    }
    if( (this.special_number == 1) && (this.sign == 0)) {
      return 0
    }
    if(this.regime < 0) {
      return 0
    }

    exponent =  (this.regime << this.exponent_size) + this.exponent
    //println("exponent=" + exponent.toString())
    //println("this.fraction=" + this.fraction.toString())
    if(exponent > (is-1)) {
      my_int = 1
      my_int = (my_int<<(is-1))-1
    } else {
      if(exponent > this.fraction_size) {
        aux = exponent - new BigInteger(this.fraction_size.toString())
        aux = this.fraction << aux.toInt
        my_int = aux
      } else {
        aux = new BigInteger(this.fraction_size.toString())
        aux =  aux - exponent
        aux = this.fraction >> aux.toInt
        my_int = aux
      }
    }
    /*
    println("aux=" + aux.toString())
    println("my_int=" + my_int.toString())
    println("~my_int=" + (~my_int).toString())
    println("~my_int+1=" + (~my_int+1).toString(2))
    aux = 1
    aux = (aux << (is+1)) - 1
    println("aux=" + aux.toString(2))
    println("(~my_int + 1) & aux=" + ((~my_int + 1) & aux).toString())
    println("this.sign=" + this.sign.toString())
    */
    if(this.sign == 1) {
      aux = 1
      aux = (aux << (is)) - 1
      my_int = (~my_int + 1) & aux
    }
    return my_int

  }

  def fromBigInt(a : BigInt, is : Int, es : Int) = {
    var sign : Int = 0
    var m : Int = 0
    var exponent : Int = 0
    var my_int : BigInt = 0
    var aux : BigInt = 0
    my_int = a
    if(my_int == 0 ) {
      this.special_number = 1
    } else {
      this.special_number = 0
    }
    sign = ((my_int >> (is-1))&1).toInt
    //println("sign=" + sign.toString())
    if(sign==1) {
      aux = 1
      aux = (aux << is) - 1
      my_int = (~my_int + 1) & aux
    }


    var countReg : Int = 0
    var sem: Int = 0
    for (index <- 0 to (is-1)) {
      aux = (my_int >> (is-1-index))
      //println("aux=" + aux.toString())
      aux =  aux & 1

      if(aux == 0 && sem == 0) {
        countReg = countReg + 1
      } else {
        sem = 1
      }
    }
    //println("countReg=" + countReg.toString())
    exponent = is - 1 - countReg
    //println("exponent=" + exponent.toString())
    //println("my_int=" + my_int.toString())
    this.exponent_size = es
    this.regime = exponent >> this.exponent_size
    this.exponent = exponent & ((1<<es)-1)
    this.fraction = my_int
    this.fraction_size = exponent
    if(sign==1) {
      this.sign = 1
    } else {
      this.sign = 0
    }

  }


  def sqrt() : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    var aux : BigInt = 0
    var sqrt_result_double : Double = 0

    if( this.special_number == 1 ) {
      my_posit = this
      return my_posit
    }
    if(this.sign == 1) {
      my_posit.sign = 1
      my_posit.special_number = 1
      return my_posit
    }
    my_posit.sign = 0
    my_posit.special_number = 0
    my_posit.regime = this.regime >> 1
    aux =  new BigInteger(this.regime.toString())
    aux = aux & 1
    aux = aux << this.exponent_size
    my_posit.exponent = (aux + this.exponent) >> 1
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction_size = ( this.fraction_size + (this.fraction_size & 1) ) >> 1
    //my_posit.fraction = sqrt(this.fraction << (this.exponent & 1 + this.fraction_size & 1))
    //TODO
    sqrt_result_double = scala.math.sqrt( ( this.fraction << ( (this.exponent & 1).toInt + (this.fraction_size & 1) ) ).toLong )
    my_posit.fraction = new BigInteger(sqrt_result_double.toLong.toString())
    if (sqrt_result_double != sqrt_result_double.toLong.toDouble)
      my_posit.b_m = 1
    else
      my_posit.b_m = 0
    return my_posit
  }


  def -() : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    my_posit = this

    if( this.special_number == 1 ) {
      return my_posit
    }
    my_posit.sign = 1 - my_posit.sign
    return my_posit
  }

  def /(that : TestPosit) : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)

    if( (this.special_number == 1) && (this.sign == 1)) {
      my_posit = this
      return my_posit
    }
    if( (that.special_number == 1) && (that.sign == 1)) {
      my_posit = that
      return my_posit
    }
    if( (that.special_number == 1) && (that.sign == 0)) {
      my_posit.sign = 1
      my_posit.special_number = 1
      return my_posit
    }
    if( (this.special_number == 1) && (this.sign == 0)) {
      my_posit = this
      return my_posit
    }

    my_posit.sign = this.sign ^ that.sign
    my_posit.special_number = 0
    my_posit.regime = this.regime - that.regime
    if(that.exponent > this.exponent) {
      my_posit.exponent = this.exponent + (1<<this.exponent_size) - that.exponent
      my_posit.regime = my_posit.regime - 1
    } else {
      my_posit.exponent = this.exponent - that.exponent
    }
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction_size = this.fraction_size + this.size - that.fraction_size
    my_posit.fraction = (this.fraction << this.size) /  that.fraction
    if(((this.fraction << this.size) %  that.fraction) != 0) {
      my_posit.b_m = 1
    } else {
      my_posit.b_m = 0
    }
    return my_posit
  }

  def *(that : TestPosit) : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)

    if( (this.special_number == 1) && (this.sign == 1)) {
      return this
    }
    if( (that.special_number == 1) && (that.sign == 1)) {
      return that
    }
    if( (this.special_number == 1) && (this.sign == 0)) {
      return this
    }
    if( (that.special_number == 1) && (that.sign == 0)) {
      return that
    }

    my_posit.sign = this.sign ^ that.sign
    my_posit.special_number = 0
    my_posit.regime = this.regime + that.regime
    my_posit.exponent = this.exponent + that.exponent
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction_size = this.fraction_size + that.fraction_size
    my_posit.fraction = this.fraction * that.fraction
    my_posit.b_m = 0
    return my_posit

  }

  def -(that : TestPosit) : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    var t : Int = 0
    if(this.special_number == 1) {
      if(this.sign == 1) {
        return this
      } else {
        my_posit = that
        if(that.special_number == 0) {
          my_posit.sign = 1 - my_posit.sign
        }
        return my_posit
      }
    }

    if(that.special_number == 1) {
      if(that.sign == 1) {
        return that
      } else {
        return this
      }
    }

    if(this.sign != that.sign) {
      my_posit = that
      my_posit.sign = 1 - that.sign
      return this + my_posit
    }
    if(that.abs() > this.abs()) {
      my_posit = that - this
      my_posit.sign = 1 - my_posit.sign
      return my_posit
    }


    my_posit.sign = this.sign
    my_posit.special_number = 0
    my_posit.regime = this.regime
    my_posit.exponent = this.exponent
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction_size = this.size * 2 - 4
    //TODO de modificat t in BigInt la fel pentru +
    t = (this.regime << this.exponent_size) + this.exponent.toInt - (that.regime << that.exponent_size) - that.exponent.toInt
    my_posit.fraction = (this.fraction << (my_posit.fraction_size - this.fraction_size)) - ((that.fraction << (my_posit.fraction_size - that.fraction_size) ) >> t )
    if(((that.fraction << (my_posit.fraction_size - that.fraction_size) ) & ((1<<t)-1) ) != 0)
      my_posit.b_m = 1
    else
      my_posit.b_m = 0

    return my_posit

  }

  def +(that : TestPosit) : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    var t : Int = 0

    if(this.special_number == 1) {
      if(this.sign == 1) {
        return this
      } else {
        return that
      }
    }

    if(that.special_number == 1) {
      if(that.sign == 1) {
        return that
      } else {
        return this
      }
    }

    if(this.sign != that.sign) {
      my_posit = that
      my_posit.sign = 1 - that.sign
      return this - my_posit
    }
    if(that.abs() > this.abs()) {
      return that + this
    }


    my_posit.sign = this.sign
    my_posit.special_number = 0
    my_posit.regime = this.regime
    my_posit.exponent = this.exponent
    my_posit.exponent_size = this.exponent_size
    my_posit.fraction_size = this.size * 2 - 4
    t = (this.regime << this.exponent_size) + this.exponent.toInt - (that.regime << that.exponent_size) - that.exponent.toInt
    my_posit.fraction = (this.fraction << (my_posit.fraction_size - this.fraction_size)) + ((that.fraction << (my_posit.fraction_size - that.fraction_size) ) >> t )
    if(((that.fraction << (my_posit.fraction_size - that.fraction_size) ) & ((1<<t)-1) ) != 0)
      my_posit.b_m = 1
    else
      my_posit.b_m = 0

    return my_posit



  }

  def ===(that : TestPosit) : Boolean = {
    var posit_1: TestPosit = new TestPosit(this.size)
    posit_1 = this
    var posit_2: TestPosit = new TestPosit(this.size)
    posit_2 = that
    var aux : Boolean = false
    if(posit_1.binaryEncode() == posit_2.binaryEncode()) {
      return true
    }
    return false
  }

  def ==(that : TestPosit) : Boolean = {
    var aux : Boolean = false
    if(this.sign == that.sign) {
      if(this.regime == that.regime) {
        if(this.exponent == that.exponent) {
          if(this.fraction_size == that.fraction_size) {
            if(this.fraction == that.fraction) {
              return true
            }
          } else {
            if(this.fraction_size > that.fraction_size) {
              if(this.fraction == (that.fraction<<(this.fraction_size-that.fraction_size)) ) {
                return true
              }
            } else {
              if( (this.fraction<<(that.fraction_size-this.fraction_size)) == that.fraction ) {
                return true
              }
            }
          }
        }
      }
    }
    return false
  }

  def >(that : TestPosit) : Boolean = {
    return !(this <= that)
  }

  def <=(that : TestPosit) : Boolean = {
    return (this < that) || (this == that)
  }


  def <(that : TestPosit) : Boolean = {
    var aux : Boolean = false
    if(this.sign > that.sign) {
      return true
    } else if(this.sign < that.sign) {
      return false
    } else {
      if(that.special_number == 1) {
        return false
      } else if(this.special_number == 1) {
        return true
      } else if(this.regime > that.regime) {
        aux = false
      } else if (this.regime < that.regime) {
        aux = true
      } else {
        if(this.exponent > that.exponent) {
          aux = false
        } else if (this.exponent < that.exponent) {
          aux = true
        } else {
          var f1 : BigInt = 0
          var f2 : BigInt = 0
          if(this.fraction_size == that.fraction_size) {
            f1 = this.fraction
            f2 = that.fraction
          } else {
            if(this.fraction_size > that.fraction_size) {
              f1 = this.fraction
              f2 = that.fraction << (this.fraction_size - that.fraction_size)
            } else {
              f1 = this.fraction << (that.fraction_size - this.fraction_size)
              f2 = that.fraction
            }
          }
          if(f1 > f2) {
            aux = false
          } else if (f1 < f2) {
            aux = true
          } else {
            return false
          }
        }
      }
      if(this.sign == 1) {
        return !aux
      } else {
        return aux
      }
    }
  }

  def abs() : TestPosit = {
    var my_posit: TestPosit = new TestPosit(this.size)
    if( (this.sign == 1) && (this.special_number==1)) {
      my_posit.sign = 1
      my_posit.special_number = 1
    } else {
      my_posit.sign = 0
      my_posit.special_number = this.special_number
      my_posit.regime = this.regime
      my_posit.exponent = this.exponent
      my_posit.fraction = this.fraction
      my_posit.exponent_size = this.exponent_size
      my_posit.fraction_size = this.fraction_size
      my_posit.b_m = this.b_m
    }
    return my_posit
  }

  def normalise() = {
    var fraction: BigInt = 0
    var exponent: BigInt = 0
    var regime: Int = 0
    var ps : Int = 0
    var es: Int = 0
    var fs: Int = 0
    var aux: BigInt = 0
    fraction = this.fraction
    exponent = this.exponent
    regime = this.regime
    ps = this.size
    es = this.exponent_size
    fs = this.fraction_size

    aux = 1
    aux = aux << (fs+1)
    while(fraction >= aux) {
      //old
      //fraction = fraction >> 1
      //endold
      //new
      fs = fs + 1
      aux = 1
      aux = aux << (fs+1)
      //endnew
      exponent = exponent + 1
    }
    if(fraction != 0) {
      aux = 1
      aux = aux << (fs)
      while(fraction < aux) {
        fraction = fraction << 1
        exponent = exponent - 1
      }
    } else {
      regime = -(ps*3)
    }

    while(exponent < 0) {
      exponent = exponent + (1<<es)
      regime = regime - 1

    }
    while(exponent >= (1<<es)) {
      exponent = exponent - (1<<es)
      regime = regime + 1
    }

    this.regime = regime
    this.exponent = exponent
    this.fraction = fraction
    this.fraction_size = fs
  }

  def binaryEncode() : BigInt = {
    var return_value: BigInt = 0
    var ps : Int = 0
    var fraction: BigInt = 0
    var fs: Int = 0
    var exponent: BigInt = 0
    var es: Int = 0
    var regime: Int = 0
    var rn: Int = 0
    var rs: Int = 0
    var regime_bits: BigInt = 0
    var ers: Int = 0
    var frs: Int = 0
    var b_nplus1: Boolean = false
    var b_m: Boolean = false
    var addOne: BigInt = 0
    var exponent_bits: BigInt = 0
    var fraction_bits: BigInt = 0
    var aux : BigInt = 0
    var aux_2 : BigInt = 0
    ps = this.size


    if(this.special_number == 1) {
      if(this.sign == 1) {
        aux = 1
        aux = aux << (ps-1)
        return_value = aux
      } else {
        return_value = 0
      }
      return return_value
    }

    this.normalise()

    fraction = this.fraction
    exponent = this.exponent
    regime = this.regime
    es = this.exponent_size
    fs = this.fraction_size

    if(fraction == 0) {
      return_value = 0
      return return_value
    } else if(regime >= (ps-2)) {
      aux = 1
      aux = aux << (ps-1)
      aux - aux - 1
      return_value = aux
    } else if(regime < -(ps-2)) {
      return_value = 1
    } else {
      if(regime >= 0) {
        rn = regime + 1
        rs = rn + 1
        aux = 1
        aux = aux << rn
        aux = aux - 1
        aux = aux << 1
        regime_bits = aux
      } else {
        rn = -regime
        rs = rn + 1
        regime_bits = 1
      }

      ers = scala.math.max(0, scala.math.min(es, ps - rs - 1))
      frs = scala.math.max(0, ps-rs-es-1)
      b_nplus1 = false
      b_m = (this.b_m != 0)
      /*
      println("ers=" + ers.toString())
      println("frs=" + frs.toString())
      println("rs=" + rs.toString())
      println("rn=" + rn.toString())
      println("regime=" + regime.toString())
      println("exponent=" + exponent.toString())
      println("fraction=" + fraction.toString())
      println("regime_bits=" + regime_bits.toString())
      */

      if(es-ers==0) {
        exponent_bits = exponent
        if(frs>=fs) {
          b_nplus1 = false
          fraction_bits = (fraction & ((1<<fs)-1)) << (frs-fs)
          b_m = b_m
        } else {
          //println("fraction_bits_aux=" + (fraction & ((1<<(fs-frs-1))-1)).toString())
          //println("fraction_bits_aux2=" + fraction.toString(2))
          aux = 1
          aux = aux << (fs-frs-1)
          b_nplus1 = ((fraction & aux) != 0) || b_nplus1
          if(frs <= (fs-2)) {
            aux = 1
            aux = aux << (fs-frs-1)
            aux = aux - 1
            b_m = ((fraction & aux) != 0) || b_m
          } else {
            b_m = b_m
          }
          if(frs == 0 ) {
            fraction_bits = 0
          } else {
            aux = 1
            aux = aux << (fs)
            aux = aux - 1
            fraction_bits = (fraction & aux) >> (fs-frs)
          }
        }
      } else {
        if(ers == 0) {
          exponent_bits = 0
        } else {
          exponent_bits = (exponent & ((1<<es)-1)) >> (es-ers)
        }
        b_nplus1 = ((exponent & (1<<(es-ers-1))) != 0) || b_nplus1
        aux = 1
        aux = aux << (fs)
        aux = aux - 1
        b_m = ((fraction & aux)!=0) || b_m
        if(es-ers > 1) {
          b_m = ((exponent & ((1<<(es-ers-1))-1)) != 0) || b_m
        }
        fraction_bits = 0
      }
      //println("exponent_bits=" + exponent_bits.toString())
      //println("fraction_bits=" + fraction_bits.toString())
      return_value = (regime_bits << (ers+frs)) | (exponent_bits << frs) | fraction_bits
      if(b_nplus1 && (b_m || (!b_m && ( (return_value & 1) != 0 )))) {
        addOne = 1
      } else {
        addOne = 0
      }
      //println("b_nplus1=" + b_nplus1.toString())
      //println("b_m=" + b_m.toString())
      //println("addOne=" + addOne.toString())
      return_value = return_value + addOne
    }
    if(this.sign == 1) {
      aux = 1
      aux = aux << (ps-1)
      aux_2 = aux
      aux = aux - 1

      return_value = ((~(return_value)+1) & aux) | aux_2
    }
    return return_value
  }


  def decodeBinary(a : BigInt, es : Int)  = {
    var ps : Int = 0
    var bp: BigInt = 0
    var aux: BigInt = 0
    var ri: BigInt = 0
    var rn : Int = 0
    var sem: Int = 0
    var k: Int = 0
    var rs: Int = 0
    var ers: Int = 0
    var frs: Int = 0
    var e: BigInt = 0
    var f: BigInt = 0

    ps = this.size
    if((a & ((1<<(ps-1))-1)) == 0) {
      this.special_number = 1
    } else {
      this.special_number = 0
    }

    this.sign = ((a >> (ps-1)) & 1).intValue

    if(this.sign > 0) {
      bp = (~a & ((1<<(ps-1))-1)) + 1
    } else {
      bp = a & ((1<<(ps-1))-1)
    }

    ri = (bp >> (ps -2)) & 1
    rn = 1
    for (index <- 0 to (ps-3)) {
      aux = (bp >> (ps-3-index)) & 1
      if(aux == ri && sem == 0) {
        rn = rn + 1
      } else {
        sem = 1
      }
    }
    if(ri == 0) {
      k = -rn
    } else {
      k = rn - 1
    }

    rs = rn + 1
    ers = scala.math.max(0, scala.math.min(es, ps-rs-1))
    frs = scala.math.max(0, ps-rs-es-1)
    if(ers == 0) {
      e = 0
    } else {
      e = ((bp & ((1<<(ps-1-rs))-1)) >> (ps-rs-ers-1)) << (es-ers)
    }

    if(frs == 0) {
      f = 0
    } else {
      f = (bp & ((1<<(ps-rs-es))-1))
    }

    f = f | (1<<frs)

    this.regime = k
    this.exponent = e
    this.exponent_size = es
    this.fraction = f
    this.fraction_size = frs
    this.b_m = 0
  }

  def make_multiply_maxtrix(): Int = {
    val posit_values : Int = scala.math.pow(2, this.size).toInt
    val max_exponent_size: Int = 0
    var value_3: Int = 0
    var value_1: BigInt = 0
    var value_2: BigInt = 0
    var posit_1: TestPosit = new TestPosit(this.size)
    var posit_2: TestPosit = new TestPosit(this.size)
    var posit_3: TestPosit = new TestPosit(this.size)



    /* TODO : FIRST TIME
    val t0 = System.nanoTime()

    var multiply_matrix: Array[Array[TestPosit]] = ofDim[TestPosit](posit_values, posit_values)
    for (value_1 <- 1 until  posit_values/2) {
      posit_1.decodeBinary(value_1, max_exponent_size)
      for (value_2 <- 1 until  posit_values/2) {
        posit_2.decodeBinary(value_2, max_exponent_size)
        posit_3 = posit_1 * posit_2
        posit_3.normalise()
        multiply_matrix (value_1) (value_2) = posit_3
      }
    }

    val fos_4 = new FileOutputStream(this.size.toString + "_" + this.exponent_size.toString  + "multiply_matrix.ser")
    val oos_4 = new ObjectOutputStream(fos_4)
    oos_4.writeObject(multiply_matrix)
    oos_4.close()
    val t1 = System.nanoTime()
    println("Multiplicare Elapsed time: " + (t1 - t0)/1000000000 + "s")
  */
    /* TODO: after first process */
    val fis_4 = new FileInputStream(this.size.toString + "_" + this.exponent_size.toString  + "multiply_matrix.ser")
    val ois_4 = new ObjectInputStream(fis_4)
    var multiply_matrix : Array[Array[TestPosit]] = ois_4.readObject.asInstanceOf[Array[Array[TestPosit]]]
    /**/


    //return 0
    /*
    for (value_1 <- 1 until  posit_values/2) {
      for (value_2 <- 1 until  posit_values/2) {
        print(" " + multiply_matrix(value_1)(value_2).toDouble());
      }
      println();
    }
    */

  /* TODO : FIRST TIME
    val t2 = System.nanoTime()
    var list_products : List[TestPosit] = List()
    for (value_1 <- 1 until  posit_values/2) {
      for (value_2 <- 1 until  posit_values/2) {
        if(!list_products.exists((x : TestPosit) => x==multiply_matrix(value_1)(value_2))) {
          list_products = multiply_matrix(value_1)(value_2)::list_products
        }
      }
    }

    list_products =  list_products.sortWith((x : TestPosit, y : TestPosit) => x<y)



   // println( "list : " + list_products.map((x : TestPosit) => x.toDouble()))
   // println( "list : " + list_products.map((x : TestPosit) => x.displayValue() + "\n"))

    var posit_sort_multiply_values : Array[TestPosit] = new Array[TestPosit](list_products.length)
    list_products.copyToArray(posit_sort_multiply_values)



    val fos_3 = new FileOutputStream(this.size.toString + "_" + this.exponent_size.toString + "posit_rank_values.ser")
    val oos_3 = new ObjectOutputStream(fos_3)
    oos_3.writeObject(posit_sort_multiply_values)
    oos_3.close()


    val t3 = System.nanoTime()
    println("Sortare Elapsed time: " + (t3 - t2)/1000000000 + "s")
    */

    /* TODO: after first process */
    val fis_3 = new FileInputStream(this.size.toString + "_" + this.exponent_size.toString + "posit_rank_values.ser")
    val ois_3 = new ObjectInputStream(fis_3)
    var posit_sort_multiply_values : Array[TestPosit] = ois_3.readObject.asInstanceOf[Array[TestPosit]]
    /**/


    /* TODO: after first process */
    val fis_2 = new FileInputStream(this.size.toString + "_" + this.exponent_size.toString + "rank_matrix.ser")
    val ois_2 = new ObjectInputStream(fis_2)
    var rank_multiply_matrix: Array[Array[Int]] = ois_2.readObject.asInstanceOf[Array[Array[Int]]]
    /**/

    val t4 = System.nanoTime()
    var rank : Int = 0

    /* TODO: FRIST TIME
    var rank_multiply_matrix: Array[Array[Int]] = ofDim[Int](posit_values, posit_values)
    for (value_1 <- 1 until  posit_values/2) {
      for (value_2 <- 1 until  posit_values/2) {
        for (rank <- 0 until (posit_sort_multiply_values.length)) {
          if (multiply_matrix(value_1)(value_2) == posit_sort_multiply_values(rank)) {
            rank_multiply_matrix(value_1)(value_2) = rank
          }
        }
      }
    }

    val fos_2 = new FileOutputStream(this.size.toString + "_" + this.exponent_size.toString + "rank_matrix.ser")
    val oos_2 = new ObjectOutputStream(fos_2)
    oos_2.writeObject(rank_multiply_matrix)
    oos_2.close()

    val t5 = System.nanoTime()
    println("atrice de rankuri Elapsed time: " + (t5 - t4)/1000000000 + "s")
    */


    /*
    Multiplicare Elapsed time: 2s
    Sortare Elapsed time: 28s
    atrice de rankuri Elapsed time: 64s
     */
    val t6 = System.nanoTime()
    /* TODO: ONLY First time */
    var rankToIntVector : Array[Int] = new Array[Int](posit_sort_multiply_values.length)
    for (rank <- 0 until (rankToIntVector.length)) {
      rankToIntVector(rank) = -1
    }
    rankToIntVector(0) = 0
    /**/
    //rankToIntVector(1) = 1

    /*
    for (value_1 <- 1 until  posit_values/2) {
      for (value_2 <- 1 until  posit_values/2) {
        print(" " + rank_multiply_matrix(value_1)(value_2));
      }
      println();
    }
     */
    var nr_variables : Int = posit_sort_multiply_values.length - 1
    var nr_equations : Int = ((posit_values/2) * (posit_values/2+1))
    println("variabile:", nr_variables.toString)
    println("ecuatii:", nr_equations.toString)

    var rankSystem : Array[Array[Int]] = ofDim(nr_equations, nr_variables)

    for (value_1 <- 0 until rankSystem.length ) {
      for (value_2 <- 0 until nr_variables) {
        rankSystem(value_1)(value_2) = 0
      }
    }



    /* TODO: Only first time */
    var index : Int = 0
    var rank_1 : Int = 0
    var rank_2 : Int = 0
    var rank_3 : Int = 0
    for (value_1 <- 0 until posit_values/2-2) {
      for (value_2 <- 0 until value_1 + 1) {
        rank_1 = rank_multiply_matrix(value_1+2)(1) - 1
        rank_2 = rank_multiply_matrix(value_2+2)(1) - 1
        rank_3 = rank_multiply_matrix(value_1+2)(value_2+2) - 1
        rankSystem(index)(rank_3) = rankSystem(index)(rank_3) - 1
        rankSystem(index)(rank_1) = rankSystem(index)(rank_1) + 1
        rankSystem(index)(rank_2) = rankSystem(index)(rank_2) + 1
        index = index + 1
      }
    }



    var mySystem : IntHomogeneousSystem = new IntHomogeneousSystem(rankSystem)
     /**/
    var solutionVectors : Array[Array[IntFraction]] = mySystem.makeSystem()

    val fos_1 = new FileOutputStream(this.size.toString + "_" + this.exponent_size.toString + "solution_vectors.ser")
    val oos_1 = new ObjectOutputStream(fos_1)
    oos_1.writeObject(solutionVectors)
    oos_1.close()
    val t7 = System.nanoTime()
    println("solutia sistemului de ecuatii pentru rankuri fara constrangeri Elapsed time: " + (t7 - t6)/1000000000 + "s")



    /* TODO: after first process
    val fis_1 = new FileInputStream(this.size.toString + "_" + this.exponent_size.toString + "solution_vectors.ser")
    val ois_1 = new ObjectInputStream(fis_1)
    var solutionVectors : Array[Array[IntFraction]] = ois_1.readObject.asInstanceOf[Array[Array[IntFraction]]]
    */


    val t8 = System.nanoTime()
    /* TODO: ONLY FIRST TIME */
    var A_matrix : Array[Array[IntFraction]] = ofDim(solutionVectors.length, solutionVectors(0).length-1)
    var b_vector : Array[IntFraction] = ofDim(solutionVectors.length)
    var P_vector : Array[IntFraction] =  ofDim(solutionVectors(0).length-1)

    for ( index_1 <- 0 until  A_matrix.length) {
      for ( jindex_1 <- 0 until A_matrix(index_1).length ) {
        A_matrix(index_1)(jindex_1) = solutionVectors(index_1)(jindex_1+1) - solutionVectors(index_1)(jindex_1)
      }
    }

    for ( index_1 <- 0 until b_vector.length) {
      b_vector(index_1) =  solutionVectors(index_1)(0) + 0
    }

    for ( index_1 <- 0 until P_vector.length) {
      P_vector(index_1) = new IntFraction(1)
    }
    /* */

    /* EXAgerata varainta
    for ( index_1 <- 0 until solutionVectors(0).length-1) {
      for ( jindex_1 <- 0 until A_matrix(index_1).length ) {
        A_matrix(index_1)(jindex_1) = solutionVectors(jindex_1)(index_1) - solutionVectors(jindex_1)(index_1+1)
      }
    }

    for ( index_1 <- solutionVectors(0).length-1 until A_matrix.length) {
      for ( jindex_1 <- 0 until A_matrix(index_1).length ) {
        A_matrix(index_1)(jindex_1) = solutionVectors(jindex_1)(index_1-(solutionVectors(0).length-1)) * -1
      }
    }


    for ( index_1 <- 0 until solutionVectors(0).length-1) {
      b_vector(index_1) = new IntFraction(-1)
    }

    for ( index_1 <- solutionVectors(0).length-1 until b_vector.length) {
      b_vector(index_1) = new IntFraction(-(index_1-(solutionVectors(0).length-1)+1))
    }


    for ( index_1 <- 0 until P_vector.length) {
      P_vector(index_1) = solutionVectors(index_1)(0) + 0
    }
    */



    //val SimplexSystem : min_ge_SimpleMethod = new min_ge_SimpleMethod(A_matrix, b_vector, P_vector)


    /* TODO: after the first process
    val fis_5 = new FileInputStream(this.size.toString + "_" + this.exponent_size.toString + "rankToIntVector.ser")
    val ois_5 = new ObjectInputStream(fis_5)
    var rankToIntVector : Array[Int] = ois_5.readObject.asInstanceOf[Array[Int]]
    println("rankuri finale")
    for (rank <- 0 until (rankToIntVector.length)) {
      print(rankToIntVector(rank) + " ")
    }
     */


    /* TODO: Only first time */
    var sem : Boolean = false
    var S_vector_aux : Array[IntFraction] =  ofDim(A_matrix.length)
    for ( index_1 <- 0 until  A_matrix.length) {
      S_vector_aux(index_1) = new IntFraction(0)
    }

    var A_matrix_aux : Array[Array[IntFraction]] = ofDim(A_matrix.length, A_matrix(0).length+1)
    for ( index_1 <- 0 until  A_matrix.length) {
      for ( jindex_1 <- 0 until A_matrix(index_1).length ) {
        A_matrix_aux(index_1)(jindex_1) = A_matrix(index_1)(jindex_1).toBigFraction().toIntFraction()
      }
    }

    for ( index_1 <- 0 until  A_matrix.length) {
      A_matrix_aux(index_1)(A_matrix(0).length) = b_vector(index_1)
    }
    var P_vector_aux : Array[IntFraction] =  ofDim(P_vector.length+1)
    for ( index_1 <- 0 until  P_vector.length) {
      P_vector_aux(index_1) = P_vector(index_1).toBigFraction().toIntFraction()
    }
    P_vector_aux(P_vector.length) = new IntFraction(0)
    // P_vector.copyToArray(P_vector_aux)

    while(sem == false) {
      var sem_2 : Boolean = false
      var sem_3 : Boolean = false
      var sem_4 : Boolean = false
      val SimplexSystem : Big_SimplexMethod = new Big_SimplexMethod(A_matrix_aux, b_vector, P_vector_aux, S_vector_aux)
      var answerRow : Array[BigFraction] = SimplexSystem.makeSystem()
      var startIndex : Int = answerRow.length - 2 - b_vector.length
      var index_1 : Int = startIndex
      var firstIndex : Int = -1
      var jindex_1 : Int = 0

      if(!answerRow(answerRow.length-1).isInteger()) {
        sem_4 = true
      }

      for (index_1 <- startIndex until answerRow.length - 2) {
        if (!answerRow(index_1).isInteger() && (sem_3==false)) {
          sem_3 = true
          firstIndex = index_1
        }
      }

      while(sem_2==false && (jindex_1 < A_matrix(0).length)) {
        var variable : IntFraction = new IntFraction(0)
        for (index_1 <- startIndex until answerRow.length - 2) {
          variable = variable + (answerRow(index_1).toIntFraction() * solutionVectors(index_1-startIndex)(jindex_1))
        }
        if(!variable.isInteger()) {
          sem_2 = true
        } else {
          jindex_1 = jindex_1 + 1
        }
      }

      if(sem_3) {
        println("sem_3")
        println("Răspunsuri")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(answerRow(startIndex+index_1).toString() + " ")
        }

        println("Before")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(S_vector_aux(index_1).toString() + " ")
        }

        index_1 = firstIndex - startIndex
        if(S_vector_aux(index_1) < answerRow(startIndex+index_1).toIntFraction()) {
          S_vector_aux(index_1) = answerRow(startIndex+index_1).toIntFraction().toNextIntegerFraction()
        } else {
          S_vector_aux(index_1) = S_vector_aux(index_1) + 1
        }

        /* OLDWAY maybe better only one change
        for ( index_1 <- 0 until S_vector_aux.length) {
          if(S_vector_aux(index_1) < answerRow(startIndex+index_1).toIntFraction()) {
            S_vector_aux2(index_1) = answerRow(startIndex+index_1).toIntFraction().toNextIntegerFraction()
          } else {
            S_vector_aux2(index_1) = S_vector_aux(index_1).toBigFraction().toIntFraction().toNextIntegerFraction()
          }
        }
         */
        println("After")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(S_vector_aux(index_1).toString() + " ")
        }
      } else {
        if(sem_4) {

          println("sem_4")
          var currentResult : IntFraction = answerRow(answerRow.length-1).toIntFraction()

          println("Before " + currentResult.toString())
          println("Răspunsuri")
          for ( index_1 <- 0 until S_vector_aux.length) {
            print(answerRow(startIndex+index_1).toString() + " ")
          }
          println("Before " + P_vector_aux(P_vector.length).toString())
          if(currentResult.isInteger()) {
            P_vector_aux(P_vector.length) = currentResult + 1
          } else {
            P_vector_aux(P_vector.length) = currentResult.toNextIntegerFraction()
          }
          println("After " + P_vector_aux(P_vector.length).toString())
        } else {
          if(sem_2) {
            println("sem 2")
            var currentResult : IntFraction = answerRow(answerRow.length-1).toIntFraction()

            println("Before " + currentResult.toString())
            println("Răspunsuri")
            for ( index_1 <- 0 until S_vector_aux.length) {
              print(answerRow(startIndex+index_1).toString() + " ")
            }
            println("Before " + P_vector_aux(P_vector.length).toString())
            if(currentResult.isInteger()) {
              P_vector_aux(P_vector.length) = currentResult + 1
            } else {
              P_vector_aux(P_vector.length) = currentResult.toNextIntegerFraction()
            }
            println("After " + P_vector_aux(P_vector.length).toString())
          } else {
            sem = true
            println("răspuns final")
            println("Răspunsuri")
            for ( index_1 <- 0 until S_vector_aux.length) {
              answerRow(startIndex+index_1) = answerRow(startIndex+index_1) + S_vector_aux(index_1).toBigFraction()
              print(answerRow(startIndex+index_1).toString() + " ")
            }

            for (rank <- 1 until (rankToIntVector.length)) {
              var variable : IntFraction = new IntFraction(0)
              for (index_1 <- startIndex until answerRow.length - 2) {
                variable = variable + (answerRow(index_1).toIntFraction() * solutionVectors(index_1-startIndex)(rank-1))
              }
              if(variable.isInteger()) {
                rankToIntVector(rank) = variable.numerator
              } else {
                println("ce ai făcut șefule")
              }
            }

            println("rankuri finale")
            for (rank <- 0 until (rankToIntVector.length)) {
              print(rankToIntVector(rank) + " ")
            }

            val fos_5 = new FileOutputStream(this.size.toString + "_" + this.exponent_size.toString + "rankToIntVector.ser")
            val oos_5 = new ObjectOutputStream(fos_5)
            oos_5.writeObject(rankToIntVector)
            oos_5.close()

          }
        }

      }
    }
    /**/
    val t9 = System.nanoTime()
    println("rezolvarea sistemul ILP Elapsed time: " + (t9 - t8)/1000000000 + "s")

    /* TODO : old way
    var sem : Boolean = false
    var S_vector_aux : Array[IntFraction] =  ofDim(A_matrix.length)
    for ( index_1 <- 0 until  A_matrix.length) {
      S_vector_aux(index_1) = new IntFraction(0)
    }
   // P_vector.copyToArray(P_vector_aux)

    while(sem == false) {
      var sem_2 : Boolean = false
      val SimplexSystem : Big_SimplexMethod = new Big_SimplexMethod(A_matrix, b_vector, P_vector, S_vector_aux)
      var answerRow : Array[BigFraction] = SimplexSystem.makeSystem()
      var startIndex : Int = answerRow.length - 2 - b_vector.length
      var index_1 : Int = startIndex

      for (index_1 <- startIndex until answerRow.length - 2) {
        if (!answerRow(index_1).isInteger()) {
          sem_2 = true
        }
      }

      if(sem_2) {
        var S_vector_aux2 : Array[IntFraction] =  ofDim(S_vector_aux.length)
        println("Răspunsuri")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(answerRow(startIndex+index_1).toString() + " ")
        }

        println("Before")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(S_vector_aux(index_1).toString() + " ")
        }
        for ( index_1 <- 0 until S_vector_aux.length) {
          if(S_vector_aux(index_1) < answerRow(startIndex+index_1).toIntFraction()) {
            S_vector_aux2(index_1) = answerRow(startIndex+index_1).toIntFraction().toNextIntegerFraction()
          } else {
            S_vector_aux2(index_1) = S_vector_aux(index_1).toBigFraction().toIntFraction().toNextIntegerFraction()
          }
        }
        S_vector_aux = S_vector_aux2
        println("After")
        for ( index_1 <- 0 until S_vector_aux.length) {
          print(S_vector_aux(index_1).toString() + " ")
        }
      } else {
        sem = true
        println("răspuns final")
      }

    }

     */
    //TODO : CONTINUUUU

    val t10 = System.nanoTime()
    var positToRank : Array[Int] = ofDim(posit_values/2)
    var positToInt : Array[Int] = ofDim(posit_values/2)

    for(value_1 <- 1 until positToRank.length) {
      positToRank(value_1) = rank_multiply_matrix(value_1)(1)
    }

    for(value_1 <- 1 until positToRank.length) {
      positToInt(value_1) = rankToIntVector(positToRank(value_1))
    }

    println()
    println("Asta e cel mai mare numar: " + positToInt(positToRank.length-1))

    var rankToPosit : Array[TestPosit] = ofDim(posit_sort_multiply_values.length)
    posit_sort_multiply_values.copyToArray(rankToPosit)


    for (value_1 <- 1 until  posit_values/2) {
      for (value_2 <- 1 until posit_values/2) {
        var value_3 : Int = positToInt(value_1) + positToInt(value_2)
        var rank: Int = 0
        for(index_1 <- 0 until rankToIntVector.length) {
          if(value_3 == (rankToIntVector(index_1))) {
            rank = index_1
          }
        }
        if( ! (rankToPosit(rank) == multiply_matrix(value_1)(value_2)) ) {
          println("AM DAT DE NAIBA PE UNDEVA")
        }
      }
    }

    //this.FindRank(rankToIntVector, rank_multiply_matrix, posit_values/2)

    val t11 = System.nanoTime()
    println("verificarea sistemului Elapsed time: " + (t11 - t10)/1000000000 + "s")
    return value_3
  }

  def generateRank2(rankToIntVector : Array[Int],
                   rankMatrix: Array[Array[Int]],
                   vec_size : Int) : (Boolean, Array[Int]) = {
    var value_1, value_2, rank : Int = 0
    var return_value : (Boolean, Array[Int]) = (false, rankToIntVector)
    var temp_vector : Array[Int] = new Array[Int](rankToIntVector.length)



    // step 1 calculate ranks from toher ranks
    for (value_1 <- 2 until vec_size) {
      for (value_2 <- 2 until value_1+1) {
        if( (rankToIntVector(rankMatrix(value_1)(1)) *
          rankToIntVector(rankMatrix(value_2)(1))*
          rankToIntVector(rankMatrix(value_1)(value_2))
          ) < 0 ) {
          if( !( (rankToIntVector(rankMatrix(value_1)(1)) == -1) &&
            (rankToIntVector(rankMatrix(value_2)(1)) == -1) )) {
            if (rankToIntVector(rankMatrix(value_1)(1)) == -1) {
              rankToIntVector(rankMatrix(value_1)(1)) = rankToIntVector(rankMatrix(value_1)(value_2)) - rankToIntVector(rankMatrix(value_2)(1))
            } else if (rankToIntVector(rankMatrix(value_2)(1)) == -1) {
              rankToIntVector(rankMatrix(value_2)(1)) = rankToIntVector(rankMatrix(value_1)(value_2)) - rankToIntVector(rankMatrix(value_1)(1))
            } else {
              rankToIntVector(rankMatrix(value_1)(value_2)) = rankToIntVector(rankMatrix(value_1)(1)) + rankToIntVector(rankMatrix(value_2)(1))
            }
          }
        }
      }
    }

    // print after step 1
    for (rank <- 1 until  rankToIntVector.length) {
        print(" " + rank + " : " + rankToIntVector(rank));
    }
    println();

    // step 2 check validity and calculate de product for int values
    var product : Int = 1
    var lastIndex : Int = 1
    var firstIndex : Int = -1
    var lastValue : Int = rankToIntVector(1)
    var temp : Int = 1
    for (rank <- 2 until  rankToIntVector.length) {
      if(rankToIntVector(rank) != -1) {
        // validate the raising monotony
        if(lastValue >= rankToIntVector(rank)) {
          return (false, rankToIntVector)
        } else {
          temp = rank-lastIndex
          /*
          //if(((rankToIntVector(rank)- lastValue) % temp) != 0) {
          if(((rankToIntVector(rank)- lastValue) < temp)) {
            if(product % temp != 0) {
              //println("index: ", temp, " producr: ", product, " gcd", gcd(temp, product))
              product = (product * temp) / gcd(temp, product)
            }
          }

        */

          /*
         if(temp > product ) {
           product = temp
         }
         */
          lastIndex = rank
          lastValue = rankToIntVector(rank)
        }
      } else {
        if(firstIndex == -1) {
          firstIndex = rank
        }
      }
    }
    println("The product: ", product);

    //step 3 multiply the rank
    var the_gcd : Int = rankToIntVector(1)
    /*
    for (rank <- 2 until  rankToIntVector.length) {
      if(rankToIntVector(rank) != -1) {
        the_gcd = gcd(the_gcd, rankToIntVector(rank))
      }
    }
     */
    //the_gcd = gcd(the_gcd, product)
    //product = product / the_gcd
    /*

    for (rank <- 1 until  rankToIntVector.length) {
      if(rankToIntVector(rank) != -1) {
        rankToIntVector(rank) = (rankToIntVector(rank) / the_gcd) * product
      }
    }

     */

    //print after step 3
    for (rank <- 1 until  rankToIntVector.length) {
      print(" " + rank + " : " + rankToIntVector(rank));
    }
    println();

    //step 4 generate for first index new values if any
    if(firstIndex == -1) {
      the_gcd = rankToIntVector(1)
      for (rank <- 2 until  rankToIntVector.length) {
        the_gcd = gcd(the_gcd, rankToIntVector(rank))
      }
      println("The gcd: ", the_gcd);
      for (rank <- 1 until  rankToIntVector.length) {
        rankToIntVector(rank) = rankToIntVector(rank) / the_gcd
      }
      //print final
      for (rank <- 1 until  rankToIntVector.length) {
        print(" " + rank + " : " + rankToIntVector(rank));
      }
      println();
      return (true, rankToIntVector)
    } else {
      var nextValue : Int = 0
      var sem : Int = 0
      for (rank <- firstIndex+1 until  rankToIntVector.length) {
        if((rankToIntVector(rank) != -1) && (sem == 0) ) {
          nextValue = rankToIntVector(rank) - (rank - firstIndex) + 1
          sem = 1
        }
      }
      if(nextValue == 0) {
        return (true, rankToIntVector)
      }
      //println("next value: ", nextValue)
      //println("firstindex: ", firstIndex)
      //return (true, rankToIntVector)
      for (value_1 <- (rankToIntVector(firstIndex-1)+1) until nextValue) {
        rankToIntVector.copyToArray(temp_vector)
        temp_vector(firstIndex) = value_1

        /*
        the_gcd = temp_vector(1)
        for (rank <- 2 until  temp_vector.length) {
          if(temp_vector(rank) != -1) {
            the_gcd = gcd(the_gcd, temp_vector(rank))
          }
        }
        for (rank <- 1 until  temp_vector.length) {
          temp_vector(rank) = temp_vector(rank) / the_gcd
        }
        */


        return_value = this.generateRank(temp_vector, rankMatrix, vec_size)
        if(return_value._1 == true) {
          return return_value
        }
      }
      return  return_value
    }

  }

  def FindRank(rankToIntVector : Array[Int],
               rankMatrix: Array[Array[Int]],
               vec_size : Int) : (Boolean, Array[Int]) = {

    var return_value : (Boolean, Array[Int]) = (false, rankToIntVector)
    var temp_vector : Array[Int] = new Array[Int](rankToIntVector.length)
    var product, rank : Int = 0
    rankToIntVector.copyToArray(temp_vector)
    var sem : Int = 0;

    ///555 -  s-a validat

    //schimbat in 102 pentru 6 din valoarea 1
    for (product <- 102 until scala.math.pow(2, 2*this.size-1).toInt) {
      if(sem == 0) {
        rankToIntVector.copyToArray(temp_vector)
        temp_vector(1) = product
        return_value = generateRank(temp_vector, rankMatrix, vec_size)
        if(return_value._1 == true) {
          println("Soluția")
          return_value._2.copyToArray(rankToIntVector)
          for (rank <- 1 until  rankToIntVector.length) {
            print(" " + rank + " : " + rankToIntVector(rank));
          }
          println();
          sem = 1
        }
      }
    }
    return return_value

  }


  def generateRank(rankToIntVector : Array[Int],
                    rankMatrix: Array[Array[Int]],
                    vec_size : Int) : (Boolean, Array[Int]) = {
    var value_1, value_2, rank : Int = 0
    var return_value : (Boolean, Array[Int]) = (false, rankToIntVector)
    var temp_vector : Array[Int] = new Array[Int](rankToIntVector.length)


    // step 1 calculate ranks from toher ranks
    for (value_1 <- 2 until vec_size) {
      for (value_2 <- 2 until value_1+1) {
        if( (rankToIntVector(rankMatrix(value_1)(1)) *
          rankToIntVector(rankMatrix(value_2)(1))*
          rankToIntVector(rankMatrix(value_1)(value_2))
          ) < 0 ) {
          if( !( (rankToIntVector(rankMatrix(value_1)(1)) == -1) &&
            (rankToIntVector(rankMatrix(value_2)(1)) == -1) )) {
            if (rankToIntVector(rankMatrix(value_1)(1)) == -1) {
              rankToIntVector(rankMatrix(value_1)(1)) = rankToIntVector(rankMatrix(value_1)(value_2)) - rankToIntVector(rankMatrix(value_2)(1))
            } else if (rankToIntVector(rankMatrix(value_2)(1)) == -1) {
              rankToIntVector(rankMatrix(value_2)(1)) = rankToIntVector(rankMatrix(value_1)(value_2)) - rankToIntVector(rankMatrix(value_1)(1))
            } else {
              rankToIntVector(rankMatrix(value_1)(value_2)) = rankToIntVector(rankMatrix(value_1)(1)) + rankToIntVector(rankMatrix(value_2)(1))
            }
          }
        }
      }
    }

    // print after step 1
    for (rank <- 1 until  rankToIntVector.length) {
      print(" " + rank + " : " + rankToIntVector(rank));
    }
    println();

    // step 2 check validity and calculate de product for int values
    var lastIndex : Int = 1
    var firstIndex : Int = -1
    var lastValue : Int = rankToIntVector(1)
    var temp : Int = 1
    for (rank <- 2 until  rankToIntVector.length) {
      if(rankToIntVector(rank) != -1) {
        // validate the raising monotony
        if(lastValue >= rankToIntVector(rank)) {
          return (false, rankToIntVector)
        } else {
          temp = rank-lastIndex
          lastIndex = rank
          lastValue = rankToIntVector(rank)
        }
      } else {
        if(firstIndex == -1) {
          firstIndex = rank
        }
      }
    }
    println("step_2 find index: ", firstIndex);


    //step 4 generate for first index new values if any
    if(firstIndex == -1) {
      var the_gcd : Int = rankToIntVector(1)
      for (rank <- 2 until  rankToIntVector.length) {
        the_gcd = gcd(the_gcd, rankToIntVector(rank))
      }
      println("The gcd: ", the_gcd);
      for (rank <- 1 until  rankToIntVector.length) {
        rankToIntVector(rank) = rankToIntVector(rank) / the_gcd
      }
      //print final
      for (rank <- 1 until  rankToIntVector.length) {
        print(" " + rank + " : " + rankToIntVector(rank));
      }
      println();
      return (true, rankToIntVector)
    } else {
      var nextValue : Int = 0
      var sem : Int = 0
      for (rank <- firstIndex+1 until  rankToIntVector.length) {
        if((rankToIntVector(rank) != -1) && (sem == 0) ) {
          nextValue = rankToIntVector(rank) - (rank - firstIndex) + 1
          sem = 1
        }
      }
      if(nextValue == 0) {
        return (true, rankToIntVector)
      }
      //println("next value: ", nextValue)
      //println("firstindex: ", firstIndex)
      //return (true, rankToIntVector)
      for (value_1 <- (rankToIntVector(firstIndex-1)+1) until nextValue) {
        rankToIntVector.copyToArray(temp_vector)
        temp_vector(firstIndex) = value_1

        return_value = this.generateRank(temp_vector, rankMatrix, vec_size)
        if(return_value._1 == true) {
          return return_value
        }
      }
      return  return_value
    }

  }

  def gcd(a : Int, b : Int) : Int = {
    b match {
      case 0 => a
      case x if x>a => gcd(x,a)
      case x => gcd(x, a%x)
    }
  }
}

@SerialVersionUID(123L)
class min_ge_SimpleMethod (val A_matrix : Array[Array[IntFraction]],
                          val b_vector : Array[IntFraction],
                          val P_vector : Array[IntFraction]) extends Serializable  {
  val noVariables : Int = A_matrix(0).length
  val noEquations : Int = A_matrix.length
  var M_Matrix : Array[Array[IntFraction]] = ofDim(noEquations+1, noVariables+noEquations+2)

  for ( index_1 <- 0 until (noEquations+1)) {
    for ( jindex_1 <- 0 until (noEquations+noVariables+2) ) {
      M_Matrix(index_1)(jindex_1) = new IntFraction(0)
    }
  }

  for ( index_1 <- 0 until noEquations) {
    for ( jindex_1 <- 0 until noVariables) {
      M_Matrix(index_1)(jindex_1) = A_matrix(index_1)(jindex_1)
    }
    M_Matrix(index_1)(noVariables+index_1) = new IntFraction(1)
    M_Matrix(index_1)(noEquations+noVariables+1) = b_vector(index_1)
  }



  for (jindex_1 <- 0 until noVariables) {
    M_Matrix(noEquations)(jindex_1) = P_vector(jindex_1) * -1
  }

  M_Matrix(noEquations)(noEquations+noVariables) = new IntFraction(1)

  def makeSystem() = {
    var sem : Boolean = false
    var zeroFraction : IntFraction =  new IntFraction(0)
    //var chosenRows : List[Int] = List()
    var lastRow : Array[(IntFraction,Int)] = new Array[(IntFraction, Int)](M_Matrix(noEquations).length-1)

    //chosenRows = -1 :: chosenRows
    //this.displaySystem()

    while (sem == false) {
      for ( jindex_1 <- 0 until lastRow.length ) {
        lastRow(jindex_1) = (M_Matrix(noEquations)(jindex_1)+0, jindex_1)
      }
      lastRow = lastRow.sortWith(((x: (IntFraction, Int),y : (IntFraction, Int)) => (x._1<y._1)))

      println("vector sortat")
      for ( jindex_1 <- 0 until (noEquations+noVariables+1) ) {
        print(lastRow(jindex_1) + " ")
      }
      println()

      var sem_2 : Boolean = false
      var sort_index : Int = 0
      var min_index_col : Int = 0
      var min_value_row : IntFraction = new IntFraction(0)

      while( (sem_2 == false) && (sort_index < (noEquations+noVariables)) ) {
        min_index_col = lastRow(sort_index)._2
        min_value_row = lastRow(sort_index)._1
        if(min_value_row < zeroFraction) {
          //TODO do stuff
          var min_index_row : Int = -1
          var min_value_col : IntFraction = new IntFraction(1)

          println("coloana cu minus:" + min_index_col)
          for ( index_1 <- 0 until noEquations) {
            //if(zeroFraction < M_Matrix(index_1)(min_index_col)) {
            println("randul: " + index_1 + " matriclea de pe coloana: " + M_Matrix(index_1)(min_index_col) +
              " matricea de la capata: " + M_Matrix(index_1)(noEquations+noVariables+1) + " valoarea de adevaăr a" +
              " inegalitatii: " + (zeroFraction < M_Matrix(index_1)(min_index_col)))
            if (!M_Matrix(index_1)(min_index_col).isZero &&
              //!chosenRows.contains(index_1) &&
              (zeroFraction < M_Matrix(index_1)(min_index_col))// &&
              //!M_Matrix(index_1)(noEquations+noVariables+1).isZero
            ) {
              var auxFraction : IntFraction = M_Matrix(index_1)(min_index_col) + 0
              if(M_Matrix(index_1)(noEquations+noVariables+1).isZero) {
                auxFraction = auxFraction * 10000 //BIG NUMBER
              } else {
                auxFraction = auxFraction/M_Matrix(index_1)(noEquations+noVariables+1)
              }
              if(sem_2 == false) {
                min_index_row = index_1
                min_value_col = auxFraction
                sem_2 = true
              } else {
                if(min_value_col < auxFraction) {
                  min_index_row = index_1
                  min_value_col = auxFraction
                }
              }
            }
          }
          println("O noua matrice")
          this.displaySystem()
          println("randul:" + min_index_row + "coloana:" + min_index_col)

          //chosenRows = min_index_row :: chosenRows
          if(!sem_2) {
            println("Mare belea a dat")
          } else {
            var to_multiply : IntFraction =  M_Matrix(min_index_row)(min_index_col).inverse()
            for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
              M_Matrix(min_index_row)(jindex_1) = M_Matrix(min_index_row)(jindex_1) * to_multiply
            }

            //substract the value from other equations
            for(index_1 <- 0 until (noEquations+1)) {
              if(!M_Matrix(index_1)(min_index_col).isZero && (index_1 != min_index_row)) {
                to_multiply = M_Matrix(index_1)(min_index_col)
                for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
                  M_Matrix(index_1)(jindex_1) = M_Matrix(index_1)(jindex_1) - (to_multiply * M_Matrix(min_index_row)(jindex_1))
                }
              }
            }
          }
          this.displaySystem()
        } else {
          sem = true
          //TODO : finalizare
          //this.displaySystem()
          //println("Răspunsul este" + M_Matrix(noEquations)(noEquations+noVariables+1).toString)
        }
        sort_index = sort_index + 1
      }

    }

    println("CEVVVVVVVVVVVVVVVVV")
    println("Răspunsul este " + M_Matrix(noEquations)(noEquations+noVariables+1).toString)




    /*The old WAY
    while (sem == false) {
      var min_index_col : Int = 0
      var min_value_row : IntFraction = M_Matrix(noEquations)(0)
      for ( jindex_1 <- 1 until (noEquations+noVariables) ) {
        if(M_Matrix(noEquations)(jindex_1) < min_value_row) {
          min_value_row = M_Matrix(noEquations)(jindex_1)
          min_index_col = jindex_1
        }
      }

      if(min_value_row < zeroFraction) {
        //TODO do stuff
        var min_index_row : Int = -1
        var min_value_col : IntFraction = new IntFraction(1)
        var sem_2 : Boolean = false

        println("coloana cu minus:" + min_index_col)
        for ( index_1 <- 0 until noEquations) {
          //if(zeroFraction < M_Matrix(index_1)(min_index_col)) {
          println("randul: " + index_1 + " matriclea de pe coloana: " + M_Matrix(index_1)(min_index_col) +
          " matricea de la capata: " + M_Matrix(index_1)(noEquations+noVariables+1) + " valoarea de adevaăr a" +
          " inegalitatii: " + (zeroFraction < M_Matrix(index_1)(min_index_col)))
          if (!M_Matrix(index_1)(min_index_col).isZero &&
            //!chosenRows.contains(index_1) &&
            (zeroFraction < M_Matrix(index_1)(min_index_col)) &&
            !M_Matrix(index_1)(noEquations+noVariables+1).isZero) {
            var auxFraction : IntFraction = M_Matrix(index_1)(min_index_col)/M_Matrix(index_1)(noEquations+noVariables+1)
            if(sem_2 == false) {
              min_index_row = index_1
              min_value_col = auxFraction
              sem_2 = true
            } else {
              if(min_value_col < auxFraction) {
                min_index_row = index_1
                min_value_col = auxFraction
              }
            }
          }
        }
        println("O noua matrice")
        this.displaySystem()
        println("randul:" + min_index_row + "coloana:" + min_index_col)

        //chosenRows = min_index_row :: chosenRows
        if(!sem_2) {
          println("Mare belea a dat")
        }

        var to_multiply : IntFraction =  M_Matrix(min_index_row)(min_index_col).inverse()
        for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
          M_Matrix(min_index_row)(jindex_1) = M_Matrix(min_index_row)(jindex_1) * to_multiply
        }


        //substract the value from other equations
        for(index_1 <- 0 until (noEquations+1)) {
          if(!M_Matrix(index_1)(min_index_col).isZero && (index_1 != min_index_row)) {
            to_multiply = M_Matrix(index_1)(min_index_col)
            for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
              M_Matrix(index_1)(jindex_1) = M_Matrix(index_1)(jindex_1) - (to_multiply * M_Matrix(min_index_row)(jindex_1))
            }
          }
        }
        //this.displaySystem()
      } else {
        sem = true
        //TODO : finalizare
        this.displaySystem()
        println("Răspunsul este" + M_Matrix(noEquations)(noEquations+noVariables+1).toString)
      }
    }

     */
  }

  def displaySystem() = {
    for ( index_1 <- 0 until (noEquations+1)) {
      for ( jindex_1 <- 0 until (noEquations+noVariables+2) ) {
        print(M_Matrix(index_1)(jindex_1).toString + " ")
      }
      println()
    }
  }

}



@SerialVersionUID(128L)
class Big_SimplexMethod (val A_matrix : Array[Array[IntFraction]],
                           val b_vector : Array[IntFraction],
                           val P_vector : Array[IntFraction],
                         val S_vector : Array[IntFraction]) extends Serializable  {
  val noVariables : Int = A_matrix(0).length
  val noEquations : Int = A_matrix.length
  var M_Matrix : Array[Array[BigFraction]] = ofDim(noEquations+1, noVariables+noEquations+2)

  for ( index_1 <- 0 until (noEquations+1)) {
    for ( jindex_1 <- 0 until (noEquations+noVariables+2) ) {
      M_Matrix(index_1)(jindex_1) = new BigFraction(0)
    }
  }

  for ( index_1 <- 0 until noEquations) {
    for ( jindex_1 <- 0 until noVariables) {
      M_Matrix(index_1)(jindex_1) = A_matrix(index_1)(jindex_1).toBigFraction()
    }
    M_Matrix(index_1)(noVariables+index_1) = new BigFraction(1)
    M_Matrix(index_1)(noEquations+noVariables+1) = b_vector(index_1).toBigFraction()
  }



  for (jindex_1 <- 0 until noVariables) {
    M_Matrix(noEquations)(jindex_1) = (P_vector(jindex_1) * -1).toBigFraction()
  }

  for (jindex_1 <- noVariables until noEquations+noVariables) {
    M_Matrix(noEquations)(jindex_1) = (S_vector(jindex_1-noVariables) * -1).toBigFraction()
  }

  M_Matrix(noEquations)(noEquations+noVariables) = new BigFraction(1)

  def makeSystem() : Array[BigFraction] = {
    var sem : Boolean = false
    var zeroFraction : BigFraction =  new BigFraction(0)
    //var chosenRows : List[Int] = List()
    var lastRow : Array[(BigFraction,Int)] = new Array[(BigFraction, Int)](M_Matrix(noEquations).length-1)
    var answerRow : Array[BigFraction] = new Array[BigFraction](M_Matrix(noEquations).length)

    //chosenRows = -1 :: chosenRows
    //this.displaySystem()

    while (sem == false) {
      for ( jindex_1 <- 0 until lastRow.length ) {
        lastRow(jindex_1) = (M_Matrix(noEquations)(jindex_1)+0, jindex_1)
      }
      lastRow = lastRow.sortWith(((x: (BigFraction, Int),y : (BigFraction, Int)) => (x._1<y._1)))

      /*
      println("vector sortat")
      for ( jindex_1 <- 0 until (noEquations+noVariables+1) ) {
        print(lastRow(jindex_1) + " ")
      }
      println()
      */

      var sem_2 : Boolean = false
      var sort_index : Int = 0
      var min_index_col : Int = 0
      var min_value_row : BigFraction = new BigFraction(0)

      while( (sem_2 == false) && (sort_index < (noEquations+noVariables)) ) {
        min_index_col = lastRow(sort_index)._2
        min_value_row = lastRow(sort_index)._1
        if(min_value_row < zeroFraction) {
          //TODO do stuff
          var min_index_row : Int = -1
          var min_value_col : BigFraction = new BigFraction(1)

          //println("coloana cu minus:" + min_index_col)
          for ( index_1 <- 0 until noEquations) {
            //if(zeroFraction < M_Matrix(index_1)(min_index_col)) {
            /*
            println("randul: " + index_1 + " matriclea de pe coloana: " + M_Matrix(index_1)(min_index_col) +
              " matricea de la capata: " + M_Matrix(index_1)(noEquations+noVariables+1) + " valoarea de adevaăr a" +
              " inegalitatii: " + (zeroFraction < M_Matrix(index_1)(min_index_col)))

             */
            if (!M_Matrix(index_1)(min_index_col).isZero &&
              //!chosenRows.contains(index_1) &&
              (zeroFraction < M_Matrix(index_1)(min_index_col))// &&
            //!M_Matrix(index_1)(noEquations+noVariables+1).isZero
            ) {
              var auxFraction : BigFraction = M_Matrix(index_1)(min_index_col) + 0
              if(M_Matrix(index_1)(noEquations+noVariables+1).isZero) {
                auxFraction = auxFraction * 10000 //BIG NUMBER
              } else {
                auxFraction = auxFraction/M_Matrix(index_1)(noEquations+noVariables+1)
              }
              if(sem_2 == false) {
                min_index_row = index_1
                min_value_col = auxFraction
                sem_2 = true
              } else {
                if(min_value_col < auxFraction) {
                  min_index_row = index_1
                  min_value_col = auxFraction
                }
              }
            }
          }
          //println("O noua matrice")
          //this.displaySystem()
          //println("randul:" + min_index_row + "coloana:" + min_index_col)

          //chosenRows = min_index_row :: chosenRows
          if(!sem_2) {
            println("Mare belea a dat")
          } else {
            var to_multiply : BigFraction =  M_Matrix(min_index_row)(min_index_col).inverse()
            for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
              M_Matrix(min_index_row)(jindex_1) = M_Matrix(min_index_row)(jindex_1) * to_multiply
            }

            //substract the value from other equations
            for(index_1 <- 0 until (noEquations+1)) {
              if(!M_Matrix(index_1)(min_index_col).isZero && (index_1 != min_index_row)) {
                to_multiply = M_Matrix(index_1)(min_index_col)
                for(jindex_1 <- 0 until (noEquations+noVariables+2)) {
                  M_Matrix(index_1)(jindex_1) = M_Matrix(index_1)(jindex_1) - (to_multiply * M_Matrix(min_index_row)(jindex_1))
                }
              }
            }
          }
          //this.displaySystem()
        } else {
          sem = true
          //TODO : finalizare
          //this.displaySystem()
          //println("Răspunsul este" + M_Matrix(noEquations)(noEquations+noVariables+1).toString)
        }
        sort_index = sort_index + 1
      }

    }

    println("CEVVVVVVVVVVVVVVVVV")
    println("Răspunsul este " + M_Matrix(noEquations)(noEquations+noVariables+1).toString)
    M_Matrix(noEquations).copyToArray(answerRow)
    return answerRow

  }

  def displaySystem() = {
    for ( index_1 <- 0 until (noEquations+1)) {
      for ( jindex_1 <- 0 until (noEquations+noVariables+2) ) {
        print(M_Matrix(index_1)(jindex_1).toString + " ")
      }
      println()
    }
  }

}

object Example extends App {
  var my_posit: TestPosit = new TestPosit(10)
  var aux: BigInt = 0;
  var max_exponent_size: Int = 2
  var bigValue : BigInt = 0


  var value: Int = 1
  bigValue = new BigInteger(value.toString())
  my_posit.decodeBinary(bigValue, max_exponent_size)
  my_posit.make_multiply_maxtrix()
  println("int value: " + value.toString() + "int value is: " +  my_posit.toBigInt(5) + " for: " +
    my_posit.toDouble() + " binary: " + my_posit.binaryEncode() + " display: " + my_posit.displayValue())


  var intSystem : Array[Array[Int]] = ofDim(4, 4)

  intSystem(0)(0) = 1
  intSystem(0)(1) = 2
  intSystem(0)(2) = 3
  intSystem(0)(3) = 2
  intSystem(1)(0) = 1
  intSystem(1)(1) = 3
  intSystem(1)(2) = 5
  intSystem(1)(3) = 5
  intSystem(2)(0) = 2
  intSystem(2)(1) = 4
  intSystem(2)(2) = 7
  intSystem(2)(3) = 1
  intSystem(3)(0) = -1
  intSystem(3)(1) = -2
  intSystem(3)(2) = -6
  intSystem(3)(3) = 7

  /*
  intSystem(0)(0) = 1
  intSystem(0)(1) = 2
  intSystem(0)(2) = 2
  intSystem(0)(3) = 3
  intSystem(1)(0) = 1
  intSystem(1)(1) = 3
  intSystem(1)(2) = 5
  intSystem(1)(3) = 5
  intSystem(2)(0) = 2
  intSystem(2)(1) = 4
  intSystem(2)(2) = 1
  intSystem(2)(3) = 7
  intSystem(3)(0) = -1
  intSystem(3)(1) = -2
  intSystem(3)(2) = 7
  intSystem(3)(3) = -6
   */
  /*
  intSystem(0)(0) = 2
  intSystem(0)(1) = 1
  intSystem(0)(2) = 0
  intSystem(0)(3) = 1
  intSystem(1)(0) = 4
  intSystem(1)(1) = 3
  intSystem(1)(2) = 1
  intSystem(1)(3) = 1
  intSystem(2)(0) = 6
  intSystem(2)(1) = 3
  intSystem(2)(2) = 0
  intSystem(2)(3) = 0
  intSystem(3)(0) = 0
  intSystem(3)(1) = 0
  intSystem(3)(2) = 0
  intSystem(3)(3) = 4

   */

  var mySystem = new IntHomogeneousSystem(intSystem)
  //mySystem.makeSystem()


  var return_1 : BigFraction = new BigFraction(new BigInteger("0"), new BigInteger("1"))
  var return_2 : BigFraction = new BigFraction(new BigInteger("-16"), new BigInteger("1"))
  println((return_1<return_2).toString)
  println((return_2<return_1).toString)

  val A_matrix : Array[Array[IntFraction]] = ofDim(2, 3)
  val b_vector : Array[IntFraction] = ofDim(2)
  val P_vector : Array[IntFraction] = ofDim(3)


  A_matrix(0)(0) = new IntFraction(1)
  A_matrix(0)(1) = new IntFraction(3)
  A_matrix(0)(2) = new IntFraction(2)
  A_matrix(1)(0) = new IntFraction(1)
  A_matrix(1)(1) = new IntFraction(5)
  A_matrix(1)(2) = new IntFraction(1)


  b_vector(0) = new IntFraction(10)
  b_vector(1) = new IntFraction(8)


  P_vector(0) = new IntFraction(8)
  P_vector(1) = new IntFraction(10)
  P_vector(2) = new IntFraction(7)

  var SimplexSystem : min_ge_SimpleMethod = new min_ge_SimpleMethod(A_matrix, b_vector, P_vector)
  //SimplexSystem.makeSystem()


  // write object to file// write object to file
/*
  val fos = new FileOutputStream("mybean.ser")
  val oos = new ObjectOutputStream(fos)
  oos.writeObject(SimplexSystem)
  oos.close()

  // read object from file
  val fis = new FileInputStream("mybean.ser")
  val ois = new ObjectInputStream(fis)
  val result = ois.readObject.asInstanceOf[min_ge_SimpleMethod]

  result.makeSystem()
  */
  /*
  for (value <- 0 until  256) {
    bigValue = new BigInteger(value.toString())
    my_posit.decodeBinary(bigValue, max_exponent_size)
    println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble())
  }
  */
  println("Hello World")
}
