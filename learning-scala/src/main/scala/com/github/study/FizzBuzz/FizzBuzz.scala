package com.github.study.FizzBuzz

import scala.util.{Failure, Success, Try}

/**
 * Created by tamaki on 2015/02/08.
 */
object FizzBuzz {
  def main(args: Array[String]): Unit = {
    part1((1 to 100)).map(println)
    part2((1 to 100)).map(println)
    part3((1 to 100)).map(println)
    println(part4((1 to 100)))
    println(part5((1 to 100)))
    part6().map(println)
  }

  private def toFizzBuzz(in: Int) = {
    (in % 3, in % 5) match {
      case (0, 0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case (_, _) => in.toString
    }
  }

  private def part1(list: Seq[Int]) = {
    list.map(toFizzBuzz)
  }

  private def part2(list: Seq[Int]) = {
    list.map(m => if (m % 3 == 0 && m % 5 == 0) "FizzBuzz" else if (m % 3 == 0) "Fizz" else if (m % 5 == 0) "Buzz" else m.toString)
  }

  private def part3(list: Seq[Int]) = {
    list.withFilter(_ % 2 != 0).map(toFizzBuzz)
  }

  private def part4(list: Seq[Int]) = {
    list.map(toFizzBuzz).mkString(",")
  }

  private def part5(list: Seq[Int]) = {
    list
      .map(toFizzBuzz)
      .flatMap{ m =>
        Try(m.toInt) match {
          case Success(x) => Some(x)
          case Failure(_) => None
        }
      }
      .sum
  }

  private def part6() = {
    def _part6(n: Int, acc:List[String]): List[String] = {
      n match {
        case x if x == 100 =>
          (toFizzBuzz(n) :: acc).reverse
        case x if x >= 1 &&x < 100 =>
          _part6(x + 1, toFizzBuzz(n) :: acc)
        case _ =>
          throw new IllegalArgumentException
      }
    }
    _part6(1, Nil)
  }


}
