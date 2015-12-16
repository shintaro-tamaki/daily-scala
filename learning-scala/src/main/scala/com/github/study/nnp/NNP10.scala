package com.github.study.nnp

import scala.annotation.tailrec

/**
 * Created by tamaki on 2015/02/08.
 */
trait NNP10 {

  // P01 (*) Find the last element of a list.
  def last(list: List[Int]): Int = {
    @tailrec
    def _last(list: List[Int]): Int = {
      list match {
        case Nil => throw new IllegalArgumentException
        case x :: Nil => x
        case x1 :: x2 => _last(x2)
      }
    }
    _last(list)
  }

  // P02 (*) Find the last but one element of a list.
  def penultimate(list: List[Int]): Int = {
    @tailrec
    def _penultimate(list: List[Int]): Int = {
      list match {
        case Nil => throw new IllegalArgumentException
        case x1 :: Nil => throw new IllegalArgumentException
        case x1 :: x2 if x2.length == 1 => x1
        case x1 :: x2 if x2.length > 1 => _penultimate(x2)
      }
    }
    _penultimate(list)
  }

  def nth(n: Int, list: List[Int]): Int = {
    @tailrec
    def _nth(list: List[Int], acc:Int): Int = {
      list match {
        case Nil => throw new IllegalArgumentException
        case x1 :: x2 if acc == 0 => x1
        case x1 :: x2 if acc > 0 => _nth(x2, acc - 1)
      }
    }
    _nth(list, n)
  }

  def length(list: List[Int]): Int = {
    @tailrec
    def _length(list: List[Int], acc: Int): Int ={
      list match {
        case Nil => acc
        case x :: Nil => _length(Nil, acc + 1)
        case x :: xs => _length(xs , acc + 1)
      }
    }
    _length(list, 0)
  }

  def reverse(list: List[Int]): List[Int] = {
    @tailrec
    def _reverse(list: List[Int], acc: List[Int]): List[Int] ={
      list match {
        case x :: Nil => x :: acc
        case x :: xs => _reverse(xs , x :: acc)
      }
    }
    _reverse(list, Nil)
  }

  def isPalindrome(list: List[Int]): Boolean = {
    @tailrec
    def _isPalidone(list: List[Int]): Boolean = {
      list match {
        case Nil => true
        case head :: Nil => true
        case head :: tail if head != tail.reverse.head => false
        case head :: tail if head == tail.reverse.head => _isPalidone(tail.dropRight(1))
      }
    }
    _isPalidone(list)
  }

  def flatten(nested: List[Any]): List[Any] = {
    def _flatten(list: List[Any], acc: List[Any]): List[Any] = {
      list match {
        case Nil => acc.reverse
        case head :: tail =>
          head match {
            case x:List[_] => _flatten(x ::: tail, acc)
            case x:Any => _flatten(tail, x :: acc)
          }
      }
    }
    _flatten(nested, Nil)
  }

  def compress(list: List[Symbol]): List[Symbol] = {
    @tailrec
    def _compress(list: List[Symbol], acc: List[Symbol]): List[Symbol] ={
      list match {
        case Nil => acc.reverse
        case x1 :: x2  => _compress(x2.dropWhile(_ == x1), x1 :: acc)
      }
    }
    _compress(list, Nil)
  }

  def pack(list: List[Symbol]): List[List[Symbol]] = {
    @tailrec
    def _pack(list: List[Symbol], acc: List[List[Symbol]]): List[List[Symbol]] ={
      list match {
        case Nil => acc.reverse
        case x1 :: x2  => _pack(x2.dropWhile(_ == x1), (x1 :: x2.takeWhile(_ == x1)) :: acc)
      }
    }
    _pack(list, Nil)
  }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    @tailrec
    def _encode(list: List[Symbol], acc: List[(Int, Symbol)]): List[(Int, Symbol)] ={
      list match {
        case Nil => acc.reverse
        case x1 :: x2  => _encode(x2.dropWhile(_ == x1), (x2.takeWhile(_ == x1).length + 1, x1) :: acc)
      }
    }
    _encode(list, Nil)
  }

}