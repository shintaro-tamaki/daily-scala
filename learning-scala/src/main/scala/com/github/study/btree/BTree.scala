package com.github.study.btree

/**
 * Created by shintaro.tamaki on 2015/10/29.
 */
sealed trait Node {
  def size: Int
  def max: Int
  def min: Int
  def sum: Int
  def avg: Double
  def find(x: Int): Option[Node]
}

case class Leaf(value: Int) extends Node {
  def size: Int = 1
  def max: Int = value
  def min: Int = value
  def sum: Int = value
  def avg: Double = value
  def find(x: Int): Option[Node] = if (x == value) Some(this) else None
}

case class Branch(left: Node, value: Int, right: Node) extends Node {
  def size: Int = left.size + 1 + right.size
  def max: Int = List(left.max, value, right.max).max
  def min: Int = List(left.min, value, right.min).min
  def sum: Int = left.sum + value + right.sum
  def avg: Double = this.sum.toDouble / this.size.toDouble
  def find(x: Int): Option[Node] = x match {
    case v if v == value => Some(this)
    case v if v < value => left.find(v)
    case v if v > value => right.find(v)
  }
}

case class BTree(node: Node) {
  def size: Int = node.size
  def max: Int = node.max
  def min: Int = node.min
  def sum: Int = node.sum
  def avg: Double = node.avg
  def find(x: Int): Option[Node] = node.find(x)
}

object BTree {
  def apply(values: List[Int]): BTree = new BTree(toNode(values))

  def toNode(values: List[Int]): Node = {
    if(values.size == 1) {
      Leaf(values.head)
    } else {
      val (left, mid :: right) = values.splitAt(values.size / 2)
      Branch(toNode(left), mid, toNode(right))
    }
  }
}