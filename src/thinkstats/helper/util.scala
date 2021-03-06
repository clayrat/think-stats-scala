package thinkstats.helper

import scala.util.Random

object util {
  def map2[A, B](abs: (List[A], List[A]), f: A => B): (List[B], List[B]) = (abs._1 map f, abs._2 map f)
  def map3[A, B](abcs: (List[A], List[A], List[A]), f: A => B): (List[B], List[B], List[B]) = (abcs._1 map f, abcs._2 map f, abcs._3 map f)

  def randomSetElement[A](s: Set[A]) = s.toList(Random.nextInt(s.size))

  def ozToGram(n: Double) = n * 28.349523125

  def removeLastZero[A](a: List[(A, A)]) = a.dropRight(1) :+ (a.last._1, a.dropRight(1).last._2)
  def removeFirstZeroes[A](a: List[(A, A)]) = {
    val (xs, ys) = a.unzip
    val index = xs.lastIndexWhere(_ == 0) + 1
    (List.fill(index)(xs(index)) ++ xs.drop(index) zip ys)
  }

}