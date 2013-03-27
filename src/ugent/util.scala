package ugent

object util {
  def map3[A, B](abcs: (List[A], List[A], List[A]), f: A => B): (List[B], List[B], List[B]) = (abcs._1 map f, abcs._2 map f, abcs._3 map f)
}