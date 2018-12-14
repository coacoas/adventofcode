package advent

case class Zipper[A](before: List[A], head: A, tail: List[A]) {
  def prev: Zipper[A] = Zipper(before.tail, before.head, head :: tail)
  def next: Zipper[A] = Zipper(head :: before, tail.head, tail.tail)

  def remove: Zipper[A] =
    if (tail.isEmpty) Zipper(before.tail, before.head, List.empty)
    else Zipper(before, tail.head, tail.tail)

  def insert(item: A): Zipper[A] =
    Zipper(head :: before, item, tail)

  def toList: List[A] =
    before.reverse ++ (head :: tail)

  def toVector: Vector[A] =
    (before.reverse.toVector :+ head) ++ tail.toVector
}

object Zipper {
  implicit class RichList[A](val l: List[A]) extends AnyVal {
    def toZipper: Zipper[A] = Zipper(List.empty, l.head, l.tail)
  }
}
