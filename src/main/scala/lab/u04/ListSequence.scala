package lab.u04

object ListSequence extends SequenceADT:

  opaque type Sequence[A] = List[A]

  override def cons[A](head: A, tail: Sequence[A]): Sequence[A] = head :: tail
  override def nil[A] = List.empty[A]

  override def map[A, B](l: Sequence[A], f: A => B): Sequence[B] = l.map(f)

  override def filter[A](l: Sequence[A], p: A => Boolean): Sequence[A] =
    l.filter(p)

  override def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l ++ r

  override def flatMap[A, B](l: Sequence[A], f: A => Sequence[B]): Sequence[B] =
    l.flatMap(f)

  override def foldLeft[A, B](l: Sequence[A], init: B, f: (B, A) => B): B =
    l.foldLeft(init)(f)

  override def collect[A, B](
      l: Sequence[A],
      p: A => Boolean,
      f: A => B
  ): Sequence[B] = l.collect { case a if p(a) => f(a) }

  override def distinct[A](l: Sequence[A]): Sequence[A] = l.distinct

  override def drop[A](l: Sequence[A], n: Int): Sequence[A] = l.drop(n)

  extension [A](l: Sequence[A])
    def head: A = l.head
    def tail: Sequence[A] = l.tail
    def isEmpty: Boolean = l.isEmpty
