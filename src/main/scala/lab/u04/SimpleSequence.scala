package lab.u04

object SimpleSequence extends SequenceADT:

  private enum SimpleSequenceImpl[A]:
    case Cons(head: A, tail: SimpleSequenceImpl[A])
    case Nil()
  import SimpleSequenceImpl.*

  opaque type Sequence[A] = SimpleSequenceImpl[A]

  override def cons[A](head: A, tail: Sequence[A]): Sequence[A] =
    SimpleSequenceImpl.Cons(head, tail)

  override def nil[A]: Sequence[A] = SimpleSequenceImpl.Nil()

  override def map[A, B](l: Sequence[A], f: A => B): Sequence[B] = l match
    case Cons(h, t) => cons(f(h), map(t, f))
    case Nil()      => nil[B]

  override def filter[A](l: Sequence[A], p: A => Boolean): Sequence[A] = l match
    case Cons(h, t) if p(h) => cons(h, filter(t, p))
    case Cons(_, t)         => filter(t, p)
    case Nil()              => nil[A]

  override def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l match
    case Cons(h, t) => cons(h, concat(t, r))
    case Nil()      => r

  override def flatMap[A, B](l: Sequence[A], f: A => Sequence[B]): Sequence[B] =
    l match
      case Cons(h, t) => concat(f(h), flatMap(t, f))
      case Nil()      => nil[B]

  override def foldLeft[A, B](l: Sequence[A], init: B, f: (B, A) => B): B =
    l match
      case Cons(h, t) => foldLeft(t, f(init, h), f)
      case Nil()      => init

  override def collect[A, B](
      l: Sequence[A],
      p: A => Boolean,
      f: A => B
  ): Sequence[B] = l match
    case Cons(h, t) if p(h) => cons(f(h), collect(t, p, f))
    case Cons(_, t)         => collect(t, p, f)
    case Nil()              => nil[B]

  override def distinct[A](l: Sequence[A]): Sequence[A] = l match
    case Cons(h, t) => cons(h, distinct(filter(t, _ != h)))
    case Nil()      => nil[A]

  override def drop[A](l: Sequence[A], n: Int): Sequence[A] = l match
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _                   => l
