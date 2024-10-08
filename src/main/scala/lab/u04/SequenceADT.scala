package lab.u04

trait SequenceADT:

  type Sequence[A]
  /*
   * constructors
   */
  def cons[A](head: A, tail: Sequence[A]): Sequence[A]
  def nil[A]: Sequence[A]
  /*
   * operations
   */
  def map[A, B](l: Sequence[A], f: A => B): Sequence[B]
  def filter[A](l: Sequence[A], p: A => Boolean): Sequence[A]
  def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A]
  def flatMap[A, B](l: Sequence[A], f: A => Sequence[B]): Sequence[B]
  def foldLeft[A, B](l: Sequence[A], init: B, f: (B, A) => B): B
  def collect[A, B](l: Sequence[A], p: A => Boolean, f: A => B): Sequence[B]
  def distinct[A](l: Sequence[A]): Sequence[A]
  def drop[A](l: Sequence[A], n: Int): Sequence[A]
  /*
   * extension methods
   */
  extension [A](l: Sequence[A])
    def head: A
    def tail: Sequence[A]
    def isEmpty: Boolean
