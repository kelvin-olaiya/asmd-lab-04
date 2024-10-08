package lab.u04

import org.scalacheck.{Arbitrary, Gen, Properties, Cogen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

trait SequenceADTProperties[T](
    val adt: SequenceADT,
    val generator: Gen[T],
    private val cogen: Cogen[T]
) extends Properties:
  import adt.*

  given Arbitrary[T] = Arbitrary(generator)
  given Cogen[T] = Cogen(cogen)
  given Arbitrary[T => T] = Arbitrary(Gen.function1(arbitrary[T]))
  given Arbitrary[Sequence[T]] = Arbitrary(
    Gen.listOf(arbitrary[T]).map(_.foldRight(nil[T])(cons))
  )

  property("Map axioms") = forAll: (seq: Sequence[T], f: T => T) =>
    seq match
      case seq if !seq.isEmpty =>
        map(seq, f) == cons(f(seq.head), map(seq.tail, f))
      case seq => map(seq, f) == nil[T]

  property("Filter axioms") = forAll: (seq: Sequence[T], p: T => Boolean) =>
    seq match
      case seq if !seq.isEmpty =>
        filter(seq, p) == (if p(seq.head) then
                             cons(seq.head, filter(seq.tail, p))
                           else filter(seq.tail, p))
      case seq => filter(seq, p) == nil[T]

  property("Concat axioms") = forAll: (seq1: Sequence[T], seq2: Sequence[T]) =>
    seq1 match
      case seq if !seq.isEmpty =>
        concat(seq1, seq2) == cons(seq.head, concat(seq.tail, seq2))
      case seq => concat(seq1, seq2) == seq2

  property("FlatMap axioms") = forAll:
    (seq: Sequence[T], f: T => Sequence[T]) =>
      seq match
        case seq if !seq.isEmpty =>
          flatMap(seq, f) == concat(f(seq.head), flatMap(seq.tail, f))
        case seq => flatMap(seq, f) == nil[T]

  property("FoldLeft axioms") = forAll:
    (seq: Sequence[T], init: T, f: (T, T) => T) =>
      seq match
        case seq if !seq.isEmpty =>
          foldLeft(seq, init, f) == foldLeft(seq.tail, f(init, seq.head), f)
        case seq => foldLeft(seq, init, f) == init

  property("Collect axioms") = forAll:
    (seq: Sequence[T], p: T => Boolean, f: T => T) =>
      seq match
        case seq if !seq.isEmpty =>
          collect(seq, p, f) == (if p(seq.head) then
                                   cons(f(seq.head), collect(seq.tail, p, f))
                                 else collect(seq.tail, p, f))
        case seq => collect(seq, p, f) == nil[T]

  property("Distinct axioms") = forAll: (seq: Sequence[T]) =>
    seq match
      case seq if !seq.isEmpty =>
        distinct(seq) == cons(
          seq.head,
          distinct(filter(seq.tail, _ != seq.head))
        )
      case seq => distinct(seq) == nil[T]

  property("Drop axioms") = forAll: (seq: Sequence[T], n: Int) =>
    (seq, n) match
      case (seq, n) if n > 0 && !seq.isEmpty =>
        drop(seq, n) == drop(seq.tail, n - 1)
      case _ => drop(seq, n) == seq
