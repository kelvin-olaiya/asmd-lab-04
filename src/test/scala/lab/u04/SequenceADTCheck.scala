package lab.u04

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import lab.u04.ListSequence.head

trait SequenceADTCheck(val adt: SequenceADT) extends Properties:
  import adt.*
  given Arbitrary[adt.Sequence[Int]] = Arbitrary:
    Gen.listOf(Gen.choose(0, 100)).map(_.foldRight(adt.nil[Int])(adt.cons))

  property("map") = forAll: (seq: Sequence[Int], f: Int => Int) =>
    seq match
      case seq if !seq.isEmpty =>
        map(seq, f) == cons(f(seq.head), map(seq.tail, f))
      case seq => adt.map(seq, f) == nil[Int]

  property("filter") = forAll: (seq: Sequence[Int], p: Int => Boolean) =>
    seq match
      case seq if !seq.isEmpty =>
        filter(seq, p) == (if p(seq.head) then
                             cons(seq.head, filter(seq.tail, p))
                           else filter(seq.tail, p))
      case seq => filter(seq, p) == nil[Int]

  property("concat") = forAll: (seq1: Sequence[Int], seq2: Sequence[Int]) =>
    seq1 match
      case seq if !seq.isEmpty =>
        concat(seq1, seq2) == cons(seq.head, concat(seq.tail, seq2))
      case seq => concat(seq1, seq2) == seq2

  property("flatMap") = forAll: (seq: Sequence[Int], f: Int => Sequence[Int]) =>
    seq match
      case seq if !seq.isEmpty =>
        flatMap(seq, f) == concat(f(seq.head), flatMap(seq.tail, f))
      case seq => flatMap(seq, f) == nil[Int]

  property("foldLeft") = forAll:
    (seq: Sequence[Int], init: Int, f: (Int, Int) => Int) =>
      seq match
        case seq if !seq.isEmpty =>
          foldLeft(seq, init, f) == foldLeft(seq.tail, f(init, seq.head), f)
        case seq => foldLeft(seq, init, f) == init

  property("collect") = forAll:
    (seq: Sequence[Int], p: Int => Boolean, f: Int => Int) =>
      seq match
        case seq if !seq.isEmpty =>
          collect(seq, p, f) == (if p(seq.head) then
                                   cons(f(seq.head), collect(seq.tail, p, f))
                                 else collect(seq.tail, p, f))
        case seq => collect(seq, p, f) == nil[Int]

  property("distinct") = forAll: (seq: Sequence[Int]) =>
    seq match
      case seq if !seq.isEmpty =>
        distinct(seq) == cons(
          seq.head,
          distinct(filter(seq.tail, _ != seq.head))
        )
      case seq => distinct(seq) == nil[Int]

  property("drop") = forAll: (seq: Sequence[Int], n: Int) =>
    (seq, n) match
      case (seq, n) if n > 0 && !seq.isEmpty =>
        drop(seq, n) == drop(seq.tail, n - 1)
      case _ => drop(seq, n) == seq

object ListSequenceCheck
    extends Properties("ListSequence")
    with SequenceADTCheck(ListSequence)
object SimpleSequenceCheck
    extends Properties("SimpleSequence")
    with SequenceADTCheck(SimpleSequence)
