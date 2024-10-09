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

  property("Map axioms") = forAll: (h: T, t: Sequence[T], f: T => T) =>
    map(nil[T], f) == nil[T]
    map(cons(h, t), f) == cons(f(h), map(t, f))

  property("Filter axioms") = forAll: (h: T, t: Sequence[T], p: T => Boolean) =>
    filter(nil[T], p) == nil[T]
    filter(cons(h, t), p) == (if p(h) then cons(h, filter(t, p))
                              else filter(t, p))

  property("Concat axioms") = forAll: (h: T, t: Sequence[T], l: Sequence[T]) =>
    concat(nil[T], l) == l
    concat(cons(h, t), l) == cons(h, concat(t, l))

  property("FlatMap axioms") = forAll:
    (h: T, t: Sequence[T], f: T => Sequence[T]) =>
      flatMap(nil[T], f) == nil[T]
      flatMap(cons(h, t), f) == concat(f(h), flatMap(t, f))

  property("FoldLeft axioms") = forAll:
    (h: T, t: Sequence[T], init: T, f: (T, T) => T) =>
      foldLeft(nil[T], init, f) == init
      foldLeft(cons(h, t), init, f) == foldLeft(t, f(init, h), f)

  property("Collect axioms") = forAll:
    (h: T, t: Sequence[T], p: T => Boolean, f: T => T) =>
      collect(nil[T], p, f) == nil[T]
      collect(cons(h, t), p, f) == (if p(h) then cons(f(h), collect(t, p, f))
                                    else collect(t, p, f))

  property("Distinct axioms") = forAll: (h: T, t: Sequence[T]) =>
    distinct(nil[T]) == nil[T]
    distinct(cons(h, t)) == cons(h, distinct(filter(t, _ != h)))

  property("Drop axioms") = forAll: (h: T, t: Sequence[T], n: Int) =>
    drop(cons(h, t), n) == (if n > 0 then drop(t, n - 1) else cons(h, t))
    drop(nil[T], n) == nil[T]
