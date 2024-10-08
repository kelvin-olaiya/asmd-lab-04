package lab.u04

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import lab.u04.monads.Monad
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import lab.u04.monads.States.State

trait MonadProperties[M[_]: Monad, T, B](
    val generator: Gen[T],
    val extract: M[T] => B
) extends Properties:
  private def unit(e: T): M[T] = summon[Monad[?]].unit(e)
  given Arbitrary[T] = Arbitrary(generator)
  given Arbitrary[M[T]] = Arbitrary(arbitrary[T].map(unit))
  given Arbitrary[T => M[T]] = Arbitrary: (x: T) =>
    unit(x)

  property("Left identity") = forAll: (e: T, f: T => M[T]) =>
    extract(unit(e).flatMap(f)) == extract(f(e))

  property("Right identity") = forAll: (m: M[T]) =>
    extract(m.flatMap(unit)) == extract(m)

  property("Associativity") = forAll: (m: M[T], f: T => M[T], g: T => M[T]) =>
    extract(m.flatMap(f).flatMap(g)) == extract(m.flatMap(x => f(x).flatMap(g)))
