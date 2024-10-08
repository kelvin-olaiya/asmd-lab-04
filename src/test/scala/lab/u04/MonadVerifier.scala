package lab.u04

import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import lab.u04.monads.States.{*, given}
import lab.u04.monads.Optionals.Optional
import lab.u04.monads.Optionals.Optional.{Just, Empty}

object StateVerifier
    extends Properties("State monad Verifier")
    with MonadProperties(arbitrary[Int], (s: State[Int, Int]) => s(0))

object OptionsVerifier
    extends Properties("Option monad Verifier")
    with MonadProperties(arbitrary[Int], (o: Optional[Int]) => o)
