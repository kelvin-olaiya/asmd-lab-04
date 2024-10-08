package lab.u04.monads

object States:

  case class State[S, R](run: S => (S, R))

  object State:
    extension [S, R](m: State[S, R])
      def apply(s: S): (S, R) = m match
        case State(run) => run(s)

  given [S]: Monad[[R] =>> State[S, R]] with
    def unit[R](a: => R): State[S, R] = State(s => (s, a))

    extension [R](m: State[S, R])
      override def flatMap[B](f: R => State[S, B]): State[S, B] =
        State(s =>
          m(s) match
            case (s2, a) => f(a).apply(s2)
        )
