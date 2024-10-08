package lab.u04.monads

object Optionals:

  // data structure for optionals
  enum Optional[A]:
    case Just(a: A)
    case Empty()

  // minimal set of algorithms
  object Optional:
    extension [A](m: Optional[A])
      def filter(p: A => Boolean): Optional[A] = m match
        case Just(a) if (p(a)) => m
        case _                 => Empty()

  // extending Optional as a Monad!
  given Monad[Optional] with
    import Optional.{Just, Empty}

    // unit: just boxing the value
    def unit[A](a: => A): Optional[A] = Just(a)

    // flatMap: opens the box if possible, gives the new box
    extension [A](m: Optional[A])
      def flatMap[B](f: A => Optional[B]): Optional[B] =
        m match
          case Just(a) => f(a)
          case Empty() => Empty()
