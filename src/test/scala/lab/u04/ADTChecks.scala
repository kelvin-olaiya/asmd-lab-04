package lab.u04

import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.{cogenInt, cogenString}

object IntSimpleSequenceCheck
    extends Properties("Int SimpleSequence")
    with SequenceADTProperties(SimpleSequence, arbitrary[Int], cogenInt)
object StringSimpleSequenceCheck
    extends Properties("String SimpleSequence")
    with SequenceADTProperties(SimpleSequence, arbitrary[String], cogenString)
object IntListSequenceCheck
    extends Properties("Int ListSequence")
    with SequenceADTProperties(ListSequence, arbitrary[Int], cogenInt)
object StringListSequenceCheck
    extends Properties("String ListSequence")
    with SequenceADTProperties(ListSequence, arbitrary[String], cogenString)
