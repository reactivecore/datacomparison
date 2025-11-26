package net.reactivecore.datacomparison

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TestBase extends AnyFlatSpec with Matchers {
  protected def lineTrim(s: String): String = {
    s.linesIterator.map(_.trim).mkString("\n")
  }
}
