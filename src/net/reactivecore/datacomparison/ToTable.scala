package net.reactivecore.datacomparison

import scala.compiletime.*
import scala.deriving.Mirror

/** Type class converting something to a table. */
trait ToTable[T] {

  /** Converts a values to a table. */
  def toTable(values: Seq[T]): Table
}

/** To-Table conversion, where always the same columns for a type are returned. */
trait StaticToTable[T] extends ToTable[T] {

  /** Column Names. */
  def columns: Seq[String]

  /** Converts a single row */
  def row(value: T): Seq[String]

  final def toTable(values: Seq[T]): Table = {
    val rows = values.map(row)
    Table(columns, rows)
  }
}

object ToTable {

  /** Derives a GenericTabular for a Product */
  inline def derived[T <: Product](using m: Mirror.ProductOf[T]): ToTable[T] = {
    val labels        = deriveLabels[T].toVector
    val cellExtractor = deriveCellExtractor[T]
    new StaticToTable[T] {
      override def columns: Seq[String] = {
        labels
      }

      override def row(value: T): Seq[String] = {
        cellExtractor(value).toVector
      }
    }
  }

  private inline def deriveLabels[T](using m: Mirror.Of[T]): List[String] = {
    summonLabels[m.MirroredElemLabels]
  }

  private inline def summonLabels[T <: Tuple]: List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ValueOf[t]].value.asInstanceOf[String] :: summonLabels[ts]
    }
  }

  private inline def deriveCellExtractor[T <: Product](using m: Mirror.ProductOf[T]): T => Seq[String] = {
    val rowExtractor = deriveCellExtractorForTuple[m.MirroredElemTypes]
    value => rowExtractor(value.productIterator.toList)
  }

  private inline def deriveCellExtractorForTuple[T <: Tuple]: List[Any] => List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => _ => Nil
      case _: (t *: ts)  =>
        val cellRenderer   = summonInline[GenericCellRenderer[t]]
        val childExtractor = deriveCellExtractorForTuple[ts]
        {
          case head :: tail =>
            cellRenderer.render(head.asInstanceOf[t]) :: childExtractor(tail)
          case Nil          => Nil
        }
    }
  }

}
