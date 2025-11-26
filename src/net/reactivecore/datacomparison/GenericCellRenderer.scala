package net.reactivecore.datacomparison

import scala.deriving.Mirror

/** Renderer for cells */
trait GenericCellRenderer[-T] {
  def render(value: T): String
}

trait LowPrioGenericCellRenderer {
  given defaultRenderer: GenericCellRenderer[Any] = value => value.toString
}

object GenericCellRenderer extends LowPrioGenericCellRenderer {

  given optionalRenderer[T](using underlying: GenericCellRenderer[T]): GenericCellRenderer[Option[T]] = value =>
    value.map(underlying.render).getOrElse("n.A.")

  given seqRenderer[T](using underlying: GenericCellRenderer[T]): GenericCellRenderer[Seq[T]] = value =>
    value
      .map { v =>
        underlying.render(v)
      }
      .mkString(",")

  inline given productRenderer[T <: Product](using m: Mirror.ProductOf[T]): GenericCellRenderer[T] = {
    val gt = ToTable.derived[T]
    new GenericCellRenderer[T] {
      override def render(value: T): String = {
        val table = gt.toTable(Seq(value))
        table.columns.view
          .zip(table.rows.head)
          .map { case (k, v) =>
            s"${k}: ${v}"
          }
          .mkString("{", ",", "}")
      }
    }
  }
}
