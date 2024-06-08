package vct.col.ast.unsorted

import vct.col.ast.CoerceSupertypeSubtype
import vct.col.ast.ops.CoerceSupertypeSubtypeOps
import vct.col.print._

trait CoerceSupertypeSubtypeImpl[G] extends CoerceSupertypeSubtypeOps[G] { this: CoerceSupertypeSubtype[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
