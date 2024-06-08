package vct.col.ast.unsorted

import vct.col.ast.CoerceSubtypeSupertype
import vct.col.ast.ops.CoerceSubtypeSupertypeOps
import vct.col.print._

trait CoerceSubtypeSupertypeImpl[G] extends CoerceSubtypeSupertypeOps[G] { this: CoerceSubtypeSupertype[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
