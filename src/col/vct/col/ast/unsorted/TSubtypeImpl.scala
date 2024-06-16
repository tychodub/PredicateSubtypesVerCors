package vct.col.ast.unsorted

import vct.col.ast.TSubtype
import vct.col.ast.ops.TSubtypeOps
import vct.col.print._

trait TSubtypeImpl[G] extends TSubtypeOps[G] { this: TSubtype[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
