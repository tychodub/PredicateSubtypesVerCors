package vct.col.ast.unsorted

import vct.col.ast.GlobalSubtype
import vct.col.ast.ops.GlobalSubtypeOps
import vct.col.print._

trait GlobalSubtypeImpl[G] extends GlobalSubtypeOps[G] { this: GlobalSubtype[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
