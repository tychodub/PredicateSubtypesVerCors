package vct.col.ast.unsorted

import vct.col.ast.JavaTSubtype
import vct.col.ast.ops.JavaTSubtypeOps
import vct.col.print._

trait JavaTSubtypeImpl[G] extends JavaTSubtypeOps[G] { this: JavaTSubtype[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
