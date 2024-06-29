package vct.col.ast.unsorted

import vct.col.ast.GlobalSubtype
import vct.col.ast.ops.GlobalSubtypeOps
import vct.col.print._

trait GlobalSubtypeImpl[G] extends GlobalSubtypeOps[G] {
  this: GlobalSubtype[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Text("subtype ") <> ctx.name(this) <> Text("(") <> args.head.show <>
      Text(")(") <> Doc.spread(args.tail) <> Text(") = ") <> body.get.show <>
      Text(";")
  }
}
