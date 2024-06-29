package vct.col.ast.unsorted

import vct.col.ast.InstanceSubtype
import vct.col.ast.ops.InstanceSubtypeOps
import vct.col.print._

trait InstanceSubtypeImpl[G] extends InstanceSubtypeOps[G] {
  this: InstanceSubtype[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Text("subtype ") <> ctx.name(this) <> Text("(") <> args.head.show <>
      Text(")(") <> Doc.spread(args.tail) <> Text(") = ") <> body.get.show <>
      Text(";")
  }
}
