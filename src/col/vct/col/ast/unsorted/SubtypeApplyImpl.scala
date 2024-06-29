package vct.col.ast.unsorted

import vct.col.ast.SubtypeApply
import vct.col.ast.ops.SubtypeApplyOps
import vct.col.print._

trait SubtypeApplyImpl[G] extends SubtypeApplyOps[G] {
  this: SubtypeApply[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    if (args.nonEmpty) { Text(ctx.name(ref)) <> "(" <> Doc.spread(args) <> ")" }
    else { Text(ctx.name(ref)) }
  }
}
