package vct.col.ast.unsorted

import vct.col.ast.TSubtype
import vct.col.ast.ops.TSubtypeOps
import vct.col.print._

trait TSubtypeImpl[G] extends TSubtypeOps[G] {
  this: TSubtype[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    val spread = refs.map(s1 => s1.map(Doc.lspread(_)))
    val subtypes =
      if (spread.isEmpty) { Text(""): Doc }
      else {
        spread.tail
          .foldLeft(foldImplies(ctx, spread.head))((state, implications) =>
            state <> " | " <> foldImplies(ctx, implications)
          )
      }
    Text("subtype") <> "<" <> supertype.show <> "," <> subtypes <> ">"
  }

  private def foldImplies(implicit ctx: Ctx, implications: Seq[Doc]): Doc = {
    if (implications.isEmpty) { Text(""): Doc }
    else {
      implications.tail.foldLeft(implications.head)((state, subtypes) =>
        state <> " ==> " <> subtypes
      )
    }
  }
}
