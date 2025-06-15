package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.{
  And,
  Apply,
  BooleanValue,
  Class,
  ClassDeclaration,
  Declaration,
  Expr,
  GlobalSubtype,
  Implies,
  InstanceSubtype,
  Local,
  Not,
  Or,
  SubtypeApply,
  TSubtype,
  Type,
}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute

import scala.collection.mutable

case object SubtypeNestedRewrite extends RewriterBuilder {
  override def key: String = "subtypeNestedRewrite"

  override def desc: String =
    "Transform predicate-subtypes defined on top of predicate subtypes into subtype on base type."

}

case class SubtypeNestedRewrite[Pre <: Generation]() extends Rewriter[Pre] {
  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  private def gatherSubtypes(varType: Type[Pre]): Expr[Pre] =
    varType match {
      case TSubtype(refs, _, _) => refs
      case _ => tt
    }

  private def subtypeAlgebraEval(
      implicit o: Origin,
      subtypeExpr: Expr[Pre],
      subtypeVar: Expr[Pre],
  ): Expr[Post] = {
    subtypeExpr match {
      case subtype: SubtypeApply[Pre] => dispatch(subtype, subtypeVar)
      case and: And[Pre] =>
        subtypeAlgebraEval(o, and.left, subtypeVar) &&
        subtypeAlgebraEval(o, and.right, subtypeVar)
      case or: Or[Pre] =>
        subtypeAlgebraEval(o, or.left, subtypeVar) ||
        subtypeAlgebraEval(o, or.right, subtypeVar)
      case implies: Implies[Pre] =>
        subtypeAlgebraEval(o, implies.left, subtypeVar) ==>
          subtypeAlgebraEval(o, implies.right, subtypeVar)
      case not: Not[Pre] => Not(subtypeAlgebraEval(o, not.arg, subtypeVar))
      case other => other.rewriteDefault()
    }
  }

  def dispatch(e: Expr[Pre], annotated: Expr[Pre]): Expr[Post] =
    e match {
      case apply: SubtypeApply[Pre] =>
        implicit val o: Origin = apply.o

        lazy val args = Substitute(Map.from[Expr[Pre], Expr[Pre]](
          for (
            (arg, v) <- apply.args.prepended(annotated).zip(apply.ref.decl.args)
          )
            yield (v.get, arg)
        ))

        args.dispatch(apply.ref.decl.body.get).rewriteDefault()
      case other => other.rewriteDefault()
    }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o

    decl match {
      case subtype: InstanceSubtype[Pre] =>
        val subtypeVar = subtype.args.head
        val subtypeExpr = gatherSubtypes(subtypeVar.t)

        classDeclarations.succeed(
          subtype,
          subtype.rewrite(body =
            Option(
              if (
                subtypeExpr match {
                  case BooleanValue(true) => true
                  case _ => false
                }
              ) { subtype.body.get.rewriteDefault() }
              else {
                subtype.body.get.rewriteDefault() && subtypeAlgebraEval(
                  o,
                  gatherSubtypes(subtypeVar.t),
                  Local(subtypeVar.ref),
                )
              }
            )
          ),
        )
      case subtype: GlobalSubtype[Pre] =>
        val subtypeVar = subtype.args.head
        val subtypeExpr = gatherSubtypes(subtypeVar.t)

        globalDeclarations.succeed(
          subtype,
          subtype.rewrite(body =
            Option(
              if (
                subtypeExpr match {
                  case BooleanValue(true) => true
                  case _ => false
                }
              ) { subtype.body.get.rewriteDefault() }
              else {
                subtype.body.get.rewriteDefault() && subtypeAlgebraEval(
                  o,
                  gatherSubtypes(subtypeVar.t),
                  Local(subtypeVar.ref),
                )
              }
            )
          ),
        )
      case other => super.dispatch(other)
    }
  }

}
