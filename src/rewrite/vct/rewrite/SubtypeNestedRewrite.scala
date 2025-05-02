package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.{
  Apply,
  Class,
  ClassDeclaration,
  Declaration,
  Expr,
  InstanceSubtype,
  Let,
  Local,
  SubtypeApply,
  TSubtype,
  Type,
  Variable,
}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.rewrite.SubtypeNestedRewrite.{Replacement, Replacements}

import scala.annotation.tailrec
import scala.collection.mutable

case object SubtypeNestedRewrite extends RewriterBuilder {
  override def key: String = "subtypeNestedRewrite"

  override def desc: String =
    "Transform predicate-subtypes defined on top of predicate subtypes into subtype on base type."

  case class Replacement[Pre](replacing: Expr[Pre], binding: Expr[Pre])(
      implicit o: Origin
  ) {
    val withVariable: Variable[Pre] = new Variable(replacing.t)

    def +(other: Replacements[Pre]): Replacements[Pre] =
      Replacements(Seq(this)) + other

    def +(other: Replacement[Pre]): Replacements[Pre] =
      Replacements(Seq(this)) + other
  }

  case class Replacements[Pre](replacements: Seq[Replacement[Pre]]) {
    def +(other: Replacements[Pre]): Replacements[Pre] =
      Replacements(replacements ++ other.replacements)

    def +(other: Replacement[Pre]): Replacements[Pre] =
      Replacements(replacements :+ other)

    def expr(e: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
      val sub = Substitute[Pre](
        replacements.map(r => r.replacing -> r.withVariable.get).toMap
      )
      val replaced = sub.labelDecls.scope { sub.dispatch(e) }
      replacements.foldRight(replaced) { case (replacement, e) =>
        Let(replacement.withVariable, replacement.binding, e)(e.o)
      }
    }
  }
}

case class SubtypeNestedRewrite[Pre <: Generation]() extends Rewriter[Pre] {
  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  private def gatherSubtypes(
      varType: Type[Pre]
  ): Seq[Seq[Seq[SubtypeApply[Pre]]]] =
    varType match {
      case TSubtype(refs, _) =>
        refs.map(or =>
          or.map(implications =>
            implications.map {
              case subtype: SubtypeApply[Pre] => subtype
              case _ => ???
            }
          )
        )
      case _ => Seq()
    }

  def dispatch(e: Expr[Pre], annotated: Expr[Pre]): Expr[Post] =
    e match {
      case apply: SubtypeApply[Pre] =>
        implicit val o: Origin = apply.o

        inlineStack.having(apply) {
          lazy val args = Replacements(
            for (
              (arg, v) <- apply.args.prepended(annotated)
                .zip(apply.ref.decl.args)
            )
              yield Replacement(v.get, arg)(v.o)
          )

          dispatch(args.expr(apply.ref.decl.body.get))
        }
      case other => other.rewriteDefault()
    }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o

    decl match {
      case subtype: InstanceSubtype[Pre] =>
        val subtypeVar = subtype.args.head

        classDeclarations.succeed(
          subtype,
          subtype.rewrite(body =
            Option((
              if (gatherSubtypes(subtypeVar.t).nonEmpty) {
                subtype.body.get.rewriteDefault() &&
                gatherSubtypes(subtypeVar.t).map(implications =>
                  implications.map(subtypes =>
                    subtypes
                      .map(subtype => dispatch(subtype, Local(subtypeVar.ref)))
                      .reduceLeft(_ && _)
                  ).reduceRight(_ ==> _)
                ).reduceLeft(_ || _)
              } else { subtype.body.get.rewriteDefault() }
            ))
          ),
        )
      case other => super.dispatch(other)
    }
  }

}
