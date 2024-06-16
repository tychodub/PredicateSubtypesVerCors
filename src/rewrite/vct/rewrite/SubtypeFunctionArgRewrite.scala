package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, PreferredName}
import vct.col.ref.Ref
import vct.col.rewrite.InlineApplicables.InlineLetThisOrigin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.rewrite.SubtypeFunctionArgRewrite.{Replacement, Replacements}

import scala.collection.mutable

case object SubtypeFunctionArgRewrite extends RewriterBuilder {
  override def key: String = "subtypeFunctionArgRewrite"

  override def desc: String =
    "Transform predicate-subtype parameters in function signature to a function contract."

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

case class SubtypeFunctionArgRewrite[Pre <: Generation]()
    extends Rewriter[Pre] {

  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  private def gatherSubtypes(varType: Type[Pre]): Seq[SubtypeApply[Pre]] =
    varType match {
      case TSubtype(refs, _) =>
        refs.map {
          case subtype: SubtypeApply[Pre] => subtype
          case other => ???
        }
      case _ => Seq()
    }

  private def extractSupertype(varType: Type[Pre]): Type[Pre] =
    varType match {
      case TSubtype(_, supertype) => supertype
      case other => other
    }

  def dispatch(e: Expr[Pre], annotated: Expr[Pre]): Expr[Post] =
    e match {
      case apply: SubtypeApply[Pre] =>
        implicit val o: Origin = apply.o

        inlineStack.having(apply) {
          lazy val args = Replacements(
            for (
              (arg, v) <- apply.args.appended(annotated)
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
      case variable: Variable[Pre] =>
        variables.succeed(
          variable,
          new Variable(dispatch(extractSupertype(variable.t))),
        )

      case method: InstanceMethod[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg =>
            gatherSubtypes(arg.t)
              .map(subtype => dispatch(subtype, Local(arg.ref)))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
            state.appendedAll(subtypes)
          )
        classDeclarations.succeed(
          method,
          new InstanceMethod(
            returnType = dispatch(extractSupertype(method.returnType)),
            args = variables.dispatch(method.args),
            outArgs = variables.dispatch(method.outArgs),
            typeArgs = variables.dispatch(method.typeArgs),
            body = method.body.map(dispatch),
            contract = method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  gatherSubtypes(method.returnType)
                    .map(subtype => dispatch(subtype, Result(method.ref)))
                    .appended(tt)
                ),
                method.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            ),
            method.inline,
            method.pure,
          )(method.blame)(method.o),
        )

      case subtype: AbstractSubtype[Pre] => subtype.drop()
      case other => super.dispatch(other)
    }
  }
}
