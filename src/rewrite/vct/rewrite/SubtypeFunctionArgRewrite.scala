package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

case object SubtypeFunctionArgRewrite extends RewriterBuilder {
  override def key: String = "subtypeFunctionArgRewrite"
  override def desc: String =
    "Transform predicate-subtype parameters in function signature to a function contract."
}

case class SubtypeFunctionArgRewrite[Pre <: Generation]()
    extends Rewriter[Pre] {

  private def gatherSubtypes(
      varType: Type[Pre]
  ): Seq[(SubtypeApply[Pre], Type[Pre])] =
    varType match {
      case TSubtype(refs, supertype) =>
        refs.map {
          case subtype: SubtypeApply[Pre] => (subtype, supertype)
          case other => ???
        }
      case _ => Seq()
    }

  private def extractPreds(applicable: Apply[Post]): Expr[Post] = {
    applicable match {
      case subtype: SubtypeApply[Post] => subtype.ref.decl.body.get
      case _ => tt
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    val stack = ScopedStack()
    decl match {
      case method: InstanceMethod[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg =>
            gatherSubtypes(arg.t).map(tuple =>
              tuple._1.rewrite(args =
                Seq(Local(arg.rewriteDefault().ref: Ref[Post, Variable[Post]]))
              )
            )
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
            state.appendedAll(subtypes.map(extractPreds))
          )
        classDeclarations.succeed(
          method,
          new InstanceMethod(
            returnType = dispatch(method.returnType),
            args = variables.dispatch(method.args),
            outArgs = variables.dispatch(method.outArgs),
            typeArgs = variables.dispatch(method.typeArgs),
            body = method.body.map(dispatch),
            contract = method.contract.rewrite(
              ensures = dispatch(SplitAccountedPredicate(
                foldPredicate(
                  gatherSubtypes(method.returnType).map(tuple => tuple._1)
                    .appended(tt)
                ),
                method.contract.ensures,
              )),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            ),
            method.inline,
            method.pure,
          )(method.blame)(method.o),
        )

      case other => super.dispatch(other)
    }
  }
}
