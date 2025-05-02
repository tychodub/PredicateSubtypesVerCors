package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{AssertFailed, AssignSubtypeFailed, Blame, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.rewrite.SubtypeFunctionArgRewrite.{
  AssertSubtypeFailed,
  Replacement,
  Replacements,
}

import scala.collection.mutable

case object SubtypeFunctionArgRewrite extends RewriterBuilder {
  override def key: String = "subtypeFunctionArgRewrite"

  override def desc: String =
    "Transform predicate-subtype parameters in function signature to a function contract."

  case class AssertSubtypeFailed(assign: Statement[_])
      extends Blame[AssertFailed] {

    override def blame(error: AssertFailed): Unit = {
      assign.o.blame(AssignSubtypeFailed(assign))
    }
  }

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

  override def dispatch(varType: Type[Pre]): Type[Post] =
    varType match {
      case TSubtype(_, supertype) => supertype.rewriteDefault()
      case other => other.rewriteDefault()
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

  private def subtypeAlgebraEvalLocal(
      implicit o: Origin,
      subtypeVar: Variable[Pre],
  ): Expr[Post] = {
    if (gatherSubtypes(subtypeVar.t).nonEmpty) {
      gatherSubtypes(subtypeVar.t).map(implications =>
        implications.map(subtypes =>
          subtypes.map(subtype => dispatch(subtype, Local(subtypeVar.ref)))
            .reduceLeft(_ && _)
        ).reduceRight(_ ==> _)
      ).reduceLeft(_ || _)
    } else { tt }
  }

  private def subtypeAlgebraEvalResult(
      implicit o: Origin,
      subtypeVar: ContractApplicable[Pre],
  ): Expr[Post] = {
    if (gatherSubtypes(subtypeVar.returnType).nonEmpty) {
      gatherSubtypes(subtypeVar.returnType).map(implications =>
        implications.map(subtypes =>
          subtypes.map(subtype => dispatch(subtype, Result(subtypeVar.ref)))
            .reduceLeft(_ && _)
        ).reduceRight(_ ==> _)
      ).reduceLeft(_ || _)
    } else { tt }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case variable: Variable[Pre] =>
        variables.succeed(variable, new Variable(dispatch(variable.t)))

      case method: InstanceMethod[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
              state.appended(subtype)
            )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, method)).appended(tt)
                ),
                method.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case method: InstanceOperatorMethod[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
              state.appended(subtypes)
            )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, method)).appended(tt)
                ),
                method.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case method: Constructor[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
              state.appended(subtypes)
            )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = method.contract.ensures.rewriteDefault(),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case method: InstanceFunction[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
              state.appended(subtypes)
            )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, method)).appended(tt)
                ),
                method.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case method: InstanceOperatorFunction[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
              state.appended(subtypes)
            )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, method)).appended(tt)
                ),
                method.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                method.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case function: Function[Pre] =>
        val argExpressions: Seq[Expr[Post]] = {
          function.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
              state.appended(subtype)
            )
        }
        globalDeclarations.succeed(
          function,
          function.rewrite(contract =
            function.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, function)).appended(tt)
                ),
                function.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                function.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case function: LlvmSpecFunction[Pre] =>
        val argExpressions: Seq[Expr[Post]] = {
          function.args.map(arg => subtypeAlgebraEvalLocal(o, arg))
            .foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
              state.appended(subtype)
            )
        }
        globalDeclarations.succeed(
          function,
          function.rewrite(contract =
            function.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(
                  Seq(subtypeAlgebraEvalResult(o, function)).appended(tt)
                ),
                function.contract.ensures.rewriteDefault(),
              ),
              requires = SplitAccountedPredicate(
                foldPredicate(argExpressions),
                function.contract.requires.rewriteDefault(),
              ),
            )
          ),
        )

      case subtype: AbstractSubtype[Pre] => subtype.drop()
      case other => super.dispatch(other)
    }
  }

  private def subtypeAlgebraEvalAssert(
      implicit o: Origin,
      subtypeVar: Assign[Pre],
  ): Assert[Post] = {
    Assert(
      if (gatherSubtypes(subtypeVar.target.t).nonEmpty) {
        gatherSubtypes(subtypeVar.target.t).map(implications =>
          implications.map(subtypes =>
            subtypes.map(subtype => dispatch(subtype, subtypeVar.target))
              .reduceLeft(_ && _)
          ).reduceRight(_ ==> _)
        ).reduceLeft(_ || _)
      } else
        { tt }: Expr[Post]
    )(AssertSubtypeFailed(subtypeVar))
  }

  private def addAssert(stat: Statement[Pre]): Seq[Statement[Post]] = {
    implicit val o: Origin = stat.o
    stat match {
      case loop: Loop[Pre] => Seq(dispatch(loop))
      case assign: Assign[Pre] =>
        Seq(dispatch(assign)).appended(subtypeAlgebraEvalAssert(o, assign))
      case assign =>
        Seq(dispatch(assign)).appendedAll(
          assign.collect { case expr: AssignExpression[Pre] => expr.target }
            .map(target =>
              Assert(
                if (gatherSubtypes(target.t).nonEmpty) {
                  gatherSubtypes(target.t).map(implications =>
                    implications.map(subtypes =>
                      subtypes.map(subtype => dispatch(subtype, target))
                        .reduceLeft(_ && _)
                    ).reduceRight(_ ==> _)
                  ).reduceLeft(_ || _)
                } else
                  { tt }: Expr[Post]
              )(AssertSubtypeFailed(assign))
            )
        )
      case other => Seq(dispatch(other)) // I think this is unreachable
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case block: Block[Pre] =>
        block.rewrite(statements =
          block.statements.foldLeft(Seq(): Seq[Statement[Post]])((seq, stat) =>
            seq.appendedAll(addAssert(stat))
          )
        )
      case loop: Loop[Pre] =>
        val subtypeExpressions: LazyList[Expr[Post]] = loop.init.collect {
          case assign: Assign[Pre] => assign.target
        }.map(target =>
          if (gatherSubtypes(target.t).nonEmpty) {
            gatherSubtypes(target.t).map(implications =>
              implications.map(subtypes =>
                subtypes.map(subtype => dispatch(subtype, target))
                  .reduceLeft(_ && _)
              ).reduceRight(_ ==> _)
            ).reduceLeft(_ || _)
          } else { tt }
        )

        loop.rewrite(
          init = loop.init.rewriteDefault(),
          update = loop.update.rewriteDefault(),
          contract = {
            loop.contract match {
              case invariant: LoopInvariant[Pre] =>
                invariant.rewrite(
                  invariant.invariant.rewriteDefault() &*
                    subtypeExpressions
                      .foldLeft(tt: Expr[Post])((state, expr) => state && expr)
                )
              case other => other.rewriteDefault()
            }
          },
        )

      case other => other.rewriteDefault()
    }
  }
}
