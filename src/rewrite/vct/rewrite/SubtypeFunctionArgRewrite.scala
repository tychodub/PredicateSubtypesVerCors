package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{
  AssertFailed,
  AssignSubtypeFailed,
  Blame,
  ExprSubtypeFailed,
  Origin,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.rewrite.SubtypeFunctionArgRewrite.{
  AssertExprSubtypeFailed,
  AssertSubtypeFailed,
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

  case class AssertExprSubtypeFailed(assign: Expr[_])
      extends Blame[AssertFailed] {

    override def blame(error: AssertFailed): Unit = {
      assign.o.blame(ExprSubtypeFailed(assign))
    }
  }
}

case class SubtypeFunctionArgRewrite[Pre <: Generation]()
    extends Rewriter[Pre] {

  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  private def gatherSubtypes(varType: Type[Pre]): Expr[Pre] =
    varType match {
      case TSubtype(refs, _, _) => refs
      case _ => tt
    }

  private def gatherStrictSubtypes(varType: Type[Pre]): Expr[Pre] =
    varType match {
      case TSubtype(refs, _, true) => refs
      case _ => tt
    }

  override def dispatch(varType: Type[Pre]): Type[Post] =
    varType match {
      case TSubtype(_, supertype, _) => supertype.rewriteDefault()
      case other => other.rewriteDefault()
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

  private def subtypeExprAssertions(
      implicit o: Origin,
      subtypeExpr: Expr[Pre],
  ): Expr[Post] = {

    subtypeAlgebraEval(o, subCheck(o, subtypeExpr), subtypeExpr)
  }

  private def subCheck(
      implicit o: Origin,
      arithmeticExpr: Expr[Pre],
  ): Expr[Pre] = {
    def subCheckMatch(arithmeticExpr: Expr[Pre]): Seq[Expr[Pre]] = {
      arithmeticExpr match {
        case op: NumericBinExpr[Pre] =>
          subCheckMatch(op.left).appendedAll(subCheckMatch(op.right))
        case other =>
          Seq(gatherStrictSubtypes(other.t)).filter(x =>
            x match { case BooleanValue(true) => false; case _ => true }
          )
      }
    }
    val expressions = subCheckMatch(arithmeticExpr)
    if (expressions.isEmpty) { tt: Expr[Pre] }
    else if (expressions.size == 1) { expressions.head }
    else { expressions.reduce(Or(_, _)) }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case variable: Variable[Pre] =>
        variables.succeed(variable, new Variable(dispatch(variable.t)))

      case method: InstanceMethod[Pre] =>
        val argExpressions: Seq[Expr[Post]] =
          method.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
            state.appended(subtype)
          )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(method.returnType),
                  Result(method.ref),
                ))),
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
          method.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
            state.appended(subtypes)
          )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(method.returnType),
                  Result(method.ref),
                ))),
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
          method.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
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
          method.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
            state.appended(subtypes)
          )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(method.returnType),
                  Result(method.ref),
                ))),
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
          method.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtypes) =>
            state.appended(subtypes)
          )
        classDeclarations.succeed(
          method,
          method.rewrite(contract =
            method.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(method.returnType),
                  Result(method.ref),
                ))),
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
          function.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
            state.appended(subtype)
          )
        }
        globalDeclarations.succeed(
          function,
          function.rewrite(contract =
            function.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(function.returnType),
                  Result(function.ref),
                ))),
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
          function.args.map(arg =>
            subtypeAlgebraEval(o, gatherSubtypes(arg.t), Local(arg.ref))
          ).foldLeft(Seq(tt): Seq[Expr[Post]])((state, subtype) =>
            state.appended(subtype)
          )
        }
        globalDeclarations.succeed(
          function,
          function.rewrite(contract =
            function.contract.rewrite(
              ensures = SplitAccountedPredicate(
                foldPredicate(Seq(subtypeAlgebraEval(
                  o,
                  gatherSubtypes(function.returnType),
                  Result(function.ref),
                ))),
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

  private def subtypeAlgebraEvalAssertStrict(
      implicit o: Origin,
      subtypeVar: Assign[Pre],
  ): Seq[Assert[Post]] = {
    Seq(
      Assert(subtypeAlgebraEval(
        o,
        gatherSubtypes(subtypeVar.target.t),
        subtypeVar.value,
      ))(AssertSubtypeFailed(subtypeVar))
    )
  }

  private def addAssert(stat: Statement[Pre]): Seq[Statement[Post]] = {
    implicit val o: Origin = stat.o
    stat match {
      case loop: Loop[Pre] => Seq(dispatch(loop))
      case assign: Assign[Pre] =>
        Seq(dispatch(assign))
          .appendedAll(subtypeAlgebraEvalAssertStrict(o, assign))
      case stat =>
        Seq(dispatch(stat)).appendedAll(
          stat.collect { case expr: AssignExpression[Pre] => expr.target }
            .map(target =>
              Assert(subtypeAlgebraEval(o, gatherSubtypes(target.t), target))(
                AssertSubtypeFailed(stat)
              )
            )
        ).prependedAll(
          stat.collect { case expr: Expr[Pre] => expr }.map(expr =>
            Assert(subtypeExprAssertions(expr.o, expr))(AssertExprSubtypeFailed(
              expr
            ))
          ).filter { case Assert(BooleanValue(true)) => false; case _ => true }
        )
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
        }.map(target => subtypeAlgebraEval(o, gatherSubtypes(target.t), target))

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
