package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.StratifyExpressions.MultipleEndpoints

import scala.collection.immutable.ListSet

object StratifyUnpointedExpressions extends RewriterBuilder {
  override def key: String = "stratifyUnpointedExpressions"
  override def desc: String =
    "Stratifies expressions that could be in any endpoint context by duplicating the expression to all endpoints currently participating"
}

case class StratifyUnpointedExpressions[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] {
  val currentParticipants: ScopedStack[ListSet[Endpoint[Pre]]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          currentParticipants.having(ListSet.from(chor.endpoints)) {
            rewriteDefault(chor)
          }
        }

      case decl => rewriteDefault(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] =
    contract match {
      case InChor(_, contract) =>
        contract.rewrite(
          requires = (stratifyExpr(_)).accounted(contract.requires),
          ensures = (stratifyExpr(_)).accounted(contract.ensures),
          contextEverywhere = stratifyExpr(contract.contextEverywhere),
        )
      case _ => contract.rewriteDefault()
    }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(_, inv: LoopInvariant[Pre]) =>
        inv.rewrite(invariant = stratifyExpr(inv.invariant))
      case InChor(_, contract: IterationContract[Pre]) =>
        contract.rewrite(
          requires = stratifyExpr(contract.requires),
          ensures = stratifyExpr(contract.ensures),
        )
      case _ => contract.rewriteDefault()
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case InChor(_, c @ ChorStatement(branch: Branch[Pre])) =>
        // All Chor branches must have two branches, of which the second one has a tt condition
        val (guard, yes, noOpt) =
          branch.branches match {
            case Seq((guard, yes)) => (guard, yes, None)
            case Seq((guard, yes), (BooleanValue(true), no)) =>
              (guard, yes, Some(no))
            case _ => ???
          }
        val newParticipants =
          if (c.hasUnpointed) { currentParticipants.top }
          else { ListSet.from(c.participants) }
        currentParticipants.having(newParticipants) {
          c.rewrite(inner =
            branch.rewrite(branches =
              (stratifyExpr(guard), super.dispatch(yes)) +:
                noOpt.map(no => Seq((tt[Post], super.dispatch(no))))
                  .getOrElse(Seq())
            )
          )
        }

      case InChor(_, c @ ChorStatement(loop: Loop[Pre])) =>
        val newParticipants =
          if (c.hasUnpointed) { currentParticipants.top }
          else { ListSet.from(c.participants) }
        currentParticipants.having(newParticipants) {
          c.rewrite(inner = loop.rewrite(cond = stratifyExpr(loop.cond)))
        }

      case statement => statement.rewriteDefault()
    }

  def stratifyExpr(expr: Expr[Pre]): Expr[Post] = {
    implicit val o = expr.o
    foldAny(expr.t)(unfoldStar(expr).flatMap {
      case expr: EndpointExpr[Pre] => Seq(expr.rewriteDefault())
      case expr =>
        currentParticipants.top.map { endpoint =>
          EndpointExpr[Post](succ(endpoint), dispatch(expr))
        }.toSeq
    })
  }
}
