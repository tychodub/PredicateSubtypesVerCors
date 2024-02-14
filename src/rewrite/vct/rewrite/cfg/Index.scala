package vct.rewrite.cfg

import vct.col.ast._
import vct.col.ref.Ref

import scala.collection.mutable

case class GlobalIndex[G](indices: mutable.Seq[Index[G]]) {

  def enter_scope(node: Node[G], index: Int = 0): GlobalIndex[G] =
    GlobalIndex(indices.prepended(Index[G](node, index)))

  def make_step(): mutable.Set[GlobalIndex[G]] = {
    if (indices.isEmpty) return mutable.Set[GlobalIndex[G]]()
    val steps: Set[Option[Index[G]]] = indices.head.make_step()
    val res = mutable.Set[GlobalIndex[G]]()
    for (step <- steps) {
      step match {
        case Some(index) => res.addOne(GlobalIndex(index +: indices.tail))
        case None => res.addAll(GlobalIndex(indices.tail).make_step())
      }
    }
    res
  }

  def resolve(): Statement[G] = indices.dropWhile(i => !i.has_statement()).head.resolve()

  def has_statement(): Boolean = indices.head.has_statement()

  def return_from_call(): GlobalIndex[G] = {
    // Find innermost subroutine call
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case InvokeProcedureIndex(_, _) | InvokeMethodIndex(_, _) => false
      case _ => true
    }
    // Find the next statement
    // TODO: Does this always return exactly one next step?
    GlobalIndex(stack.tail).make_step().head
  }

  def handle_exception(e: Expr[G]): GlobalIndex[G] = {
    // Find innermost try-catch block of appropriate type
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case TryCatchFinallyIndex(stmt, 0) => !stmt.catches.exists(c => c.decl.t.equals(e.t))
      case _ => true
    }
    // Unhandled exception
    if (stack.isEmpty) return GlobalIndex(stack)
    // Return to exception handler and go to catch block
    stack.head match {
      case TryCatchFinallyIndex(stmt, _) => GlobalIndex(TryCatchFinallyIndex(stmt, stmt.catches.indexWhere(c => c.decl.t.equals(e.t)) + 2) +: stack.tail)
    }
  }

  def handle_break(): GlobalIndex[G] = {
    // Find innermost occurrence of either a loop or a switch statement
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) => true    // If some godless heathen calls break in the init of a loop, don't consider that loop
      case PVLLoopIndex(_, _) | LoopIndex(_, _) | RangedForIndex(_) | UnresolvedSeqLoopIndex(_, _) | SeqLoopIndex(_) | SwitchIndex(_) => false
      case _ => true
    }
    // Find the next statement
    // TODO: Does this always return exactly one next step?
    GlobalIndex(stack.tail).make_step().head
  }

  def continue_innermost_loop(): GlobalIndex[G] = {
    // Find innermost loop that could be the target of continue
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) => true
      case PVLLoopIndex(_, _) | LoopIndex(_, _) | RangedForIndex(_) | UnresolvedSeqLoopIndex(_, _) | SeqLoopIndex(_) => false
      case _ => true
    }
    stack.head match {
      case PVLLoopIndex(pvl_loop, _) => GlobalIndex(PVLLoopIndex(pvl_loop, 1) +: stack.tail)
      case LoopIndex(loop, _) => GlobalIndex(LoopIndex(loop, 1) +: stack.tail)
      case UnresolvedSeqLoopIndex(unresolved_seq_loop, _) => GlobalIndex(UnresolvedSeqLoopIndex(unresolved_seq_loop, 0) +: stack.tail)
      case RangedForIndex(_) | SeqLoopIndex(_) => GlobalIndex(stack)
    }
  }
}

sealed trait Index[G] {
  /**
   * Defines the set of possible next steps. An index in the returned set indicates that this index can replace the
   * previous index at the top level of the index stack. A None value indicates that a step is possible, but it reaches
   * outside the scope of this index to the index below.
   *
   * @return A set of all steps possible from the current index
   */
  def make_step(): Set[Option[Index[G]]]

  /**
   * Returns the statement that corresponds to the current index.
   *
   * @return The statement at the current index
   */
  def resolve(): Statement[G]

  /**
   * Determines whether the index contains a statement.
   *
   * @return true if the index contains at least one statement, false otherwise
   */
  def has_statement(): Boolean = true
}

object Index {
  def from[G](node: Node[G], index: Int): Index[G] = node match {
    case instance_method: InstanceMethod[G] => InitialIndex(instance_method)
    case run_method: RunMethod[G] => RunMethodIndex(run_method)
    case assign: Assign[G] => AssignmentIndex(assign, index)
    case pvl_branch: PVLBranch[G] => PVLBranchIndex(pvl_branch, index)
    case pvl_loop: PVLLoop[G] => PVLLoopIndex(pvl_loop, index)
    case label: Label[G] => LabelIndex(label)
    case framed_proof: FramedProof[G] => FramedProofIndex(framed_proof, index)
    case extract: Extract[G] => ExtractIndex(extract)
    case eval: Eval[G] => EvalIndex(eval, index)
    case invoke_procedure: InvokeProcedure[G] => InvokeProcedureIndex(invoke_procedure, index)
    case invoke_constructor: InvokeConstructor[G] => InvokeConstructorIndex(invoke_constructor, index)
    case invoke_method: InvokeMethod[G] => InvokeMethodIndex(invoke_method, index)
    case block: Block[G] => BlockIndex(block, index)
    case scope: Scope[G] => ScopeIndex(scope)
    case branch: Branch[G] => BranchIndex(branch, index)
    case indet_branch: IndetBranch[G] => IndetBranchIndex(indet_branch, index)
    case switch: Switch[G] => SwitchIndex(switch)
    case loop: Loop[G] => LoopIndex(loop, index)
    case ranged_for: RangedFor[G] => RangedForIndex(ranged_for)
    case try_catch_finally: TryCatchFinally[G] => TryCatchFinallyIndex(try_catch_finally, index)
    case synchronized: Synchronized[G] => SynchronizedIndex(synchronized)
    case par_invariant: ParInvariant[G] => ParInvariantIndex(par_invariant)
    case par_atomic: ParAtomic[G] => ParAtomicIndex(par_atomic)
    case par_barrier: ParBarrier[G] => ParBarrierIndex(par_barrier)
    case vec_block: VecBlock[G] => VecBlockIndex(vec_block)
    case wand_package: WandPackage[G] => WandPackageIndex(wand_package)
    case model_do: ModelDo[G] => ModelDoIndex(model_do)
    case cpp_lifetime_scope: CPPLifetimeScope[G] => CPPLifetimeScopeIndex(cpp_lifetime_scope)
    case unresolved_seq_branch: UnresolvedSeqBranch[G] => UnresolvedSeqBranchIndex(unresolved_seq_branch, index)
    case unresolved_seq_loop: UnresolvedSeqLoop[G] => UnresolvedSeqLoopIndex(unresolved_seq_loop, index)
    case seq_branch: SeqBranch[G] => SeqBranchIndex(seq_branch, index)
    case seq_loop: SeqLoop[G] => SeqLoopIndex(seq_loop)
    case veymont_assign_expression: VeyMontAssignExpression[G] => VeyMontAssignExpressionIndex(veymont_assign_expression)
    case communicatex: CommunicateX[G] => CommunicateXIndex(communicatex)
    case statement: Statement[G] => ExpressionContainerIndex(statement, index)
    case _ => ???
  }

  def apply[G](node: Node[G], index: Int): Index[G] = from(node, index)
}

case class InitialIndex[G](instance_method: InstanceMethod[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = instance_method.body.get
}

case class RunMethodIndex[G](run_method: RunMethod[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = run_method.body.get
}

case class ExpressionContainerIndex[G](statement: Statement[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index == 0) Set(Some(ExpressionContainerIndex(statement, 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = statement
}

case class AssignmentIndex[G](assign: Assign[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < 2) Set(Some(AssignmentIndex(assign, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(assign.target)(assign.target.o)
    case 1 => Eval(assign.value)(assign.value.o)
    case 2 => assign
  }
}

case class PVLBranchIndex[G](pvl_branch: PVLBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (pvl_branch.branches.size - 1))
      Set(Some(PVLBranchIndex(pvl_branch, index + 2)), Some(PVLBranchIndex(pvl_branch, index + 1)))
    else if (index == 2 * (pvl_branch.branches.size - 1))
      Set(Some(PVLBranchIndex(pvl_branch, index + 1)), None)
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0) Eval(pvl_branch.branches.apply(index / 2)._1)(pvl_branch.branches.apply(index / 2)._1.o)
    else pvl_branch.branches.apply((index - 1) / 2)._2
  }
}

case class PVLLoopIndex[G](pvl_loop: PVLLoop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(PVLLoopIndex(pvl_loop, 1)))
    case 1 => Set(Some(PVLLoopIndex(pvl_loop, 2)), None)
    case 2 => Set(Some(PVLLoopIndex(pvl_loop, 3)))
    case 3 => Set(Some(PVLLoopIndex(pvl_loop, 1)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => pvl_loop.init
    case 1 => Eval(pvl_loop.cond)(pvl_loop.cond.o)
    case 2 => pvl_loop.body
    case 3 => pvl_loop.update
  }
}

case class LabelIndex[G](label: Label[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = label.stat
}

case class FramedProofIndex[G](framed_proof: FramedProof[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < 2) Set(Some(FramedProofIndex(framed_proof, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(framed_proof.pre)(framed_proof.pre.o)
    case 1 => Eval(framed_proof.post)(framed_proof.post.o)
    case 2 => framed_proof.body
  }
}

case class ExtractIndex[G](extract: Extract[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = extract.contractedStatement
}

case class EvalIndex[G](eval: Eval[G], index: Int, subexpressions: Seq[Statement[G]]) extends Index[G] {
  def this(eval: Eval[G], index: Int) = this(eval, index, Utils.find_all_subexpressions(eval.expr))
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < subexpressions.size - 1) Set(Some(EvalIndex(eval, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = subexpressions.apply(index)
  override def has_statement(): Boolean = subexpressions.nonEmpty
}
object EvalIndex {
  def apply[G](eval: Eval[G], index: Int): EvalIndex[G] = new EvalIndex(eval, index)
}

case class InvokeProcedureIndex[G](invoke_procedure: InvokeProcedure[G], index: Int) extends Index[G] {
  // Order of operations:
  // 1. args
  // 2. given
  // 3. outArgs
  // 4. yields
  // 5. procedure body
  override def make_step(): Set[Option[Index[G]]] = {
    val args: Seq[Expr[G]] = invoke_procedure.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_procedure.givenMap
    val outArgs: Seq[Expr[G]] = invoke_procedure.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_procedure.yields
    if (index < args.size + givenMap.size + outArgs.size + yields.size - 1 ||
        index == args.size + givenMap.size + outArgs.size + yields.size - 1 && invoke_procedure.ref.decl.body.nonEmpty)
      Set(Some(InvokeProcedureIndex(invoke_procedure, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_procedure.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_procedure.givenMap
    val outArgs: Seq[Expr[G]] = invoke_procedure.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_procedure.yields
    if (index < args.size) {
      val expr: Expr[G] = args.apply(index)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size) {
      val expr: Expr[G] = givenMap.apply(index - args.size)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + yields.size) {
      val expr: Expr[G] = yields.apply(index - args.size - givenMap.size - outArgs.size)._1
      Eval(expr)(expr.o)
    } else invoke_procedure.ref.decl.body.get
  }
  override def has_statement(): Boolean =
    invoke_procedure.args.nonEmpty ||
      invoke_procedure.givenMap.nonEmpty ||
      invoke_procedure.outArgs.nonEmpty ||
      invoke_procedure.yields.nonEmpty ||
      invoke_procedure.ref.decl.body.nonEmpty
}

case class InvokeConstructorIndex[G](invoke_constructor: InvokeConstructor[G], index: Int) extends Index[G] {
  // Order of operations:
  // 1. args
  // 2. given
  // 3. outArgs
  // 4. yields
  // 5. out
  // 6. constructor body
  override def make_step(): Set[Option[Index[G]]] = {
    val args: Seq[Expr[G]] = invoke_constructor.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_constructor.givenMap
    val outArgs: Seq[Expr[G]] = invoke_constructor.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_constructor.yields
    if (index < args.size + givenMap.size + outArgs.size + yields.size ||
      index == args.size + givenMap.size + outArgs.size + yields.size && invoke_constructor.ref.decl.body.nonEmpty)
      Set(Some(InvokeConstructorIndex(invoke_constructor, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_constructor.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_constructor.givenMap
    val outArgs: Seq[Expr[G]] = invoke_constructor.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_constructor.yields
    if (index < args.size) {
      val expr: Expr[G] = args.apply(index)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size) {
      val expr: Expr[G] = givenMap.apply(index - args.size)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + yields.size) {
      val expr: Expr[G] = yields.apply(index - args.size - givenMap.size - outArgs.size)._1
      Eval(expr)(expr.o)
    } else if (index == args.size + givenMap.size + outArgs.size + yields.size) {
      Eval(invoke_constructor.out)(invoke_constructor.out.o)
    } else invoke_constructor.ref.decl.body.get
  }
}

case class InvokeMethodIndex[G](invoke_method: InvokeMethod[G], index: Int) extends Index[G] {
  // Order of operations:
  // 1. obj
  // 2. args
  // 3. given
  // 4. outArgs
  // 5. yields
  // 6. method body
  override def make_step(): Set[Option[Index[G]]] = {
    val args: Seq[Expr[G]] = invoke_method.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_method.givenMap
    val outArgs: Seq[Expr[G]] = invoke_method.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_method.yields
    if (index < args.size + givenMap.size + outArgs.size + yields.size ||
      index == args.size + givenMap.size + outArgs.size + yields.size && invoke_method.ref.decl.body.nonEmpty)
      Set(Some(InvokeMethodIndex(invoke_method, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_method.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_method.givenMap
    val outArgs: Seq[Expr[G]] = invoke_method.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_method.yields
    if (index == 0) {
      Eval(invoke_method.obj)(invoke_method.obj.o)
    } else if (index < args.size + 1) {
      val expr: Expr[G] = args.apply(index - 1)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + 1) {
      val expr: Expr[G] = givenMap.apply(index - args.size - 1)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + 1) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size - 1)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + yields.size + 1) {
      val expr: Expr[G] = yields.apply(index - args.size - givenMap.size - outArgs.size - 1)._1
      Eval(expr)(expr.o)
    } else invoke_method.ref.decl.body.get
  }
}

case class BlockIndex[G](block: Block[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < block.statements.size - 1) Set(Some(BlockIndex(block, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = block.statements.apply(index)
}

case class ScopeIndex[G](scope: Scope[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = scope.body
}

case class BranchIndex[G](branch: Branch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (branch.branches.size - 1))
      Set(Some(BranchIndex(branch, index + 2)), Some(BranchIndex(branch, index + 1)))
    else if (index == 2 * (branch.branches.size - 1))
      Set(Some(BranchIndex(branch, index + 1)), None)
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0) Eval(branch.branches.apply(index / 2)._1)(branch.branches.apply(index / 2)._1.o)
    else branch.branches.apply((index - 1) / 2)._2
  }
}

case class IndetBranchIndex[G](indet_branch: IndetBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = indet_branch.branches.apply(index)
}

// TODO: Switch cases could be multiple context indices deep; this does not work with the single index for make_step()
case class SwitchIndex[G](switch: Switch[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = switch.body
}

case class LoopIndex[G](loop: Loop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(LoopIndex(loop, 1)))
    case 1 => Set(Some(LoopIndex(loop, 2)), None)
    case 2 => Set(Some(LoopIndex(loop, 3)))
    case 3 => Set(Some(LoopIndex(loop, 1)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => loop.init
    case 1 => Eval(loop.cond)(loop.cond.o)
    case 2 => loop.body
    case 3 => loop.update
  }
}

case class RangedForIndex[G](ranged_for: RangedFor[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = ranged_for.body
}

case class TryCatchFinallyIndex[G](try_catch_finally: TryCatchFinally[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index != 1) Set(Some(TryCatchFinallyIndex(try_catch_finally, 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => try_catch_finally.body
    case 1 => try_catch_finally.after
    case _ => try_catch_finally.catches.apply(index - 2).body
  }
}

case class SynchronizedIndex[G](synchronized: Synchronized[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = synchronized.body
}

case class ParInvariantIndex[G](par_invariant: ParInvariant[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_invariant.content
}

case class ParAtomicIndex[G](par_atomic: ParAtomic[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_atomic.content
}

case class ParBarrierIndex[G](par_barrier: ParBarrier[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_barrier.content
}

case class VecBlockIndex[G](vec_block: VecBlock[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = vec_block.content
}

case class WandPackageIndex[G](wand_package: WandPackage[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = wand_package.proof
}

case class ModelDoIndex[G](model_do: ModelDo[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = model_do.impl
}

case class CPPLifetimeScopeIndex[G](cpp_lifetime_scope: CPPLifetimeScope[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = cpp_lifetime_scope.body
}

case class UnresolvedSeqBranchIndex[G](unresolved_seq_branch: UnresolvedSeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = unresolved_seq_branch.branches.apply(index)._2
}

case class UnresolvedSeqLoopIndex[G](unresolved_seq_loop: UnresolvedSeqLoop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(UnresolvedSeqLoopIndex(unresolved_seq_loop, 1)), None)
    case 1 => Set(Some(UnresolvedSeqLoopIndex(unresolved_seq_loop, 0)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(unresolved_seq_loop.cond)(unresolved_seq_loop.cond.o)
    case 1 => unresolved_seq_loop.body
  }
}

case class SeqBranchIndex[G](seq_branch: SeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = index match {
    case 0 => seq_branch.yes
    case 1 => seq_branch.no.get
  }
}

case class SeqLoopIndex[G](seq_loop: SeqLoop[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = seq_loop.body
}

case class VeyMontAssignExpressionIndex[G](veymont_assign_expression: VeyMontAssignExpression[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = veymont_assign_expression.assign
}

case class CommunicateXIndex[G](communicatex: CommunicateX[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = communicatex.assign
}