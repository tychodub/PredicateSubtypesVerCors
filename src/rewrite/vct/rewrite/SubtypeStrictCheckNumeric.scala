package vct.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.NoSuchNameError
import vct.col.resolve.lang.Spec
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}

case object SubtypeStrictCheckNumeric extends RewriterBuilderArg[Boolean] {
  override def key: String = "subtypeStrictCheckNumeric"

  override def desc: String =
    "Transform non-subtyped numeric types into default strict subtypes that check for overflows."

}

case class SubtypeStrictCheckNumeric[Pre <: Generation](
    strictArithmeticChecks: Boolean
) extends Rewriter[Pre] {
  implicit val o: Origin = Origin(Seq())
  private lazy val addedIntSubtype: GlobalSubtype[Post] = {
    val declVar: Variable[Post] = new Variable(TInt())(o.where(name = "x"))
    val varRef = Local(declVar.ref: Ref[Post, Variable[Post]])
    val addedSubtype = {
      new GlobalSubtype(
        Seq(declVar),
        Some(And(
          LessEq(IntegerValue(-2147483648), varRef),
          LessEq(varRef, IntegerValue(2147483647)),
        )),
      )(o.where(name = "strictIntGenerated"))
    }
    addedSubtype
  }

  override def dispatch(varType: Type[Pre]): Type[Post] = {
    implicit val o: Origin = varType.o
    varType match {
      case TSubtype(refs, supertype, strict) =>
        TSubtype(refs.rewriteDefault(), super.dispatch(supertype), strict)
      case intType: TInt[Pre] if strictArithmeticChecks =>
        TSubtype(
          SubtypeApply(addedIntSubtype.ref, Seq()),
          intType.rewriteDefault(),
          strict = true,
        )
      case other => other.rewriteDefault()
    }
  }

  override def dispatch(decl: Program[Pre]): Program[Post] = {
    implicit val o: Origin = decl.o
    decl match {
      case program if strictArithmeticChecks =>
        program.rewrite(declarations =
          globalDeclarations.collect { program.declarations.map(dispatch) }
            ._1 ++ Seq(addedIntSubtype)
        )

      case other => super.dispatch(other)
    }
  }
}
