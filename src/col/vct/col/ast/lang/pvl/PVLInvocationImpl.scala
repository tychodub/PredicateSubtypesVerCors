package vct.col.ast.lang.pvl

import vct.col.ast.{Applicable, PVLInvocation, TClass, TProcess, TResource, Type}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Text}
import vct.col.resolve.ctx._
import vct.col.ast.ops.PVLInvocationOps
import vct.col.ref.Ref

trait PVLInvocationImpl[G] extends PVLInvocationOps[G] { this: PVLInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) => decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(_) => TResource()
    case RefInstanceFunction(decl) =>
      val returnType = decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
      obj.flatMap { e =>
        e.t.asClass.map { tcls =>
          tcls.instantiate(returnType)
        }
      }.getOrElse(returnType)
    case RefInstanceMethod(decl) =>
      val returnType = decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
      obj.flatMap { e =>
        e.t.asClass.map { tcls =>
          tcls.instantiate(returnType)
        }
      }.getOrElse(returnType)
    case RefInstancePredicate(_) => TResource()
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(_) => TProcess()
    case RefModelAction(_) => TProcess()
    case RefProverFunction(decl) => decl.returnType
    case PVLBuiltinInstanceMethod(f) => f(obj.get)(args).t
    case BuiltinInstanceMethod(f) => f(obj.get)(args).t
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Group(obj.map(assoc(_) <> ".").getOrElse(Empty) <>
      method <>
      (if(typeArgs.isEmpty) Empty else Text("<") <> Doc.args(typeArgs) <> ">")) <>
      "(" <> Doc.args(args) <> ")") <>
      DocUtil.givenYields(givenMap, yields))
}