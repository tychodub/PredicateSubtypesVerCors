package vct.col.ast.temporaryimplpackage.lang

import hre.util.FuncTools
import vct.col.ast.{JavaDeref, TArray, TNotAValue, Type}
import vct.col.resolve.{BuiltinField, RefAxiomaticDataType, RefJavaClass, RefJavaField, RefModel, RefModelField, RefUnloadedJavaNamespace, RefVariable}
import vct.col.util.Types

trait JavaDerefImpl[G] { this: JavaDeref[G] =>
  override def t: Type[G] = ref.get match {
    case RefModelField(decl) => decl.t
    case ref: RefUnloadedJavaNamespace[G] => Types.notAValue(ref)
    case ref: RefJavaClass[G] => Types.notAValue(ref)
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case ref: RefModel[G] => Types.notAValue(ref)
    case RefVariable(v) => v.t
    case RefJavaField(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case BuiltinField(f) => f(obj).t
  }
}