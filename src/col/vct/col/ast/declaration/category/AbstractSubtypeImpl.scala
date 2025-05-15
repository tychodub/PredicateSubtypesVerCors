package vct.col.ast.declaration.category

import vct.col.ast.{AbstractSubtype, Expr, TResource, Type, TBool}

trait AbstractSubtypeImpl[G] extends ApplicableImpl[G] {
  this: AbstractSubtype[G] =>
  override def returnType: Type[G] = TBool()
  override def body: Option[Expr[G]]
}
