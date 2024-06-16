package vct.col.ast.declaration.category

import vct.col.ast.{AbstractSubtype, Expr, TResource, Type}

trait AbstractSubtypeImpl[G] extends ApplicableImpl[G] {
  this: AbstractSubtype[G] =>
  override def returnType: Type[G] = TResource()
  override def body: Option[Expr[G]]
}
