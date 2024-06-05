package vct.col.ast.declaration.category

import vct.col.ast.{AbstractSubtype, TResource, Type}

trait AbstractSubtypeImpl[G] extends ApplicableImpl[G] {
  this: AbstractSubtype[G] =>
  override def returnType: Type[G] = TResource()
}
