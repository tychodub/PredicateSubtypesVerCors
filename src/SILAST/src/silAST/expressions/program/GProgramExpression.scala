package silAST.expressions.program

import scala.collection.Seq
import silAST.expressions.logical.LogicalExpression
import silAST.source.SourceLocation
import silAST.expressions.logical.GLogicalExpression
import silAST.expressions.program.terms.GProgramTerm

trait GProgramExpression[+T <: GProgramTerm[T]] extends GLogicalExpression[T] 
{
//  def subExpressions(): Seq[GLogicalExpression[T]]
}