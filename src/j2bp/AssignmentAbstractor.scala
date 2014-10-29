package j2bp

import org.objectweb.asm.MethodVisitor

import common.AtomicPredicate
import common.Expression


abstract class AssignmentAbstractor
{
	def generateAbstractStore(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, srcValueExpr : Expression, bcIndex : Int, tgtVarType : String) : Unit

	def generateAbstractPutfield(ctx : AbstractionContext, mv : MethodVisitor, tgtObj : Expression, fieldName : String, srcValueExpr : Expression, bcIndex : Int, fieldType : String)

	def generateVariableLoad(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, tgtPred : AtomicPredicate, srcValueExpr : Expression, srcPredSet : Set[AtomicPredicate]) 	
}
