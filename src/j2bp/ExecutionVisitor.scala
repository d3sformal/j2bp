package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map

import common.AtomicPredicate
import common.Expression


trait ExecutionVisitor
{
	def visitPreInstruction(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object) =
	{
	}
	
	def visitPostInstruction(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object) =
	{
	}
	
	def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
	}

	def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
	}
	
	def visitGetInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String) =
	{
	}

	def visitGotoInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, targetIndex : Int) =
	{
	}
	
	def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
	}
	
	def visitMonitorInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, insnOpcode : Int) =
	{
	}

	def visitNewInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String) =
	{
	}	
	
	def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
	}
	
	def visitReturnInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, retExpr : Expression) =
	{
	}

	def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
	}
	
	def visitSwitchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, testedValue : Expression, caseValues : List[Int], caseTargets : List[Int], defaultTarget : Int) =
	{
	}

	def visitThrowInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, excObj : Expression) =
	{
	}
	
}
