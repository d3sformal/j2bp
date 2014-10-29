package j2bp.containers

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import scala.util.matching.Regex

import common.AtomicPredicate
import common.Expression
import common.FunctionExpression
import common.UnaryPredicate
import common.BinaryPredicate
import common.ExpressionUtils


object ContainerPredicateUtils
{
	def isMapOrderPredicate(pred : AtomicPredicate) : Boolean =
	{
		if ( ! pred.isInstanceOf[UnaryPredicate] ) return false
		
		val unPred = pred.asInstanceOf[UnaryPredicate]
		
		if ( ! unPred.arg.isInstanceOf[FunctionExpression] ) return false
		
		return (unPred.arg.asInstanceOf[FunctionExpression].name == BasicContainerModel.FUNC_MAP_ORDER)
	}
	
	def containsFieldReadOverMapGet(pred : AtomicPredicate) : Boolean =
	{
		if (pred.containsFunction(BasicContainerModel.FUNC_MAP_GET))
		{
			if (pred.isInstanceOf[BinaryPredicate])
			{
				val binPred = pred.asInstanceOf[BinaryPredicate]
			
				if (ExpressionUtils.isFieldAccessPath(binPred.left) && binPred.left.containsFunction(BasicContainerModel.FUNC_MAP_GET)) return true
				if (ExpressionUtils.isFieldAccessPath(binPred.right) && binPred.right.containsFunction(BasicContainerModel.FUNC_MAP_GET)) return true
			}
		}
		
		return false
	}
	
	def extractMapOrderFuncExpr(morderPred : AtomicPredicate) : FunctionExpression =
	{
		return morderPred.asInstanceOf[UnaryPredicate].arg.asInstanceOf[FunctionExpression]
	}
	
	def extractMapKeysViewPair(mkeysPred : AtomicPredicate) : (Expression, Expression) =
	{
		val mkeysFunc = mkeysPred.asInstanceOf[UnaryPredicate].arg.asInstanceOf[FunctionExpression]
		
		return (mkeysFunc.args(1), mkeysFunc.args(2))
	}
	
	def extractMapValuesViewPair(mvaluesPred : AtomicPredicate) : (Expression, Expression) =
	{
		val mvaluesFunc = mvaluesPred.asInstanceOf[UnaryPredicate].arg.asInstanceOf[FunctionExpression]
		
		return (mvaluesFunc.args(1), mvaluesFunc.args(2))
	}
}
