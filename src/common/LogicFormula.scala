package common


abstract class LogicFormula
{
	def getOperator() : String

	def containsExpression(exprStr : String) : Boolean
	
	def isAlwaysTrue() : Boolean =
	{
		return false
	}
	
	def isAlwaysFalse() : Boolean =
	{
		return false
	}
}
