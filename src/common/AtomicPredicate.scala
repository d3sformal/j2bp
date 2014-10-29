package common


abstract class AtomicPredicate extends LogicFormula
{
	def containsOperand(exprStr : String) : Boolean
	
	def containsOperand(expr : Expression) : Boolean =
	{
		return containsOperand(expr.toString())
	}
	
	def containsFunction(funcName : String) : Boolean
}
