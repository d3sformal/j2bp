package common


class BinaryPredicate (val op : String, val left : Expression, val right : Expression) extends AtomicPredicate
{
	private val predStr = op + " " + left.toString() + " " + right.toString()

	
	def getOperator() : String = 
	{
		return op
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return (left.contains(exprStr) || right.contains(exprStr))
	}
	
	def containsOperand(exprStr : String) : Boolean =
	{
		return (left.toString() == exprStr) || (right.toString() == exprStr)
	}          
	
	def containsFunction(funcName : String) : Boolean =
	{
		return (left.containsFunction(funcName) || right.containsFunction(funcName))
	}
	
	override def isAlwaysTrue() : Boolean =
	{
		if ((op == "=") && (left == right)) return true
		
		return false
	}
	
	override def isAlwaysFalse() : Boolean =
	{
		if (op == "=")
		{
			if (ExpressionUtils.isConstantValue(left) && ExpressionUtils.isConstantValue(right) && (left != right)) return true
		}
				
		return false
	}
	
	override def toString() : String = 
	{
		return predStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherPred : BinaryPredicate =>
				{
					// symmetric with respect to left and right operands
					if ((this.op == otherPred.op) && (this.left == otherPred.left) && (this.right == otherPred.right)) return true
					else if (((this.op == "=") || (this.op == "!=")) && (this.op == otherPred.op) && (this.left == otherPred.right) && (this.right == otherPred.left)) return true
					else return false
				}
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		// symmetric with respect to left and right operands
		(left.hashCode() + right.hashCode()) * 31 + op.hashCode()
	}		
}
