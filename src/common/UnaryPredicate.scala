package common


class UnaryPredicate (val op : String, val arg : Expression) extends AtomicPredicate
{
	private var predStr : String = null
	
	
	def getOperator() : String = 
	{
		return op
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return arg.contains(exprStr)
	}
	
	def containsOperand(exprStr : String) : Boolean =
	{
		return (arg.toString() == exprStr)
	}
	
	def containsFunction(funcName : String) : Boolean =
	{
		return arg.containsFunction(funcName)
	}
	
	override def toString() : String = 
	{
		if (predStr != null) return predStr
		
		predStr = ""
	
		if (op == "") predStr = arg.toString()
		else predStr = op + " " + arg.toString()
		
		return predStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherPred : UnaryPredicate => 
				 (this.op == otherPred.op) && (this.arg == otherPred.arg)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		arg.hashCode() * 31 + op.hashCode()
	}
}
