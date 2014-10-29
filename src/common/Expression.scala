package common


// variable names and constants are represented by this class
class Expression (val valueStr : String)
{
	def contains(exprStr : String) : Boolean =
	{
		if (toString() == exprStr) return true
		
		for (delim1 <- ExpressionUtils.delimiters)
		{
			if ( toString().startsWith(exprStr + delim1) || toString().endsWith(delim1 + exprStr) ) return true

			for (delim2 <- ExpressionUtils.delimiters)
			{				
				if (toString().contains(delim1 + exprStr + delim2)) return true
			}
		}
		
		return false	
	}
	
	def containsFunction(funcName : String) : Boolean =
	{
		return false
	}
	
	def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		return ExpressionUtils.createExprFromStr(valueStr.replace(oldExprStr, newExprStr))
	}
	
	override def toString() : String = 
	{
		return valueStr
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherExpr : Expression => 
				 (this.toString() == otherExpr.toString())
			case _ => false
		}
	}
	
	override def hashCode() : Int = valueStr.hashCode()
}
