package common


// immutable string with unobservable internal structure
class StringExpression (val strValue : String) extends Expression (toString())
{
	override def contains(exprStr : String) : Boolean =
	{
		if (toString() == exprStr) return true
		else return false
	}
	
	override def containsFunction(funcName : String) : Boolean =
	{
		return false
	}
	
	override def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		if (oldExprStr == toString()) return new StringExpression(newExprStr)
		else return this
	}
	
	override def toString() : String = 
	{
		return strValue
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherSE : StringExpression => 
				 (this.toString() == otherSE.toString())
			case _ => false
		}
	}
	
	override def hashCode() : Int = strValue.hashCode()
}
