package common


class Negation (val clause : LogicFormula) extends LogicFormula
{
	private val formStr = "not (" + clause.toString() + ")"


	def getOperator() : String = 
	{
		return "not"
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return clause.containsExpression(exprStr)	
	}
	
	override def isAlwaysTrue() : Boolean =
	{
		return clause.isAlwaysFalse()
	}
	
	override def isAlwaysFalse() : Boolean =
	{
		return clause.isAlwaysTrue()
	}
	
	override def toString() : String = 
	{
		return formStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case other : Negation => 
				 (this.clause == other.clause)
			case _ => false
		}
	}
	
	override def hashCode() : Int = 
	{
		return (-1) * clause.hashCode()
	}
}
