package common


class Conjunction (val clauses : List[LogicFormula]) extends LogicFormula
{
	private var formStr : String = null
	private var hc : Int = -1
	
	
	def getOperator() : String = 
	{
		return "and"
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		for (cl <- clauses)
		{
			if (cl.containsExpression(exprStr)) return true
		}

		return false
	}
	
	override def toString() : String = 
	{
		if (formStr != null) return formStr
		
		formStr = ""
	
		formStr = formStr + "(" + clauses.head.toString() + ")"
		
		for (cl <- clauses.tail)
		{
			formStr = formStr + " and "
			
			formStr = formStr + "(" + cl.toString() + ")"
		}

		return formStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case other : Conjunction => 
				 (this.clauses == other.clauses)
			case _ => false
		}
	}
	
	override def hashCode() : Int = 
	{
		if (hc != -1) return hc
		
		hc = 0
		
		for (cl <- clauses)
		{
			hc = hc * 31 + cl.hashCode() + 1
		}
		
		return hc
	}
}
