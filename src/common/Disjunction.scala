package common


class Disjunction (val clauses : List[LogicFormula]) extends LogicFormula
{
	private var formStr : String = null
	
	
	def getOperator() : String = 
	{
		return "or"
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
			formStr = formStr + " or "

			formStr = formStr + "(" + cl.toString() + ")"
		}

		return formStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case other : Disjunction => 
				 (this.clauses == other.clauses)
			case _ => false
		}
	}
	
	override def hashCode() : Int = 
	{
		return clauses.hashCode() + 1
	}
}
