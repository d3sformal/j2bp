package common


class Implication (val ante : LogicFormula, val cons : LogicFormula) extends LogicFormula
{	
	private val formStr = "(" + ante.toString() + ") => ("  + cons.toString() + ")"
	
	
	def getOperator() : String = 
	{
		return "=>"
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		if (ante.containsExpression(exprStr)) return true
		if (cons.containsExpression(exprStr)) return true
		
		return false
	}

	override def toString() : String = 
	{
		return formStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case other : Implication => 
				 (this.ante == other.ante) && (this.cons == other.cons)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		ante.hashCode() * 31 + cons.hashCode()
	}

}
