package common


class ExistentialQuantification (val quantVarName : String, val clause : LogicFormula) extends LogicFormula
{
	private val formStr = "exists " + quantVarName + " : (" + clause + ")"


	def getOperator() : String = 
	{
		return "exists"
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return clause.containsExpression(exprStr) || (quantVarName == exprStr)
	}
	
	override def toString() : String = 
	{
		return formStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case other : ExistentialQuantification => 
				 (this.quantVarName == other.quantVarName) && (this.clause == other.clause)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		clause.hashCode() * 31 + quantVarName.hashCode()
	}		
}
