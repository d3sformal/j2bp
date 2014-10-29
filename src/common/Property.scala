package common


class Property (val formula : LogicFormula, val codeLocations : List[(String, Int)])
{
	private var propStr : String = null

	
	override def toString() : String = 
	{
		if (propStr != null) return propStr
		
		propStr = formula.toString()
		
		propStr = propStr + " : "
		
		var first = true
	
		for ( (mthName, bcPos) <- codeLocations)
		{
			if (first) first = false
			else propStr = propStr + ","

			propStr = propStr + "(" + mthName + "," + bcPos + ")"
		}

		return propStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherProp : Property => 
				 (this.formula == otherProp.formula) && (this.codeLocations == otherProp.codeLocations)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		formula.hashCode() * 31 + codeLocations.hashCode()
	}
}
