package common


class ArithmeticExpression (val op : String, val left : Expression, val right : Expression) extends Expression (toString())
{
	private var aeStr : String = null

	
	override def containsFunction(funcName : String) : Boolean =
	{
		return (left.containsFunction(funcName) || right.containsFunction(funcName))
	}
		
	override def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		if (oldExprStr == toString()) return ExpressionUtils.createExprFromStr(newExprStr)
		
		return new ArithmeticExpression(op, left.replace(oldExprStr, newExprStr), right.replace(oldExprStr, newExprStr))
	}
	
	override def toString() : String = 
	{
		if (aeStr != null) return aeStr
				
		aeStr = left + " " + op + " " + right
	
		return aeStr
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherExpr : ArithmeticExpression =>
				{
					// symmetric with respect to left and right operand
					if ((this.op == otherExpr.op) && (this.left == otherExpr.left) && (this.right == otherExpr.right)) return true
					else if (((this.op == "+") || (this.op == "*")) && (this.op == otherExpr.op) && (this.left == otherExpr.right) && (this.right == otherExpr.left)) return true
					else return false
				}
			case _ => false
		}
	}
	
	override def hashCode() : Int = 
	{
		// symmetric with respect to left and right operand
		(left.hashCode() + right.hashCode()) * 31 + op.hashCode()
	}
}
