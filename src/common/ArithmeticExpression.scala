/*
 * Copyright (C) 2015, Charles University in Prague.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
