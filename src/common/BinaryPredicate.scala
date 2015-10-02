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


class BinaryPredicate (val op : String, val left : Expression, val right : Expression) extends AtomicPredicate
{
	private val predStr = op + " " + left.toString() + " " + right.toString()

	
	def getOperator() : String = 
	{
		return op
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return (left.contains(exprStr) || right.contains(exprStr))
	}
	
	def containsOperand(exprStr : String) : Boolean =
	{
		return (left.toString() == exprStr) || (right.toString() == exprStr)
	}          
	
	def containsFunction(funcName : String) : Boolean =
	{
		return (left.containsFunction(funcName) || right.containsFunction(funcName))
	}
	
	override def isAlwaysTrue() : Boolean =
	{
		if ((op == "=") && (left == right)) return true
		
		return false
	}
	
	override def isAlwaysFalse() : Boolean =
	{
		if (op == "=")
		{
			if (ExpressionUtils.isConstantValue(left) && ExpressionUtils.isConstantValue(right) && (left != right)) return true
		}
				
		return false
	}
	
	override def toString() : String = 
	{
		return predStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherPred : BinaryPredicate =>
				{
					// symmetric with respect to left and right operands
					if ((this.op == otherPred.op) && (this.left == otherPred.left) && (this.right == otherPred.right)) return true
					else if (((this.op == "=") || (this.op == "!=")) && (this.op == otherPred.op) && (this.left == otherPred.right) && (this.right == otherPred.left)) return true
					else return false
				}
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		// symmetric with respect to left and right operands
		(left.hashCode() + right.hashCode()) * 31 + op.hashCode()
	}		
}
