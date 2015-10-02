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


class UnaryPredicate (val op : String, val arg : Expression) extends AtomicPredicate
{
	private var predStr : String = null
	
	
	def getOperator() : String = 
	{
		return op
	}
	
	def containsExpression(exprStr : String) : Boolean =
	{
		return arg.contains(exprStr)
	}
	
	def containsOperand(exprStr : String) : Boolean =
	{
		return (arg.toString() == exprStr)
	}
	
	def containsFunction(funcName : String) : Boolean =
	{
		return arg.containsFunction(funcName)
	}
	
	override def toString() : String = 
	{
		if (predStr != null) return predStr
		
		predStr = ""
	
		if (op == "") predStr = arg.toString()
		else predStr = op + " " + arg.toString()
		
		return predStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherPred : UnaryPredicate => 
				 (this.op == otherPred.op) && (this.arg == otherPred.arg)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		arg.hashCode() * 31 + op.hashCode()
	}
}
