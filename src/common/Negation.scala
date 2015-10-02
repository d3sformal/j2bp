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
