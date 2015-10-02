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
