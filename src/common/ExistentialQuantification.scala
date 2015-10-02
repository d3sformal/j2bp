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
