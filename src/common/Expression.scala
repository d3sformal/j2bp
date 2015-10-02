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


// variable names and constants are represented by this class
class Expression (val valueStr : String)
{
	def contains(exprStr : String) : Boolean =
	{
		if (toString() == exprStr) return true
		
		for (delim1 <- ExpressionUtils.delimiters)
		{
			if ( toString().startsWith(exprStr + delim1) || toString().endsWith(delim1 + exprStr) ) return true

			for (delim2 <- ExpressionUtils.delimiters)
			{				
				if (toString().contains(delim1 + exprStr + delim2)) return true
			}
		}
		
		return false	
	}
	
	def containsFunction(funcName : String) : Boolean =
	{
		return false
	}
	
	def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		return ExpressionUtils.createExprFromStr(valueStr.replace(oldExprStr, newExprStr))
	}
	
	override def toString() : String = 
	{
		return valueStr
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherExpr : Expression => 
				 (this.toString() == otherExpr.toString())
			case _ => false
		}
	}
	
	override def hashCode() : Int = valueStr.hashCode()
}
