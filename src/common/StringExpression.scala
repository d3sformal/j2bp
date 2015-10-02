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


// immutable string with unobservable internal structure
class StringExpression (val strValue : String) extends Expression (toString())
{
	override def contains(exprStr : String) : Boolean =
	{
		if (toString() == exprStr) return true
		else return false
	}
	
	override def containsFunction(funcName : String) : Boolean =
	{
		return false
	}
	
	override def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		if (oldExprStr == toString()) return new StringExpression(newExprStr)
		else return this
	}
	
	override def toString() : String = 
	{
		return strValue
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherSE : StringExpression => 
				 (this.toString() == otherSE.toString())
			case _ => false
		}
	}
	
	override def hashCode() : Int = strValue.hashCode()
}
