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


class FunctionExpression (val name : String, val args : Array[Expression]) extends Expression (toString())
{
	private var feStr : String = null
	
	override def containsFunction(funcName : String) : Boolean =
	{
		if (this.name == funcName) return true
		
		for (arg <- args)
		{
			if (arg.containsFunction(funcName)) return true
		}
		
		return false
	}
	
	override def replace(oldExprStr : String, newExprStr : String) : Expression =
	{
		if (oldExprStr == toString()) return ExpressionUtils.createExprFromStr(newExprStr)
		
		
		val newArgs = new Array[Expression](args.length)
		
		for (i <- 0 to (args.length - 1))
		{
			newArgs(i) = args(i).replace(oldExprStr, newExprStr)					
		}
		
		return new FunctionExpression(name, newArgs)
	}

	override def toString() : String = 
	{
		if (feStr != null) return feStr
				
		feStr = name
	
		feStr = feStr + "("
		
		var first : Boolean = true
	
		for (arg <- args)
		{
			if (first) first = false
			else feStr = feStr + ","

			feStr = feStr + arg
		}
		
		feStr = feStr + ")"

		return feStr
	}

	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherFE : FunctionExpression => 
				{
					if (this.name != otherFE.name) return false
					
					if (this.args.length != otherFE.args.length) return false
					
					for (i <- 0 to (this.args.length - 1))
					{
						if (this.args(i) != otherFE.args(i)) return false
					}
					
					return true
				}
			case _ => false
		}
	}
	
	override def hashCode() : Int = 
	{
		var hc : Int = 0
		
		hc = hc + name.hashCode()
		
		for (i <- 0 to (this.args.length - 1))
		{
			hc = hc * 31 + this.args(i).hashCode()
		}
		
		return hc
	}
}
