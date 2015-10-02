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


class Property (val formula : LogicFormula, val codeLocations : List[(String, Int)])
{
	private var propStr : String = null

	
	override def toString() : String = 
	{
		if (propStr != null) return propStr
		
		propStr = formula.toString()
		
		propStr = propStr + " : "
		
		var first = true
	
		for ( (mthName, bcPos) <- codeLocations)
		{
			if (first) first = false
			else propStr = propStr + ","

			propStr = propStr + "(" + mthName + "," + bcPos + ")"
		}

		return propStr
	}
	
	override def equals(other : Any) : Boolean =
	{
		other match 
		{
			case otherProp : Property => 
				 (this.formula == otherProp.formula) && (this.codeLocations == otherProp.codeLocations)
			case _ => false
		}
	}
	
	override def hashCode() : Int =
	{
		formula.hashCode() * 31 + codeLocations.hashCode()
	}
}
