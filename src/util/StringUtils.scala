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
package util


object StringUtils
{
	def extractPackageName(className : String) : String =
	{
		if (className.indexOf('.') != -1) return className.substring(0, className.lastIndexOf('.'))
		else return ""
	}
	
	def extractShortMethodName(fullMethodName : String) : String =
	{
		return fullMethodName.substring(fullMethodName.lastIndexOf('.') + 1)
	}

	def extractClassName(fullMethodName : String) : String =
	{
		return fullMethodName.substring(0, fullMethodName.lastIndexOf('.'))
	}

	def getInternalClassName(plainClassName : String) : String = 
	{
		return "L" + plainClassName.replace('.', '/') + ";"
	}
	
	def getPlainClassName(internalClassName : String) : String =
	{
		var curPos = 0
		
		// skip array dimensions
		while (internalClassName.charAt(curPos) == '[') curPos += 1
		
		if (internalClassName.charAt(curPos) == 'L') curPos += 1
		
		// omit the ";" character at the end
		val plainClassName = internalClassName.substring(curPos, internalClassName.length() - 1).replace('/', '.')
		
		return plainClassName
	}
	
	def getClassNameAsIndivisibleString(className : String) : String =
	{
		return className.replace('.', '_')	
	}
	
	def getPlainClassNameAsIndivisibleString(internalClassName : String) : String =
	{
		return getPlainClassName(internalClassName).replace('.', '_')	
	}

	def printStringList(prefix : String, strList : List[String]) = 
	{
		print(prefix)
		
		if (strList.size > 0)
		{
			print(strList.head)
		
			for (str <- strList.tail) print(", " + str)
		}
		
		println(" ")
	}
	
	def printStringSet(prefix : String, strSet : Set[String]) =
	{
		printStringList(prefix, strSet.toList)	
	}
	
	def findClosingBracket(inputStr : String, openBracketPos : Int) : Int =
	{
		var nestingLevel = 0
		
		var curPos = openBracketPos + 1
		
		while (curPos < inputStr.length())
		{
			val curSymbol = inputStr.charAt(curPos)
			
			if (curSymbol == '(') 
			{
				nestingLevel = nestingLevel + 1
			}
			
			if (curSymbol == ')')
			{
				if (nestingLevel == 0) return curPos
				
				nestingLevel = nestingLevel - 1
			}
			
			curPos = curPos + 1
		}
		
		// not found
		return -1
	}
	
	def getBracketedStringFromPos(inputStr : String, pos : Int) : String =
	{
		val openBracketPos = inputStr.indexOf('(', pos)
		if (openBracketPos == -1) return ""
		
		val closeBracketPos = findClosingBracket(inputStr, openBracketPos)
		if (closeBracketPos == -1) return ""
		
		return inputStr.substring(openBracketPos + 1, closeBracketPos)
	}
	
	def splitWithBrackets(inputStr : String, delim : Char) : List[String] =
	{
		var strList = List[String]()
		
		var nestingLevel = 0
		
		var curPos = 0
		
		var prevDelimPos = 0
		
		while (curPos < inputStr.length())
		{
			val curSymbol = inputStr.charAt(curPos)
			
			if (curSymbol == '(') 
			{
				nestingLevel = nestingLevel + 1
			}
			
			if (curSymbol == ')')
			{					
				nestingLevel = nestingLevel - 1
			}
			
			if (curSymbol == delim)
			{
				if (nestingLevel == 0)
				{
					strList = strList :+ inputStr.substring(prevDelimPos, curPos)
					
					prevDelimPos = curPos + 1
				}
			}
			
			curPos = curPos + 1
		}
		
		// last string
		if (nestingLevel == 0)
		{
			strList = strList :+ inputStr.substring(prevDelimPos)
		}
		
		return strList
	}

	def findArithmeticOperatorWithBrackets(inputStr : String) : Int =
	{
		var nestingLevel = 0
		
		var curPos = 0
		
		while (curPos < inputStr.length())
		{
			val curSymbol = inputStr.charAt(curPos)
			
			if (curSymbol == '(') 
			{
				nestingLevel = nestingLevel + 1
			}
			
			if (curSymbol == ')')
			{					
				nestingLevel = nestingLevel - 1
			}
			
			if ((curSymbol == '+') || (curSymbol == '-') || (curSymbol == '*') || (curSymbol == '/'))
			{
				if (nestingLevel == 0) return curPos
			}
			
			curPos = curPos + 1
		}

		return -1
	}
		
}
