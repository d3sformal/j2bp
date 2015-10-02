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
package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map

import common.LogicFormula
import common.AtomicPredicate
import common.Property


abstract class PropertiesManager
{
	def loadGeneratedProperties(propList : List[Property])
	
	def loadPropertiesFromFile(fileName : String)
	
	def getAllProperties() : List[Property]
	
	def getPropertiesForCodeLocation(inputProps : List[Property], fullMethodName : String, bcIndex : Int) : List[LogicFormula]
	
	def getAtomicPredicatesWithMethodName(inputProps : List[Property]) : List[(AtomicPredicate, String)]
		
	def printAllPropertiesToConsole(prefix : String)
	
}
