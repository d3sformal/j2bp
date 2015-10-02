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
import scala.collection.mutable.HashMap
import scala.util.matching.Regex

import common.LogicFormula


abstract class SMTProvider
{
	/**
	 * This method checks validity of the formula "(resdetFormulas[1] and ... and resdetFormulas[N]) => weakPrecond".
	 */
	def checkValidity(ctx : AbstractionContext, weakPrecond : LogicFormula, resdetCube : Set[LogicFormula]) : Boolean

	
	def resetCounters()

	/**
	 * This method can return a tuple if multiple counters are needed.
	 */
	def getCountersValue() : Long	
}
