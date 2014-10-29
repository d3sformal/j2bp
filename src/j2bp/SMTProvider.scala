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
