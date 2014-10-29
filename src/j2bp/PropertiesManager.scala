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
