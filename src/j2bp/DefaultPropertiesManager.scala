package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.io.Source._

import common._

import util.StringUtils


class DefaultPropertiesManager extends PropertiesManager
{
	protected var properties = List[Property]()

	
	def loadGeneratedProperties(propList : List[Property]) =
	{
		properties = properties ++ propList
	}
	
	def loadPropertiesFromFile(fileName : String) =
	{
		var lines = fromFile(fileName).getLines()

		for (line <- lines if line.trim().length() > 0)
		{
			if (Main.DEBUG) println("[DEBUG DefaultPropertiesManager.loadFromFile] line = " + line)
			
			val lineParts = line.split(";")
			
			val formOpt = FormulaUtils.parseFromStr(lineParts(0))
			
			if (formOpt != None) 
			{
				val form = formOpt.get
				
				var codeLocations = List[(String, Int)]()
				
				for ( i <- 1 to (lineParts.length - 1) )
				{
					val codeLocStr = lineParts(i).split(":")
					
					val fullMethodName = codeLocStr(0)
					val bytecodePos = java.lang.Integer.parseInt(codeLocStr(1))
					
					codeLocations = codeLocations :+ (fullMethodName, bytecodePos)
				}
				
				val newProp = new Property(form, codeLocations)
				
				properties = properties :+ newProp				
			}
		}
	}
	
	def getAllProperties() : List[Property] =
	{
		return properties
	}
	
	def getPropertiesForCodeLocation(inputProps : List[Property], fullMethodName : String, bcIndex : Int) : List[LogicFormula] =
	{
		var propsCodeLoc = List[LogicFormula]()
		
		for (prop <- inputProps)
		{
			if ( prop.codeLocations.contains( (fullMethodName, bcIndex) ) ) propsCodeLoc = propsCodeLoc :+ prop.formula			
		}
		
		return propsCodeLoc
	}
	
	def getAtomicPredicatesWithMethodName(inputProps : List[Property]) : List[(AtomicPredicate, String)] =
	{
		var propAtomPredsMethods = List[(AtomicPredicate, String)]()
		
		for (prop <- inputProps)
		{
			for (atomPred <- FormulaUtils.extractAtomicPredicates(prop.formula))
			{
				for (codeLoc <- prop.codeLocations)
				{
					propAtomPredsMethods = propAtomPredsMethods :+ (atomPred, codeLoc._1)
				}
			}
		}

		return propAtomPredsMethods
	}
	
	def printAllPropertiesToConsole(prefix : String) = 
	{
		for ( prop <- properties )
		{
			println("\t predicate: " + prop.formula.toString())

			for ( (mthName, bcPos) <- prop.codeLocations ) println(prefix + "\t" + mthName + ":" + bcPos)	
		}		
	}	
}
