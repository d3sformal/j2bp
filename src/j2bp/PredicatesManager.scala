package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map

import common.Expression
import common.AtomicPredicate


abstract class PredicatesManager
{
	def loadGeneratedPredicates(static2preds : Map[String, Set[AtomicPredicate]], object2preds : Map[String, Set[AtomicPredicate]], method2preds : Map[String, Set[AtomicPredicate]])
	
	def loadPredicatesFromFile(fileName : String)
	
	def printAllPredicatesToConsole(prefix : String) 

	// returns list to keep same predicates associated with different methods
	def getAllPredicates() : List[AtomicPredicate]
	
	def countAllPredicates() : Int

	def countPredicatesOverStaticFields(className : String) : Int	

	def getPredicatesOverStaticFields(className : String) : Set[AtomicPredicate]

	def countPredicatesOverObjectFields(className : String) : Int
	
	def getPredicatesOverObjectFields(className : String) : Set[AtomicPredicate]	

	def countPredicatesOverMethodParams(className : String, methodName : String) : Int
	
	// ignore the bytecode index equal to -1 (we want all predicates in that case)
	def getPredicatesOverMethodVariables(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate]
	def getPredicatesOverMethodParams(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate]
	def getPredicatesOverMethodLocalVars(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate]
	
	def getAllPredicatesForCodeLocation(fullMethodName : String, bcPos : Int) : Set[AtomicPredicate]
	
	def isPredicateOverObjectFields(pred : AtomicPredicate) : Boolean
	def isPredicateOverMethodVariables(pred : AtomicPredicate) : Boolean
	
	// ignore the bytecode index equal to -1 (we want all predicates in that case)
	def existsPredicate(className : String, methodName : String, pred : AtomicPredicate, bcIndex : Int) : Boolean
	
	// ignore the bytecode index equal to -1 (we want all predicates in that case)
	def getPredicatesOverExpr(className : String, methodName : String, exprStr : String, bcIndex : Int) : Set[AtomicPredicate]
	
	// ignore the bytecode index equal to -1 (we want all predicates in that case)
	def getPredicatesWithOperand(className : String, methodName : String, operand : Expression, bcIndex : Int) : Set[AtomicPredicate]
	
	def getAliasingPredicates() : Set[AtomicPredicate]
	
	def getReservedVariableNames() : List[String]
	def getReservedFunctionSignatures() : List[(String,String,String)]
	def getReservedConstants() : List[(String,String)]
	
	def isReservedName(name : String) : Boolean
	
	def addMethodPredicate(className : String, methodName : String, pred : AtomicPredicate)	
	def addObjectPredicate(className : String, pred : AtomicPredicate)	
	def addStaticPredicate(className : String, pred : AtomicPredicate)
	
	def dropUnnecessaryPredicates(requiredPredicatesWithMethodName : List[(AtomicPredicate, String)], programClasses : List[String])
	
	def dropPredicatesWithNonlinearArithmetic()

	def defineScopeForPredicatesOverMethodVars(mthvar2bcindexes : Map[(String,String), List[(Int,Int)]])
	
	def recordMissingPredicate(pred : AtomicPredicate)	
	def getMissingPredicates() : Set[AtomicPredicate]
	def clearMissingPredicates()
	
	def invalidateCaches()
	
	def clearAll()
	
}
