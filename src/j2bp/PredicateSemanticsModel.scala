package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import common.AtomicPredicate
import common.LogicFormula
import common.Expression


abstract class PredicateSemanticsModel
{
	def addSupportFormula(form : LogicFormula)
	
	def getSupportFormulas() : Set[LogicFormula]
	
	def derivePermanentSupportingFormulas()
	
	def getTemporarySupportingFormulasResultSet(updatedPred : AtomicPredicate, predWP : LogicFormula, resdetPreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Set[LogicFormula]
	
	/**
	 * Returns supporting formulas and map from logic variables to matching expressions that are captured in the formulas.
	 */
	def getTemporarySupportingFormulasResultCube(updatedPred : AtomicPredicate, predWP : LogicFormula, resdetPreds : Set[AtomicPredicate], resdetCube : Set[LogicFormula], varnames2matchexprs : Map[Expression, Set[Expression]], curClassOrigName : String, curMethodName : String) : (Set[LogicFormula], Map[Expression,Set[Expression]])
	
	/**
	 * Returns tuples of literals conflicting with "headPred".
	 */
	def getConflictingLiterals(headPred : AtomicPredicate, resdetPredSet : Set[AtomicPredicate], weakPrecond : LogicFormula, curClassOrigName : String, curMethodName : String) : Set[Set[LogicFormula]]
	
	def isAmbiguousCube(resdetCube : Set[LogicFormula], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : Boolean

	/**
	 * For each temporary variable it returns indexes of predicates that make a given cube unambiguous.
	 */
	def getIndexesForUnambiguousPredicates(resdetPredList : List[AtomicPredicate], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : List[(String,List[Int])]
	
	def isIrrelevantCubeForAliasing(resdetCube : Set[LogicFormula], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : Boolean
	
	/**
	 * Returns indexes of aliasing predicates and corresponding relevant predicates.
	 */
	def getIndexesForAliasingRelevantPredicates(resdetPredList : List[AtomicPredicate], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : List[(Int,List[Int])]
	
	def isRelevantPredicateOverAliasedExpr(pred : AtomicPredicate, targetVarExpr : Expression, aliasedExpr : Expression) : Boolean
	
	def findMatchingExpressions(inputForm : LogicFormula, availablePreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Map[Expression, Set[Expression]]
	
	def clearAll()
}
