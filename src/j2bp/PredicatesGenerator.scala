package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod}
import com.ibm.wala.types.TypeReference

import common.Constants
import common.Expression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils


abstract class PredicatesGenerator
{
	/**
	 * This method returns tuple of maps: 
	 *  (1) from class name to predicates over static fields; 
	 *  (2) from class name to predicates over object fields;
	 *  (3) from full method name to predicates over local variables and method parameters.
	 */
	def inferPredicates(progClasses : List[String], mthvar2values : Map[(String, Expression), Set[Expression]]) : ( Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]] )
	
	
	protected def addPredicate(entityName : String, entity2predicates : Map[String, Set[AtomicPredicate]], pred : AtomicPredicate) =
	{
		var predSet = entity2predicates.getOrElse(entityName, Set[AtomicPredicate]())

		predSet = predSet + pred

		entity2predicates.put(entityName, predSet) 
	}

}
