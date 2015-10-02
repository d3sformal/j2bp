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
package j2bp.containers

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.matching.Regex

import org.objectweb.asm.MethodVisitor

import common.Constants
import common.Expression
import common.StringExpression
import common.AtomicPredicate
import common.BinaryPredicate
import common.FormulaUtils
import common.ExpressionUtils

import j2bp.Configuration


object BasicContainerModel 
{
	def isSupportedClass(className : String) : Boolean =
	{
		if ( ! className.contains("java.util") ) return false
		
		if (className.contains("Map")) return true
		if (className.contains("Set")) return true
		if (className.contains("List")) return true
		if (className.contains("Collection")) return true
		
		if (className.contains("Iterator") || className.contains("Itr")) return true
		
		return false
	}

	def isSupportedCollectionClass(className : String) : Boolean =
	{
		if ( ! className.contains("java.util") ) return false
		
		if (className.contains("Map")) return true
		if (className.contains("Set")) return true
		if (className.contains("List")) return true
		if (className.contains("Collection")) return true

		return false
	}
	
	def isSupportedListClass(className : String) : Boolean =
	{
		if ( ! className.contains("java.util") ) return false
		
		if (className.contains("List")) return true
		
		return false
	}
	
	def isSupportedMethod(className : String, methodName : String) : Boolean =
	{
		if ( ! isSupportedClass(className) ) return false
		
		if (methodName == "<init>") return true
		
		if (className.contains("Map"))
		{
			methodName match
			{
				case "put" => return true
				case "get" => return true
				case "remove" => return true
				case "clear" => return true
				case "containsKey" => return true
				case "containsValue" => return true
				case "size" => return true
				case "keySet" => return true
				case "values" => return true
				case "iterator" => return true
				case _ => return false
			}			
		}
		
		if (className.contains("Iterator") || className.contains("Itr"))
		{
			methodName match
			{
				case "hasNext" => return true
				case "next" => return true
				case "remove" => return true
				case _ => return false
			}			
		}
		
		if (className.contains("Set"))
		{
			methodName match
			{
				case "add" => return true
				case "remove" => return true
				case "clear" => return true
				case "contains" => return true
				case "size" => return true
				case "iterator" => return true
				case _ => return false
			}			
		}

		if (className.contains("List"))
		{
			methodName match
			{
				case "add" => return true
				case "get" => return true
				case "set" => return true
				case "remove" => return true
				case "clear" => return true
				case "contains" => return true
				case "size" => return true
				case "iterator" => return true
				case _ => return false
			}			
		}
		
		if (className.contains("Collection"))
		{
			methodName match
			{
				case "add" => return true
				case "remove" => return true
				case "clear" => return true
				case "contains" => return true
				case "size" => return true
				case "iterator" => return true
				case _ => return false
			}			
		}
		
		return false
	}
	
	def hasMethodReturnValue(className : String, methodName : String) : Boolean =
	{
		if ( ! isSupportedMethod(className, methodName) ) return false

		if (methodName == "<init>") return false
		
		// all supported methods of Java container classes have return value (except constructors)
		return true
	}
		
	def hasMethodUnusedReturnValue(className : String, methodName : String) : Boolean =
	{
		if ( ! isSupportedClass(className) ) return false
		
		if (className.contains("Map"))
		{
			if ((methodName == "put") || (methodName == "remove")) return true
		}
		if (className.contains("Set"))
		{
			if ((methodName == "add") || (methodName == "remove")) return true
		}
		if (className.contains("List"))
		{
			if ((methodName == "add") || (methodName == "remove") || (methodName == "set")) return true
		}
		if (className.contains("Collection"))
		{
			if ((methodName == "add") || (methodName == "remove")) return true
		}
		
		return false
	}
	
	
	// constants for names of function symbols
	val FUNC_MAP_GET = "mget"
	val FUNC_MAP_SIZE = "msize"
	val FUNC_MAP_ORDER = "morder"
	val FUNC_MAP_KEYS = "mkeys"
	val FUNC_MAP_VALUES = "mvalues"
	
	val FUNC_ARRAY_MAP = "map"
	val FUNC_ARRAY_SIZE = "msz"
	val FUNC_ARRAY_ITER = "mit"
	val FUNC_ARRAY_KEYS = "mks"
	val FUNC_ARRAY_VALUES = "mvs"
	
	val BOTTOM_STR = "bot"
	val BOTTOM_EXPR = new Expression("bot")
	
	val VAR_LOGIC_ANY : String = common.Constants.LOGIC_VAR_PREFIX + "a"
	val VAR_LOGIC_MAP : String = common.Constants.LOGIC_VAR_PREFIX + "m"	
	val VAR_LOGIC_KEY : String = common.Constants.LOGIC_VAR_PREFIX + "k"
	
	val LOGIC_ANY_EXPR = new Expression(VAR_LOGIC_ANY)
	val LOGIC_MAP_EXPR = new Expression(VAR_LOGIC_MAP)
	val LOGIC_KEY_EXPR = new Expression(VAR_LOGIC_KEY)
	
	val VAR_QUANT_KEY : String = common.Constants.QUANT_VAR_PREFIX + "k"
	
	val QUANT_KEY_EXPR = new Expression(VAR_QUANT_KEY)
		
	val VAR_TMP_ITER : String = common.Constants.TEMP_VAR_PREFIX + "it"
	val VAR_TMP_KEY : String = common.Constants.TEMP_VAR_PREFIX + "k"
	val VAR_TMP_LOCAL : String = common.Constants.TEMP_VAR_PREFIX + "l"
	val VAR_TMP_INDEX : String = common.Constants.TEMP_VAR_PREFIX + "i"
	val VAR_TMP_VALUE : String = common.Constants.TEMP_VAR_PREFIX + "v"
	val VAR_TMP_BOOL : String = common.Constants.TEMP_VAR_PREFIX + "b"

	val TMP_ITER_EXPR = new Expression(VAR_TMP_ITER)
	val TMP_KEY_EXPR = new Expression(VAR_TMP_KEY)
	val TMP_LOCAL_EXPR = new Expression(VAR_TMP_LOCAL)
	val TMP_INDEX_EXPR = new Expression(VAR_TMP_INDEX)
	val TMP_VALUE_EXPR = new Expression(VAR_TMP_VALUE)
	val TMP_BOOL_EXPR = new Expression(VAR_TMP_BOOL)
	
	
	// maximal possible container size
	var maxSize = 1


	def isSpecialArrayName(exprStr : String) : Boolean =
	{
		if ((exprStr == FUNC_ARRAY_MAP) || (exprStr == FUNC_ARRAY_SIZE) || (exprStr == FUNC_ARRAY_ITER) || (exprStr == FUNC_ARRAY_KEYS) || (exprStr == FUNC_ARRAY_VALUES)) return true
		else return false
	}

	
	// this method returns a tuple that contains operation name, target variable (receiver), array with symbolic arguments, and array with names of variables used to store return values
	// it implements the mapping from methods of Java collection classes (Map, List, Set) to corresponding operations on the abstract map container
	def getAbstractOperationSeq(className : String, methodName : String, targetObj : Expression, argValues : Array[Expression], retVarExpr : Expression, callingClassName : String, callingMethodName : String, bcIndex : Int) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		if (className.contains("Map"))
		{
			if (methodName.equals("put")) 
			{
				return getAbstractOperationSeqForPut(className, targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
			
			if (methodName.equals("get")) return List( ("get", targetObj, Array(argValues(0)), Array(retVarExpr)) )
			if (methodName.equals("containsKey")) return List( ("containsKey", targetObj, Array(argValues(0)), Array(retVarExpr)) )
			if (methodName.equals("containsValue")) return List( ("containsValue", targetObj, Array(argValues(0)), Array(retVarExpr)) )
			if (methodName.equals("size")) return List( ("size", targetObj, new Array(0), Array(retVarExpr)) )
			if (methodName.equals("keySet")) return List( ("keysView", targetObj, new Array(0), Array(retVarExpr)) )
			if (methodName.equals("values")) return List( ("valuesView", targetObj, new Array(0), Array(retVarExpr)) )
			
			if (methodName.equals("remove"))
			{
				return getAbstractOperationSeqForRemove(className, targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
			
			if (methodName.equals("clear")) 
			{
				return getAbstractOperationSeqForClear(targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
		}
		
		if (className.contains("Iterator") || className.contains("Itr"))
		{
			if (methodName.equals("hasNext")) return List( ("hasMore", targetObj, new Array(0), Array(retVarExpr)) )
			
			if (methodName.equals("next")) 
			{
				if (ContainerAbstractionData.isIteratorForList(callingClassName, callingMethodName, targetObj))
				{
					// we have iterator over list -> the getNext operations is translated into the sequence "it.moveNext(); i = it.getCurrent(); v = m.get(i)"
					
					// we must get the map associated with the iterator variable
					val mapObj = ContainerAbstractionData.getMapsForIterator(callingClassName, callingMethodName, targetObj).head
					
					return List( ("moveNext", targetObj, new Array(0), new Array(0)), ("getCurrent", targetObj, new Array(0), Array(TMP_KEY_EXPR)), ("get", mapObj, Array(TMP_KEY_EXPR), Array(retVarExpr)) )
				}
				else
				{
					return List( ("moveNext", targetObj, new Array(0), new Array(0)), ("getCurrent", targetObj, new Array(0), Array(retVarExpr)) )
				}
			}
			
			if (methodName.equals("remove"))
			{
				// we must get the map associated with the iterator variable
				val mapObj = ContainerAbstractionData.getMapsForIterator(callingClassName, callingMethodName, targetObj).head
					
				var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]( ("getCurrent", targetObj, new Array(0), Array(TMP_KEY_EXPR)) ) 
				
				resSeq = resSeq ++ getAbstractOperationSeqForRemove("Map", mapObj, Array(TMP_KEY_EXPR), null, callingClassName, callingMethodName, bcIndex)
				
				return resSeq
			}
		}
		
		if (className.contains("Set"))
		{
			if (methodName.equals("add"))
			{
				if (className.contains("TreeSet"))
				{
					return getAbstractOperationSeqForAddIntoSortedMap(targetObj, argValues(0), Constants.ONE_EXPR)
				}
				else
				{
					return List( ("put", targetObj, Array(argValues(0), Constants.ONE_EXPR), new Array(0)) )
				}
			}
			
			if (methodName.equals("contains")) return List( ("containsKey", targetObj, Array(argValues(0)), Array(retVarExpr)) )
			if (methodName.equals("size")) return List( ("size", targetObj, new Array(0), Array(retVarExpr)) )
			if (methodName.equals("iterator")) return List( ("createIterator", targetObj, new Array(0), Array(retVarExpr)) )

			if (methodName.equals("clear")) 
			{
				return getAbstractOperationSeqForClear(targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
			
			if (methodName.equals("remove"))
			{
				return getAbstractOperationSeqForRemove(className, targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
		}

		if (className.contains("List") || className.contains("Collection"))
		{
			if (methodName.equals("get")) 
			{
				var resSeq = getFindKeySequence(targetObj, argValues(0), TMP_KEY_EXPR)
				
				resSeq = resSeq :+ ("get", targetObj, Array(TMP_KEY_EXPR), Array(retVarExpr))

				return resSeq				
			}
			
			if (methodName.equals("add") && (argValues.length == 1))
			{
				var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]()
				
				// code that finds unused slot
				resSeq = resSeq ++ getFindUnusedSlotSequence(targetObj)
				
				// addition
				resSeq = resSeq :+ ("put", targetObj, Array(TMP_KEY_EXPR, argValues(0)), new Array[Expression](0))
				
				return resSeq
			}
			
			if (methodName.equals("add") && (argValues.length == 2))
			{
				var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]()
				
				// find key and store into "tmpl"				
				resSeq = resSeq ++ getFindKeySequence(targetObj, argValues(0), TMP_LOCAL_EXPR)
								
				// code that finds unused slot into "tmpk"
				resSeq = resSeq ++ getFindUnusedSlotSequence(targetObj)

				// addition
				resSeq = resSeq :+ ("putAhead", targetObj, Array(TMP_KEY_EXPR, argValues(1), TMP_LOCAL_EXPR), new Array[Expression](0))
				
				return resSeq
			}
			
			if (methodName.equals("remove"))
			{
				return getAbstractOperationSeqForRemove(className, targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}
			
			if (methodName.equals("set") && (argValues.length == 2))
			{
				val findkeySeq = getFindKeySequence(targetObj, argValues(0), TMP_KEY_EXPR)

				val normaladdLabelName = ContainerAbstractionData.getUniqueLabelName("normaladd")
				val doneLabelName = ContainerAbstractionData.getUniqueLabelName("done")
				
				val resSeq = findkeySeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("hasMore", TMP_ITER_EXPR, new Array(0), Array(TMP_BOOL_EXPR)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_BOOL_EXPR, Constants.ZERO_EXPR, new StringExpression(normaladdLabelName)), new Array(0)), ("moveNext", TMP_ITER_EXPR, new Array(0), new Array(0)), ("getCurrent", TMP_ITER_EXPR, new Array(0), Array(TMP_LOCAL_EXPR)), ("remove", targetObj, Array(TMP_KEY_EXPR), new Array(0)), ("putAhead", targetObj, Array(TMP_KEY_EXPR, argValues(1), TMP_LOCAL_EXPR), new Array(0)), ("goto", Constants.EMPTY_STR_EXPR, Array(new StringExpression(doneLabelName)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(normaladdLabelName)), new Array(0)), ("remove", targetObj, Array(TMP_KEY_EXPR), new Array(0)), ("put", targetObj, Array(TMP_KEY_EXPR, argValues(1)), new Array[Expression](0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(doneLabelName)), new Array(0)) )
				
				return resSeq
			}

			if (methodName.equals("contains")) return List( ("containsValue", targetObj, Array(argValues(0)), Array(retVarExpr)) )
			if (methodName.equals("size")) return List( ("size", targetObj, new Array(0), Array(retVarExpr)) )
			
			if (methodName.equals("clear")) 
			{
				return getAbstractOperationSeqForClear(targetObj, argValues, retVarExpr, callingClassName, callingMethodName, bcIndex)
			}

			if (methodName.equals("iterator")) 
			{
				ContainerAbstractionData.saveIterForList(callingClassName, callingMethodName, retVarExpr)
				
				return List( ("createIterator", targetObj, new Array(0), Array(retVarExpr)) )
			}
		}
		
		return List()
	}
	
	private def getAbstractOperationSeqForClear(targetObj : Expression, argValues : Array[Expression], retVarExpr : Expression, callingClassName : String, callingMethodName : String, bcIndex : Int) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]( ("clear", targetObj, new Array(0), new Array(0)) )
		
		val mapObjPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, targetObj.toString(), bcIndex)
		
		for (moPred <- mapObjPreds)
		{	
			if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_KEYS))
			{
				val pairMapView = ContainerPredicateUtils.extractMapKeysViewPair(moPred)
				
				val m = pairMapView._1
				val ms = pairMapView._2
				
				if (m == targetObj)
				{							
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("clear", ms, new Array(0), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
				else if (ms == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("clear", m, new Array(0), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
			}
			else if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_VALUES))
			{
				val pairMapView = ContainerPredicateUtils.extractMapValuesViewPair(moPred)
				
				val m = pairMapView._1
				val ml = pairMapView._2
				
				if (m == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("clear", ml, new Array(0), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
				else if (ml == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("clear", m, new Array(0), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
			}
		}
		
		return resSeq		
	}
	
	private def getAbstractOperationSeqForRemove(className : String, targetObj : Expression, argValues : Array[Expression], retVarExpr : Expression, callingClassName : String, callingMethodName : String, bcIndex : Int) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]()
		
		if (className.contains("List") || className.contains("Collection"))
		{
			val findkeySeq = getFindKeySequence(targetObj, argValues(0), TMP_KEY_EXPR)
			
			resSeq = resSeq ++ findkeySeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("remove", targetObj, Array(TMP_KEY_EXPR), new Array(0)) )
		}
		else
		{
			resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("remove", targetObj, Array(argValues(0)), new Array(0)) )
		}				
		
		val mapObjPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, targetObj.toString(), bcIndex)
		
		for (moPred <- mapObjPreds)
		{	
			if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_KEYS))
			{
				val pairMapView = ContainerPredicateUtils.extractMapKeysViewPair(moPred)
				
				val m = pairMapView._1
				val ms = pairMapView._2
			
				if (m == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("remove", ms, Array(argValues(0)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
				else if (ms == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("remove", m, Array(argValues(0)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
			}
			else if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_VALUES))
			{
				val pairMapView = ContainerPredicateUtils.extractMapValuesViewPair(moPred)
				
				val m = pairMapView._1
				val ml = pairMapView._2
			
				if (m == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("get", targetObj, Array(argValues(0)), Array(TMP_VALUE_EXPR)), ("findKey", ml, Array(TMP_VALUE_EXPR), Array(TMP_LOCAL_EXPR)) )
					
					resSeq = resSeq ++ getFindKeySequence(ml, TMP_LOCAL_EXPR, TMP_KEY_EXPR)
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("remove", ml, Array(TMP_KEY_EXPR), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
				else if (ml == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)), ("get", targetObj, Array(argValues(0)), Array(TMP_VALUE_EXPR)), ("findKey", m, Array(TMP_VALUE_EXPR), Array(TMP_LOCAL_EXPR)), ("remove", m, Array(TMP_LOCAL_EXPR), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )
				}
			}
		}
		
		return resSeq		
	}
	
	private def getAbstractOperationSeqForPut(className : String, targetObj : Expression, argValues : Array[Expression], retVarExpr : Expression, callingClassName : String, callingMethodName : String, bcIndex : Int) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]()
		
		// target variable always points to some map (i.e., className contains "Map"), because addition to views for map is not possible
		
		if (className.contains("TreeMap"))
		{
			resSeq = resSeq ++ getAbstractOperationSeqForAddIntoSortedMap(targetObj, argValues(0), argValues(1))
		}
		else
		{
			resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("put", targetObj, Array(argValues(0), argValues(1)), new Array(0)) )
		}		
		
		val mapObjPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, targetObj.toString(), bcIndex)
		
		for (moPred <- mapObjPreds)
		{	
			if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_KEYS))
			{
				val pairMapView = ContainerPredicateUtils.extractMapKeysViewPair(moPred)
				
				val m = pairMapView._1
				val ms = pairMapView._2
			
				if (m == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)) )

					if (className.contains("TreeMap"))
					{
						// we must add the element at the right place in the sorted set that represents key view								
						resSeq = resSeq ++ getAbstractOperationSeqForAddIntoSortedMap(ms, argValues(0), Constants.ONE_EXPR)
					}
					else
					{
						resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("put", ms, Array(argValues(0), Constants.ONE_EXPR), new Array(0)) )
					}
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )													
				}
			}
			else if (moPred.containsFunction(BasicContainerModel.FUNC_MAP_VALUES))
			{
				val pairMapView = ContainerPredicateUtils.extractMapValuesViewPair(moPred)
				
				val m = pairMapView._1
				val ml = pairMapView._2
			
				if (m == targetObj)
				{
					val nextLabelName = ContainerAbstractionData.getUniqueLabelName("next")

					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("ifpred", Constants.EMPTY_STR_EXPR, Array(new StringExpression("not ("+moPred.toString()+")"), new StringExpression(nextLabelName)), new Array(0)) )

					if (className.contains("TreeMap"))
					{
						// we must add the element at the right place in the list that represents value view
						// we must compute the proper index and then generate code for List.add(i,o)
					
						val headLabelName = ContainerAbstractionData.getUniqueLabelName("head")
						val endLabelName = ContainerAbstractionData.getUniqueLabelName("end")

						// compute proper index by traversing over keys and increment from 0, and store the result into VAR_TMP_INDEX
						// the given key must be in the map because it was added by "put" above
						resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("assign", Constants.EMPTY_STR_EXPR, Array(TMP_INDEX_EXPR, Constants.ZERO_EXPR), new Array(0)), ("createIterator", targetObj, new Array(0), Array(TMP_ITER_EXPR)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("moveNext", TMP_ITER_EXPR, new Array(0), new Array(0)), ("getCurrent", TMP_ITER_EXPR, new Array(0), Array(TMP_KEY_EXPR)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_KEY_EXPR, argValues(0), new StringExpression(endLabelName)), new Array(0)), ("increment", Constants.EMPTY_STR_EXPR, Array(TMP_INDEX_EXPR), Array(TMP_INDEX_EXPR)), ("goto", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(endLabelName)), new Array(0)) )

					
						// code for add(i,o)
						
						// find key that corresponds to index and store the key into "tmpl"
						resSeq = resSeq ++ getFindKeySequence(ml, TMP_INDEX_EXPR, TMP_LOCAL_EXPR)

						// code that finds unused slot into "tmpk"
						resSeq = resSeq ++ getFindUnusedSlotSequence(ml)
						
						// addition
						resSeq = resSeq :+ ("putAhead", ml, Array(TMP_KEY_EXPR, argValues(0), TMP_LOCAL_EXPR), new Array[Expression](0))
					}
					else
					{
						// code for add(o)
		
						// code that finds unused slot
						resSeq = resSeq ++ getFindUnusedSlotSequence(ml)
						
						// addition
						resSeq = resSeq :+ ("put", ml, Array(TMP_KEY_EXPR, argValues(0)), new Array[Expression](0))
					}
					
					resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(nextLabelName)), new Array(0)) )							
				}
			}
		}
		
		return resSeq		
	}
	
	private def getAbstractOperationSeqForAddIntoSortedMap(targetObj : Expression, keyArg : Expression, valueArg : Expression) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		var resSeq = List[(String, Expression, Array[Expression], Array[Expression])]()
	
		val headLabelName = ContainerAbstractionData.getUniqueLabelName("head")
		val endLabelName = ContainerAbstractionData.getUniqueLabelName("end")
		val loopexitLabelName = ContainerAbstractionData.getUniqueLabelName("loopex")
		val normalLabelName = ContainerAbstractionData.getUniqueLabelName("norm")
		
		resSeq = resSeq ++ List[(String, Expression, Array[Expression], Array[Expression])]( ("createIterator", targetObj, new Array(0), Array(TMP_ITER_EXPR)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("hasMore", TMP_ITER_EXPR, new Array(0), Array(TMP_BOOL_EXPR)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_BOOL_EXPR, Constants.ZERO_EXPR, new StringExpression(loopexitLabelName)), new Array(0)), ("moveNext", TMP_ITER_EXPR, new Array(0), new Array(0)), ("getCurrent", TMP_ITER_EXPR, new Array(0), Array(TMP_KEY_EXPR)), ("ifgt", Constants.EMPTY_STR_EXPR, Array(TMP_KEY_EXPR, keyArg, new StringExpression(loopexitLabelName)), new Array(0)), ("goto", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(loopexitLabelName)), new Array(0)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_BOOL_EXPR, Constants.ZERO_EXPR, new StringExpression(normalLabelName)), new Array(0)), ("putAhead", targetObj, Array(keyArg, valueArg, TMP_KEY_EXPR), new Array(0)), ("goto", Constants.EMPTY_STR_EXPR, Array(new StringExpression(endLabelName)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(normalLabelName)), new Array(0)), ("put", targetObj, Array(keyArg, valueArg), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(endLabelName)), new Array(0)) )
		
		return resSeq
	}			
	
	private def getFindKeySequence(targetObj : Expression, idxArg : Expression, retKeyVarExpr : Expression) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		val headLabelName = ContainerAbstractionData.getUniqueLabelName("head")
		val loopexitLabelName = ContainerAbstractionData.getUniqueLabelName("loopex")
		val endLabelName = ContainerAbstractionData.getUniqueLabelName("end")

		val fkSeq = List[(String, Expression, Array[Expression], Array[Expression])]( ("assign", Constants.EMPTY_STR_EXPR, Array(retKeyVarExpr, Constants.ZERO_EXPR), new Array(0)), ("assign", Constants.EMPTY_STR_EXPR, Array(TMP_INDEX_EXPR, idxArg), new Array(0)), ("createIterator", targetObj, new Array(0), Array(TMP_ITER_EXPR)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_INDEX_EXPR, Constants.ZERO_EXPR, new StringExpression(loopexitLabelName)), new Array(0)), ("hasMore", TMP_ITER_EXPR, new Array(0), Array(TMP_BOOL_EXPR)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_BOOL_EXPR, Constants.ZERO_EXPR, new StringExpression(endLabelName)), new Array(0)), ("moveNext", TMP_ITER_EXPR, new Array(0), new Array(0)), ("decrement", Constants.EMPTY_STR_EXPR, Array(TMP_INDEX_EXPR), Array(TMP_INDEX_EXPR)), ("goto", Constants.EMPTY_STR_EXPR, Array(new StringExpression(headLabelName)), new Array(0)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(loopexitLabelName)), new Array(0)), ("hasMore", TMP_ITER_EXPR, new Array(0), Array(TMP_BOOL_EXPR)), ("ifeq", Constants.EMPTY_STR_EXPR, Array(TMP_BOOL_EXPR, Constants.ZERO_EXPR, new StringExpression(endLabelName)), new Array(0)), ("moveNext", TMP_ITER_EXPR, new Array(0), new Array(0)), ("getCurrent", TMP_ITER_EXPR, new Array(0), Array(retKeyVarExpr)), ("label", Constants.EMPTY_STR_EXPR, Array(new StringExpression(endLabelName)), new Array(0)) )
		
		return fkSeq 
	}
	
	private def getFindUnusedSlotSequence(targetObj : Expression) : List[(String, Expression, Array[Expression], Array[Expression])] =
	{
		var fusSeq = List[(String, Expression, Array[Expression], Array[Expression])]()

		val endSlotLabelName = ContainerAbstractionData.getUniqueLabelName("ends")

		for (i <- 1 to maxSize)
		{
			val predStr = "not (= " + ContainerExpressionUtils.createMapGetExprStr(targetObj, new Expression(String.valueOf(i))) + " " + BOTTOM_STR + ")"
		
			val nextSlotLabelName = ContainerAbstractionData.getUniqueLabelName("next")

			fusSeq = fusSeq :+ ("ifpred", Constants.EMPTY_STR_EXPR, Array[Expression](new StringExpression(predStr), new StringExpression(nextSlotLabelName)), new Array[Expression](0))
			fusSeq = fusSeq :+ ("assign", Constants.EMPTY_STR_EXPR, Array[Expression](TMP_KEY_EXPR, new Expression(String.valueOf(i))), new Array[Expression](0))
			fusSeq = fusSeq :+ ("goto", Constants.EMPTY_STR_EXPR, Array[Expression](new StringExpression(endSlotLabelName)), new Array[Expression](0))
			fusSeq = fusSeq :+ ("label", Constants.EMPTY_STR_EXPR, Array[Expression](new StringExpression(nextSlotLabelName)), new Array[Expression](0))
		}
		
		fusSeq = fusSeq :+ ("label", Constants.EMPTY_STR_EXPR, Array[Expression](new StringExpression(endSlotLabelName)), new Array[Expression](0))
		
		return fusSeq
	}

	
	// returns list of expressions such that each predicate whose truth value may be updated must contain at least one of them
	def getExpressionsToBeUpdated(absOperName : String, absArgValues : Array[Expression], absRetVarExprs : Array[Expression], absTargetObj : Expression, callingClassName : String, callingMethodName : String, bcIndex : Int) : Set[Expression] =
	{
		var exprSet = Set[Expression]()

		if ( (absOperName == "get") || (absOperName == "containsKey") || (absOperName == "containsValue") || (absOperName == "findKey") || (absOperName == "size") || (absOperName == "createIterator") || (absOperName == "keysView") || (absOperName == "valuesView") || (absOperName == "hasMore") || (absOperName == "getCurrent") ) 
		{
			exprSet = exprSet + absRetVarExprs(0)
		}

		if ( (absOperName == "put") || (absOperName == "putAhead") )
		{
			exprSet = exprSet + ContainerExpressionUtils.createMapSizeExpr(absTargetObj)

			
			val mgetExprStrFragment = ContainerExpressionUtils.createMapGetExprStrFragment(absTargetObj)
			
			val mgetPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, mgetExprStrFragment, bcIndex)
			
			for (mgetPred <- mgetPreds)
			{
				val mgetFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(mgetPred, mgetExprStrFragment) 
				
				for (mgetFE <- mgetFuncExprs)
				{
					val keyExpr = mgetFE.args(2)
				
					// consider predicates where 'keyExpr' is the argument or some constant value (if the key argument is some variable expression)
					if ( (keyExpr == absArgValues(0)) || ( ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(keyExpr) ) )
					{
						exprSet = exprSet + ContainerExpressionUtils.createMapGetExpr(absTargetObj, keyExpr)
					}
				}
			}
			
			
			var morderExprStrFragment = ContainerExpressionUtils.createMapOrderExprStrFragment(absTargetObj)
			
			val morderPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, morderExprStrFragment, bcIndex)

			for (morderPred <- morderPreds)
			{
				val morderFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(morderPred, morderExprStrFragment) 
				
				for (morderFE <- morderFuncExprs)
				{
					val posExpr1 = morderFE.args(2)
					val posExpr2 = morderFE.args(3)
				
					// omit predicates that contain neither the key (the argument or some integer constant if the key argument is a variable expression) nor the bottom symbol
					if ( (posExpr1 == absArgValues(0)) || ( ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(posExpr1) ) || (posExpr2 == absArgValues(0)) || ( ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(posExpr2) ) || (posExpr2 == BOTTOM_EXPR) )
					{
						exprSet = exprSet + ContainerExpressionUtils.createMapOrderExpr(absTargetObj, posExpr1, posExpr2)
					}
				}
			}
		}
		
		if (absOperName == "putAhead")
		{
			var morderExprStrFragment = ContainerExpressionUtils.createMapOrderExprStrFragment(absTargetObj)
		
			val morderPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, morderExprStrFragment, bcIndex)

			for (morderPred <- morderPreds)
			{
				val morderFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(morderPred, morderExprStrFragment) 
				
				for (morderFE <- morderFuncExprs)
				{
					val posExpr1 = morderFE.args(2)
					val posExpr2 = morderFE.args(3)
				
					if ( (((posExpr1 == absArgValues(0)) || (ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(posExpr1))) && ((posExpr2 == absArgValues(2)) || (ExpressionUtils.isVariableExpression(absArgValues(2)) && ExpressionUtils.isConstantExpression(posExpr2)))) || (((posExpr1 == absArgValues(2)) || (ExpressionUtils.isVariableExpression(absArgValues(2)) && ExpressionUtils.isConstantExpression(posExpr1))) && ((posExpr2 == absArgValues(0)) || (ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(posExpr2)))) )
					{
						exprSet = exprSet + ContainerExpressionUtils.createMapOrderExpr(absTargetObj, posExpr1, posExpr2)						
					}
				}
			}
		}
		
		if ( absOperName == "remove" )
		{
			var mgetExprStrFragment = ContainerExpressionUtils.createMapGetExprStrFragment(absTargetObj)
	
			val mgetPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, mgetExprStrFragment, bcIndex)
			
			for (mgetPred <- mgetPreds)
			{
				val mgetFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(mgetPred, mgetExprStrFragment) 
				
				for (mgetFE <- mgetFuncExprs)
				{
					val keyExpr = mgetFE.args(2)
				
					// consider predicates where 'k' is the argument or some constant value (if the argument is some variable expression)
					if ( (keyExpr == absArgValues(0)) || (ExpressionUtils.isVariableExpression(absArgValues(0)) && ExpressionUtils.isConstantExpression(keyExpr)) )
					{
						exprSet = exprSet + ContainerExpressionUtils.createMapGetExpr(absTargetObj, keyExpr)
					}
				}
			}
			
			exprSet = exprSet + ContainerExpressionUtils.createMapSizeExpr(absTargetObj)
			
			val morderExprStrFragment = ContainerExpressionUtils.createMapOrderExprStrFragment(absTargetObj)			
			val morderPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, morderExprStrFragment, bcIndex)
			for (morderPred <- morderPreds)
			{
				val morderFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(morderPred, morderExprStrFragment)
				for (morderFE <- morderFuncExprs) exprSet = exprSet + morderFE
			}
		}
		
		if ( absOperName == "clear" )
		{
			val mgetExprStrFragment = ContainerExpressionUtils.createMapGetExprStrFragment(absTargetObj)			
			val mgetPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, mgetExprStrFragment, bcIndex)
			for (mgetPred <- mgetPreds)
			{
				val mgetFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(mgetPred, mgetExprStrFragment)
				for (mgetFE <- mgetFuncExprs) exprSet = exprSet + mgetFE
			}

			exprSet = exprSet + ContainerExpressionUtils.createMapSizeExpr(absTargetObj)			
			
			val morderExprStrFragment = ContainerExpressionUtils.createMapOrderExprStrFragment(absTargetObj)			
			val morderPreds = Configuration.predicatesMngr.getPredicatesOverExpr(callingClassName, callingMethodName, morderExprStrFragment, bcIndex)
			for (morderPred <- morderPreds)
			{
				val morderFuncExprs = FormulaUtils.extractFunctionExprsWithFragment(morderPred, morderExprStrFragment)
				for (morderFE <- morderFuncExprs) exprSet = exprSet + morderFE
			}
		}
		
		if ((absOperName == "moveNext") || (absOperName == "moveLast"))
		{
			exprSet = exprSet + absTargetObj
		}

		return exprSet
	}
}

