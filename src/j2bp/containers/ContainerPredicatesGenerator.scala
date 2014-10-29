package j2bp.containers

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 
import com.ibm.wala.shrikeBT._ 
import com.ibm.wala.types.TypeReference

import common.Constants
import common.Expression
import common.FunctionExpression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils
import common.FormulaUtils

import util.StringUtils

import j2bp.Main
import j2bp.Configuration
import j2bp.AbstractionContext
import j2bp.WALAUtils
import j2bp.ExecutionSimulator


class ContainerPredicatesGenerator extends j2bp.PredicatesGenerator with j2bp.ExecutionVisitor
{
	/**
	 * This method returns tuple of maps: 
	 *  (1) from class name to predicates over static fields; 
	 *  (2) from class name to predicates over object fields;
	 *  (3) from full method name to predicates over local variables and method parameters.
	 */
	def inferPredicates(progClasses : List[String], mthvar2values : Map[(String, Expression), Set[Expression]]) : ( Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]] ) =
	{
		val static2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		val object2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		val method2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		
		var storedKeyValueInfoList = List[(String,String,Expression,Expression,Expression)]()
		var removedKeyValueInfoList = List[(String,String,Expression,Expression)]()
		var loadedKeyValueInfoList = List[(String,String,Expression,Expression,Expression)]()
		var queriedKeysInfoList = List[(String,String,Expression,Expression)]()
		var queriedValuesInfoList = List[(String,String,Expression,Expression)]()		
		var createdMapIterInfoList = List[(String,String,Expression,Expression)]()
		var retrievedIterKeyInfoList = List[(String,String,Expression,Expression)]()

		var mapReprListInfos = List[(String,String,Expression)]()
		
		val ctx = Main.initNewContext(progClasses)
		
		for (className <- progClasses)
		{
			val cls = WALAUtils.findClass(className)
				
			if (Main.INFO) println("[INFO] generating predicates for class: " + className);
			
			ctx.initCurClass(className)
			
			// generate method abstractions
			for (mth <- cls.getDeclaredMethods()) 
			{
				ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic()) 

				val recInfo = inferPredicatesForMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface() || mth.isAbstract(), ctx, static2predicates, object2predicates, method2predicates)
				
				storedKeyValueInfoList = storedKeyValueInfoList ++ recInfo._1
				removedKeyValueInfoList = removedKeyValueInfoList ++ recInfo._2
				loadedKeyValueInfoList = loadedKeyValueInfoList ++ recInfo._3
				queriedKeysInfoList = queriedKeysInfoList ++ recInfo._4
				queriedValuesInfoList = queriedValuesInfoList ++ recInfo._5
				createdMapIterInfoList = createdMapIterInfoList ++ recInfo._6
				retrievedIterKeyInfoList = retrievedIterKeyInfoList ++ recInfo._7
				
				mapReprListInfos = mapReprListInfos ++ recInfo._8
			}
		}
		
		// for each abstract operation "r = it.getCurrent" such that iterator "it" associated with the map "m", add into the list of queried keys for "m" all expressions 'e' such that there exists the predicate 'r = e' and all keys stored in "m"
		for (rikInfo <- retrievedIterKeyInfoList)
		{
			val rikClassName = rikInfo._1
			val rikMethodName = rikInfo._2
			val iterObj = rikInfo._3
			val retrKeyExpr = rikInfo._4
			
			var iterObjID1 = ""
			if (containsMethodVariables(iterObj)) iterObjID1 = rikClassName + "." + rikMethodName + "." + iterObj.toString()
			else iterObjID1 = rikClassName + "." + iterObj.toString()
			
			for (cmiInfo <- createdMapIterInfoList)
			{
				var iterObjID2 = ""
				if (containsMethodVariables(cmiInfo._4)) iterObjID2 = cmiInfo._1 + "." + cmiInfo._2 + "." + cmiInfo._4.toString()
				else iterObjID2 = cmiInfo._1 + "." + cmiInfo._4.toString()
				
				if (iterObjID2 == iterObjID1)
				{
					var rikMapObj = cmiInfo._3
					
					var predSet : Set[AtomicPredicate] = null
					
					// we need only predicates over 'keyVar' with the same scope
					if (containsMethodVariables(retrKeyExpr)) predSet = method2predicates.getOrElse(rikClassName + "." + rikMethodName, Set[AtomicPredicate]())
					else predSet = object2predicates.getOrElse(rikClassName, Set[AtomicPredicate]()) ++ static2predicates.getOrElse(rikClassName, Set[AtomicPredicate]())
					
					// find all predicates 'retrKeyExpr = expr' (created during bytecode traversal) 
					for ( exPred <- predSet )
					{
						if (exPred.isInstanceOf[BinaryPredicate])
						{
							val binExPred = exPred.asInstanceOf[BinaryPredicate]
							
							if (binExPred.containsOperand(retrKeyExpr) && (binExPred.op == "="))
							{
								var tgtExpr : Expression = null
								if (binExPred.left == retrKeyExpr) tgtExpr = binExPred.right
								if (binExPred.right == retrKeyExpr) tgtExpr = binExPred.left
								
								queriedKeysInfoList = queriedKeysInfoList :+ (rikClassName, rikMethodName, rikMapObj, tgtExpr)
							}
						}
					}
					
					// get all keys stored in the map
					for (skvInfo <- storedKeyValueInfoList)
					{
						val skvClassName = skvInfo._1
						val skvMethodName = skvInfo._2
						val skvMapObj = skvInfo._3
						val skvKeyExpr = skvInfo._4
												
						var mapObjID1 = ""
						if (containsMethodVariables(rikMapObj)) mapObjID1 = rikClassName + "." + rikMethodName + "." + rikMapObj.toString()
						else mapObjID1 = rikClassName + "." + rikMapObj.toString()
						
						var mapObjID2 = ""
						if (containsMethodVariables(skvMapObj)) mapObjID2 = skvClassName + "." + skvMethodName + "." + skvMapObj.toString()
						else mapObjID2 = skvClassName + "." + skvMapObj.toString()
						
						if (mapObjID1 == mapObjID2)
						{							
							queriedKeysInfoList = queriedKeysInfoList :+ (skvClassName, skvMethodName, skvMapObj, skvKeyExpr)
						}
					}
				}
			}
		}
		
				
		var allMapKeyInfoList = List[(String,String,Expression,Expression)]()
		for (kvInfo <- storedKeyValueInfoList) allMapKeyInfoList = allMapKeyInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3, kvInfo._4)
		for (kvInfo <- removedKeyValueInfoList) allMapKeyInfoList = allMapKeyInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3, kvInfo._4)
		for (kvInfo <- loadedKeyValueInfoList) allMapKeyInfoList = allMapKeyInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3, kvInfo._4)
		
		var allMapsInfoList = List[(String,String,Expression)]()
		for (kvInfo <- storedKeyValueInfoList) allMapsInfoList = allMapsInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3)
		for (kvInfo <- removedKeyValueInfoList) allMapsInfoList = allMapsInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3)
		for (kvInfo <- loadedKeyValueInfoList) allMapsInfoList = allMapsInfoList :+ (kvInfo._1, kvInfo._2, kvInfo._3)
		for (cmiInfo <- createdMapIterInfoList) allMapsInfoList = allMapsInfoList :+ (cmiInfo._1, cmiInfo._2, cmiInfo._3)
		for (mrlInfo <- mapReprListInfos) allMapsInfoList = allMapsInfoList :+ (mrlInfo._1, mrlInfo._2, mrlInfo._3)
		
		
		// generate predicates over map content from retrieved information
		// we need predicate over key-value pair if the key is queried via get(k) or containsKey(k), or if the value is queried via containsValue(v)

		if (Main.DEBUG)
		{
			println("[DEBUG ContainerPredicatesGenerator.inferPredicates] recorded information:")
			for (skvInfo <- storedKeyValueInfoList) println("\t\t stored key-value pair: ("+skvInfo._1+","+skvInfo._2+","+skvInfo._3+","+skvInfo._4+","+skvInfo._5+")")			
			for (rkvInfo <- removedKeyValueInfoList) println("\t\t removed key-value pair: ("+rkvInfo._1+","+rkvInfo._2+","+rkvInfo._3+","+rkvInfo._4+")")
			for (lkvInfo <- loadedKeyValueInfoList) println("\t\t loaded key-value pair: ("+lkvInfo._1+","+lkvInfo._2+","+lkvInfo._3+","+lkvInfo._4+","+lkvInfo._5+")")
			for (qkInfo <- queriedKeysInfoList) println("\t\t queried key: ("+qkInfo._1+","+qkInfo._2+","+qkInfo._3+","+qkInfo._4+")")
			for (qvInfo <- queriedValuesInfoList) println("\t\t queried value: ("+qvInfo._1+","+qvInfo._2+","+qvInfo._3+","+qvInfo._4+")")
			for (cmiInfo <- createdMapIterInfoList) println("\t\t created map-iterator pair: ("+cmiInfo._1+","+cmiInfo._2+","+cmiInfo._3+","+cmiInfo._4+")")
			for (rikInfo <- retrievedIterKeyInfoList) println("\t\t retrieved iterator-key pair: ("+rikInfo._1+","+rikInfo._2+","+rikInfo._3+","+rikInfo._4+")")
			for (mrlInfo <- mapReprListInfos) println("\t\t map represents list: ("+mrlInfo._1+","+mrlInfo._2+","+mrlInfo._3+")")
		}
		
		
		// each tuple contains a new predicate, full method name, and some important expressions
		var newPredicates = List[(AtomicPredicate, String, List[Expression])]()

		
		// create predicates mget(map,m,k) = v for all queried keys and values related to the same map
		for (skvInfo <- storedKeyValueInfoList)
		{
			val className = skvInfo._1
			val methodName = skvInfo._2
			val mapObj = skvInfo._3
			val keyExpr = skvInfo._4
			val valueExpr = skvInfo._5

			var isLoadedKey = false
			
			for (lkvInfo <- loadedKeyValueInfoList)
			{
				if ((lkvInfo._1 == className) && (lkvInfo._3 == mapObj) && (lkvInfo._4 == keyExpr)) 
				{
					if (containsMethodVariables(mapObj))
					{
						if (lkvInfo._2 == methodName) isLoadedKey = true
					}
					else
					{
						isLoadedKey = true	
					}
				}
			}
			
			if (isQueriedKey(queriedKeysInfoList, className, methodName, mapObj, keyExpr) || isQueriedValue(queriedValuesInfoList, className, methodName, mapObj, valueExpr) || isLoadedKey)
			{
				val newPred = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, keyExpr), valueExpr)

				newPredicates = newPredicates :+ (newPred, className+"."+methodName, List(mapObj,keyExpr,valueExpr))
			}
		}
		
		
		for (lkvInfo <- loadedKeyValueInfoList)
		{
			val className = lkvInfo._1
			val methodName = lkvInfo._2
			val mapObj = lkvInfo._3
			val keyExpr = lkvInfo._4
			val retVarExpr = lkvInfo._5

			// create predicate "mget(m,k) = r"
			
			val newPredRV = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, keyExpr), retVarExpr)
					
			newPredicates = newPredicates :+ (newPredRV, className+"."+methodName, List(mapObj,keyExpr,retVarExpr))

			
			// find all predicates 'retVarName = expr' (created during bytecode traversal) and create predicates "mget(m,k) = expr"
			
			var exPredSet = Set[AtomicPredicate]()
			
			for (predSet <- static2predicates.values()) exPredSet = exPredSet ++ predSet
			for (predSet <- object2predicates.values()) exPredSet = exPredSet ++ predSet
			for (predSet <- method2predicates.values()) exPredSet = exPredSet ++ predSet
			
			for (exPred <- exPredSet)
			{
				if (exPred.isInstanceOf[BinaryPredicate])
				{
					val binExPred = exPred.asInstanceOf[BinaryPredicate]
			
					if (binExPred.containsOperand(retVarExpr) && ((binExPred.op == "=") || (binExPred.op == "!=")))
					{
						var tgtExpr : Expression = null
						if (binExPred.left == retVarExpr) tgtExpr = binExPred.right
						if (binExPred.right == retVarExpr) tgtExpr = binExPred.left
						
						val mgetExpr = ContainerExpressionUtils.createMapGetExpr(mapObj, keyExpr)
						
						// such predicate with tgtExpr equal to mgetExpr might be just created above
						if (tgtExpr != mgetExpr)
						{
							val newPred = new BinaryPredicate("=", mgetExpr, tgtExpr)
			
							newPredicates = newPredicates :+ (newPred, className+"."+methodName, List(mapObj,keyExpr,tgtExpr))
						}
					}
				}
			}
		}
		
		
		// create predicate morder(mit,m,k1,k2) for all pairs of queried keys related to the same map
		
		var generatedMapKeysTriples = List[(String,Expression,Expression)]()
		
		for (mkInfo1 <- allMapKeyInfoList)
		{
			val className1 = mkInfo1._1
			val methodName1 = mkInfo1._2
			val mapObj1 = mkInfo1._3
			val keyExpr1 = mkInfo1._4

			for (mkInfo2 <- allMapKeyInfoList)
			{
				val className2 = mkInfo2._1
				val methodName2 = mkInfo2._2
				val mapObj2 = mkInfo2._3
				val keyExpr2 = mkInfo2._4
				
				
				var mapObjID1 = ""
				if (containsMethodVariables(mapObj1)) mapObjID1 = className1 + "." + methodName1 + "." + mapObj1.toString()
				else mapObjID1 = className1 + "." + mapObj1.toString()
				
				var mapObjID2 = ""
				if (containsMethodVariables(mapObj2)) mapObjID2 = className2 + "." + methodName2 + "." + mapObj2.toString()
				else mapObjID2 = className2 + "." + mapObj2.toString()
				
				
				if ( (mapObjID1 == mapObjID2) && (keyExpr1 != keyExpr2) && isQueriedKey(queriedKeysInfoList, className1, methodName1, mapObj1, keyExpr1) && isQueriedKey(queriedKeysInfoList, className2, methodName2, mapObj1, keyExpr2) )
				{
					if ( ( ! generatedMapKeysTriples.contains((mapObjID1,keyExpr1,keyExpr2)) ) && ( ! generatedMapKeysTriples.contains((mapObjID1,keyExpr2,keyExpr1)) ) )
					{						
						val newPred = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj1, keyExpr1, keyExpr2))
						
						newPredicates = newPredicates :+ (newPred, className1+"."+methodName1, List(mapObj1,keyExpr1,keyExpr2))

						generatedMapKeysTriples = generatedMapKeysTriples :+ (mapObjID1,keyExpr1,keyExpr2)
					}
				}
			}
		}
		
		
		// create predicates morder(m,k,bot) and morder(m,bot,k) for each queried key 'k' related to map 'm'
		for (mkInfo <- allMapKeyInfoList)
		{
			val className = mkInfo._1
			val methodName = mkInfo._2
			val mapObj = mkInfo._3
			val keyExpr = mkInfo._4
			
			if ( isQueriedKey(queriedKeysInfoList, className, methodName, mapObj, keyExpr) )
			{
				val newPred1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr, BasicContainerModel.BOTTOM_EXPR))
				val newPred2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.BOTTOM_EXPR, keyExpr))

				newPredicates = newPredicates :+ (newPred1, className+"."+methodName, List(mapObj,keyExpr))
				newPredicates = newPredicates :+ (newPred2, className+"."+methodName, List(mapObj,keyExpr))
			}
		}
		
		
		// create predicate morder(m,bot,bot) for each map 'm'
		for (mkInfo <- allMapKeyInfoList)
		{
			val className = mkInfo._1
			val methodName = mkInfo._2
			val mapObj = mkInfo._3
			
			val newPred = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.BOTTOM_EXPR, BasicContainerModel.BOTTOM_EXPR))
			
			newPredicates = newPredicates :+ (newPred, className+"."+methodName, List(mapObj))
		}
		
		
		// for each pair of a queried key and iterator related to the same map, create the predicates morder(m,k,it) and morder(m,it,k)
		for (cmiInfo <- createdMapIterInfoList)
		{
			val className = cmiInfo._1
			val methodName = cmiInfo._2
			val mapObj = cmiInfo._3
			val iterObj = cmiInfo._4
			
			for (mkInfo <- allMapKeyInfoList)
			{
				var mapObjID1 = ""
				if (containsMethodVariables(mapObj)) mapObjID1 = className + "." + methodName + "." + mapObj.toString()
				else mapObjID1 = className + "." + mapObj.toString()
				
				var mapObjID2 = ""
				if (containsMethodVariables(mkInfo._3)) mapObjID2 = mkInfo._1 + "." + mkInfo._2 + "." + mkInfo._3.toString()
				else mapObjID2 = mkInfo._1 + "." + mkInfo._3.toString()
				
				if (mapObjID1 == mapObjID2)
				{
					val keyExpr = mkInfo._4
					
					if ( isQueriedKey(queriedKeysInfoList, className, methodName, mapObj, keyExpr) )
					{						
						val newPred1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr, iterObj))
						val newPred2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, iterObj, keyExpr))
						
						newPredicates = newPredicates :+ (newPred1, className+"."+methodName, List(mapObj,keyExpr,iterObj))
						newPredicates = newPredicates :+ (newPred2, className+"."+methodName, List(mapObj,iterObj,keyExpr))
					}
				}
			}
		}
		
		
		// for each iterator 'it' related to map 'm', create predicates morder(m,it,bot) and morder(m,bot,it)
		for (cmiInfo <- createdMapIterInfoList)
		{
			val className = cmiInfo._1
			val methodName = cmiInfo._2
			val mapObj = cmiInfo._3
			val iterObj = cmiInfo._4
		
			val newPred1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, iterObj, BasicContainerModel.BOTTOM_EXPR))
			val newPred2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.BOTTOM_EXPR, iterObj))
			
			newPredicates = newPredicates :+ (newPred1, className+"."+methodName, List(mapObj,iterObj))
			newPredicates = newPredicates :+ (newPred2, className+"."+methodName, List(mapObj,iterObj))
		}

		
		// for each map 'm' that represents an ordered list, we generate the predicate mget(m,0) = bot, predicates mget(m,1) = bot, ..., mget(m,sz_max) = bot, and predicates mget(map,m,1) = w, ..., mget(map,m,sz_max) = w for any possible stored element (stored value in the same map) in place of the symbol 'w'
		for (mrlInfo <- mapReprListInfos)
		{
			val className = mrlInfo._1
			val methodName = mrlInfo._2
			val mapObj = mrlInfo._3
			
			val newZeroPred = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, Constants.ZERO_EXPR), BasicContainerModel.BOTTOM_EXPR)			
			newPredicates = newPredicates :+ (newZeroPred, className+"."+methodName, List(mapObj))
			
			for (sz <- 1 to BasicContainerModel.maxSize)
			{
				val newUnusedPred = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, new Expression(String.valueOf(sz))), BasicContainerModel.BOTTOM_EXPR)
				
				newPredicates = newPredicates :+ (newUnusedPred, className+"."+methodName, List(mapObj))
				
				for (skvInfo <- storedKeyValueInfoList)
				{
					val classNameSKV = skvInfo._1
					val methodNameSKV = skvInfo._2
					val mapObjSKV = skvInfo._3
					val valueExpr = skvInfo._5
					
					var mapObjID = ""
					if (containsMethodVariables(mapObj)) mapObjID = className + "." + methodName + "." + mapObj.toString()
					else mapObjID = className + "." + mapObj.toString()
				
					var mapObjSKVID = ""
					if (containsMethodVariables(mapObjSKV)) mapObjSKVID = classNameSKV + "." + methodNameSKV + "." + mapObjSKV.toString()
					else mapObjSKVID = classNameSKV + "." + mapObjSKV.toString()
				
					if (mapObjID == mapObjSKVID)
					{	
						val newValuePred = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, new Expression(String.valueOf(sz))), valueExpr)
						
						newPredicates = newPredicates :+ (newValuePred, className+"."+methodName, List(mapObj,valueExpr))
					}
				}
			}
		}
		
		
		// generate predicates 'r = x', 'morder(mit,m,x,it)', 'morder(mit,m,it,x)', 'morder(mit,m,bot,x)', and 'morder(mit,m,x,bot)' for each abstract operation 'r = it.getCurrent()', map 'm' associated with 'it' and all integer values 'x' between 1 and sz_max, and also generate predicate 'r = 0' -> but only if the map represents some list 
		for (rikInfo <- retrievedIterKeyInfoList)
		{
			val className = rikInfo._1
			val methodName = rikInfo._2
			val iterObj = rikInfo._3
			val keyExpr = rikInfo._4
			
			var iterObjID1 = ""
			if (containsMethodVariables(iterObj)) iterObjID1 = className + "." + methodName + "." + iterObj.toString()
			else iterObjID1 = className + "." + iterObj.toString()
			
			for (cmiInfo <- createdMapIterInfoList)
			{
				var iterObjID2 = ""
				if (containsMethodVariables(cmiInfo._4)) iterObjID2 = cmiInfo._1 + "." + cmiInfo._2 + "." + cmiInfo._4.toString()
				else iterObjID2 = cmiInfo._1 + "." + cmiInfo._4.toString()
					
				if (iterObjID2 == iterObjID1)
				{
					var mapObj = cmiInfo._3
			
					// consider only maps that represent lists
					if ( mapReprListInfos.contains( (className, methodName, mapObj) ) )
					{
						for (sz <- 1 to BasicContainerModel.maxSize)
						{
							val newEqPred = new BinaryPredicate("=", keyExpr, ExpressionUtils.createExprFromStr(String.valueOf(sz)))
							
							newPredicates = newPredicates :+ (newEqPred, className+"."+methodName, List(keyExpr))
							
							val newMapOrderPredIt1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, new Expression(String.valueOf(sz)), iterObj))
							val newMapOrderPredIt2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, iterObj, new Expression(String.valueOf(sz))))
				
							newPredicates = newPredicates :+ (newMapOrderPredIt1, className+"."+methodName, List(mapObj,iterObj))
							newPredicates = newPredicates :+ (newMapOrderPredIt2, className+"."+methodName, List(mapObj,iterObj))						
											
							val newMapOrderPredBot1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, new Expression(String.valueOf(sz)), BasicContainerModel.BOTTOM_EXPR))
							val newMapOrderPredBot2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.BOTTOM_EXPR, new Expression(String.valueOf(sz))))
					
							newPredicates = newPredicates :+ (newMapOrderPredBot1, className+"."+methodName, List(mapObj))
							newPredicates = newPredicates :+ (newMapOrderPredBot2, className+"."+methodName, List(mapObj))
						}
						
						for (sz1 <- 1 to BasicContainerModel.maxSize)
						{
							for (sz2 <- (sz1 + 1) to BasicContainerModel.maxSize)
							{
								val newMapOrderPred1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, new Expression(String.valueOf(sz1)), new Expression(String.valueOf(sz2))))
								val newMapOrderPred2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, new Expression(String.valueOf(sz2)), new Expression(String.valueOf(sz1))))
								
								newPredicates = newPredicates :+ (newMapOrderPred1, className+"."+methodName, List(mapObj))
								newPredicates = newPredicates :+ (newMapOrderPred2, className+"."+methodName, List(mapObj))
							}
						}
						
						val newZeroEqPred = new BinaryPredicate("=", keyExpr, Constants.ZERO_EXPR)				
						newPredicates = newPredicates :+ (newZeroEqPred, className+"."+methodName, List(keyExpr))
					}
				}
			}
		}
		
		
		// for each abstract map 'm' we create the predicate 'msize(msz,m) >= sz_max', because they are needed to correctly force backtracking when the size of some container exceeds the bound
		for (mapInfo <- allMapsInfoList)
		{
			val className = mapInfo._1
			val methodName = mapInfo._2
			val mapObj = mapInfo._3
			
			val newSizePred = new BinaryPredicate(">=", ContainerExpressionUtils.createMapSizeExpr(mapObj), ExpressionUtils.createExprFromStr(String.valueOf(BasicContainerModel.maxSize)))
						
			newPredicates = newPredicates :+ (newSizePred, className+"."+methodName, List(mapObj))
		}


		// save new predicates		
		for ( newPredInfo <- newPredicates )
		{
			if (containsMethodVariables(newPredInfo._3)) addPredicate(newPredInfo._2, method2predicates, newPredInfo._1)
			else if (containsObjectFields(newPredInfo._3)) addPredicate(StringUtils.extractClassName(newPredInfo._2), object2predicates, newPredInfo._1)
			else addPredicate(StringUtils.extractClassName(newPredInfo._2), static2predicates, newPredInfo._1)
		}

		
		// generate predicates over map content with temporary variables (tmpk) replaced by their possible constant values, and drop the original predicate with 'tmpk'
		// constant values are taken from available equality predicates
		
		for (mthName <- method2predicates.keys)
		{
			val className = StringUtils.extractClassName(mthName)
			
			val oldMthPredSet = method2predicates.getOrElse(mthName, Set[AtomicPredicate]())
			
			var newMthPredSet = Set[AtomicPredicate]()
		
			for (pred <- oldMthPredSet)
			{
				var containsTempKeyVar = false
				
				if (pred.containsFunction(BasicContainerModel.FUNC_MAP_GET))
				{
					val mgetFuncExprs = FormulaUtils.extractFunctionExpressionsRecursively(pred, BasicContainerModel.FUNC_MAP_GET)
					
					for (mgetFunc <- mgetFuncExprs)
					{
						if (mgetFunc.args(2) == BasicContainerModel.TMP_KEY_EXPR) containsTempKeyVar = true						
					}
				}
				
				if (pred.containsFunction(BasicContainerModel.FUNC_MAP_ORDER))
				{
					val morderFuncExprs = FormulaUtils.extractFunctionExpressionsRecursively(pred, BasicContainerModel.FUNC_MAP_ORDER)
					
					for (morderFunc <- morderFuncExprs)
					{
						if (morderFunc.args(2) == BasicContainerModel.TMP_KEY_EXPR) containsTempKeyVar = true
						if (morderFunc.args(3) == BasicContainerModel.TMP_KEY_EXPR) containsTempKeyVar = true						
					}
				}
				
				if (containsTempKeyVar)
				{
					var possValues = Set[Expression]()
					
					// find possible values based on equality predicates
					for ( tkvPred <- oldMthPredSet if (tkvPred.isInstanceOf[BinaryPredicate] && tkvPred.containsOperand(BasicContainerModel.VAR_TMP_KEY)) )
					{
						val valueExpr = FormulaUtils.extractOtherOperandFromBinPred(tkvPred.asInstanceOf[BinaryPredicate], BasicContainerModel.TMP_KEY_EXPR)
					
						if (ExpressionUtils.isConstantValue(valueExpr)) possValues = possValues + valueExpr
					}
					
					// replace variable 'tmpk' with possible constant values					
					for (posVal <- possValues)
					{
						newMthPredSet = newMthPredSet + FormulaUtils.copyWithReplace(pred, BasicContainerModel.VAR_TMP_KEY, posVal.toString())
					}
				}
				else
				{
					// keep the original predicate
					newMthPredSet = newMthPredSet + pred
				}
			}
			
			method2predicates.put(mthName, newMthPredSet)
		}
				
			
		// generate predicates over map content with keys and values at the same scope as the map variable
		
		for (mthName <- method2predicates.keys)
		{
			val className = StringUtils.extractClassName(mthName)
			
			val oldMthPredSet = method2predicates.getOrElse(mthName, Set[AtomicPredicate]())
			
			var newMthPredSet = Set[AtomicPredicate]()
			
			for (pred <- oldMthPredSet)
			{
				// keep also the original predicate
				newMthPredSet = newMthPredSet + pred

				// for each mget and morder, we replace local variable names with constants and object fields (i.e., create more predicates) based on possible values of the local variables
				
				if (pred.containsFunction(BasicContainerModel.FUNC_MAP_GET) || pred.containsFunction(BasicContainerModel.FUNC_MAP_ORDER))
				{
					val mapFuncExprs = FormulaUtils.extractFunctionExpressionsRecursively(pred, BasicContainerModel.FUNC_MAP_GET) ++ FormulaUtils.extractFunctionExpressionsRecursively(pred, BasicContainerModel.FUNC_MAP_ORDER)
					
					var isMapWithObjectScope = false
					
					for (mapFunc <- mapFuncExprs)
					{
						if ( ! ExpressionUtils.isMethodVariable(mapFunc.args(1)) ) isMapWithObjectScope = true
					}
				
					if (isMapWithObjectScope)
					{
						var newExprs : Map[Expression, Set[Expression]] = new HashMap
						
						for (mapFE <- mapFuncExprs)
						{
							if (mapFE.name == BasicContainerModel.FUNC_MAP_GET)
							{
								val oldKeyExpr = mapFE.args(2)
								
								if ( ! ExpressionUtils.isMethodVariable(mapFE.args(1)) )
								{
									// map variable has object scope -> create new predicates with keys and values at the object scope (fields, integer constants)
						
									newExprs.put( oldKeyExpr, mthvar2values.getOrElse( (mthName, oldKeyExpr), Set[Expression](oldKeyExpr) ) )
								}
								else
								{
									// keep old key
									newExprs.put(oldKeyExpr, Set[Expression](oldKeyExpr))
								}
							}
							else if (mapFE.name == BasicContainerModel.FUNC_MAP_ORDER)
							{
								val oldPosExpr1 = mapFE.args(2)
								val oldPosExpr2 = mapFE.args(3)
								
								if ( ! ExpressionUtils.isMethodVariable(mapFE.args(1)) )
								{
									// map variable has object scope -> create new predicates with keys and values at the object scope (fields, integer constants)
						
									newExprs.put( oldPosExpr1, mthvar2values.getOrElse( (mthName, oldPosExpr1), Set[Expression](oldPosExpr1) ) )
									newExprs.put( oldPosExpr2, mthvar2values.getOrElse( (mthName, oldPosExpr2), Set[Expression](oldPosExpr2) ) )
								}
								else
								{
									// keep old key
									newExprs.put(oldPosExpr1, Set[Expression](oldPosExpr1))
									newExprs.put(oldPosExpr2, Set[Expression](oldPosExpr2))
								}
							}
						}
						
						val predOperands = FormulaUtils.extractOperands(pred)
						
						for (operand <- predOperands)
						{
							if (ExpressionUtils.isMethodVariable(operand))
							{
								newExprs.put( operand, mthvar2values.getOrElse( (mthName, operand), Set[Expression](operand) ) )
							}
							else
							{
								newExprs.put(operand, Set[Expression](operand))
							}
						}
						
						// generate all possible combinations (over all possible values) using iterative process
						
						var newPredCombs = Set[AtomicPredicate](pred)
						
						for ( (oldExpr, newExprSet) <- newExprs )
						{
							var tempNewPredCombs = Set[AtomicPredicate]()
							
							for (prevNewPred <- newPredCombs)
							{
								for (newExpr <- newExprSet)
								{
									val curNewPred = FormulaUtils.copyWithReplace(prevNewPred, oldExpr.toString(), newExpr.toString())
									
									tempNewPredCombs = tempNewPredCombs + curNewPred
								}
							}
							
							newPredCombs = tempNewPredCombs
						}
									
						for (newPred <- newPredCombs)
						{
							if (FormulaUtils.containsMethodVariables(newPred)) newMthPredSet = newMthPredSet + newPred
							else if (FormulaUtils.containsObjectFields(newPred)) addPredicate(className, object2predicates, newPred)
							else addPredicate(className, static2predicates, newPred)
						}
					}
				}
			}
			
			method2predicates.put(mthName, newMthPredSet)
		}
		
		return (static2predicates, object2predicates, method2predicates)
	}

	
	def inferPredicatesForMethod(mth : IBytecodeMethod, isAbstract : Boolean, ctx : AbstractionContext, static2predicates : Map[String, Set[AtomicPredicate]], object2predicates : Map[String, Set[AtomicPredicate]], method2predicates : Map[String, Set[AtomicPredicate]]) : (List[(String,String,Expression,Expression,Expression)], List[(String,String,Expression,Expression)], List[(String,String,Expression,Expression,Expression)], List[(String,String,Expression,Expression)], List[(String,String,Expression,Expression)], List[(String,String,Expression,Expression)], List[(String,String,Expression,Expression)], List[(String,String,Expression)]) =
	{
		if (Main.INFO) println("[INFO] inferring predicates for method: " + ctx.getCurMethodName());
		
		// loop through all Shrike bytecode instructions and process each relevant one (some are ignored) to get necessary information for generating predicates
		
		var dataHolder : Map[String, java.lang.Object] = new HashMap
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
		
		ExecutionSimulator.simulateMethod(mth, isAbstract, ctx, this, dataHolder)
		
		// retrieve data from holder
		val storedKeyValueInfoList = dataHolder.getOrElse("storedKV", List[(String,String,Expression,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression,Expression)]]		
		val removedKeyValueInfoList = dataHolder.getOrElse("removedKV", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		val loadedKeyValueInfoList = dataHolder.getOrElse("loadedKV", List[(String,String,Expression,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression,Expression)]]
		var queriedKeysInfoList = dataHolder.getOrElse("queriedKeys", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		var queriedValuesInfoList = dataHolder.getOrElse("queriedValues", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]		
		val createdMapIterInfoList = dataHolder.getOrElse("createdMI", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		val retrievedIterKeyInfoList = dataHolder.getOrElse("retrievedIK", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]		
		val sizeRetVarInfoList = dataHolder.getOrElse("szVars", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		val createdMapKeyViewPairs = dataHolder.getOrElse("createdMKV", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		val createdMapValueViewPairs = dataHolder.getOrElse("createdMVV", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		val referenceVars = dataHolder.getOrElse("refVars", List[Expression]()).asInstanceOf[List[Expression]]
		val mapReprListInfos = dataHolder.getOrElse("mapRL", List[(String,String,Expression)]()).asInstanceOf[List[(String,String,Expression)]]

		// generate predicates from retrieved information 

		if (Main.DEBUG)
		{
			println("[DEBUG ContainerPredicatesGenerator.inferPredicatesForMethod] recorded information:")			
			for (szVarInfo <- sizeRetVarInfoList) println("\t\t size return var: ("+szVarInfo._1+","+szVarInfo._2+")")
			for (mkvp <- createdMapKeyViewPairs) println("\t\t map and key view: ("+mkvp._1+","+mkvp._2+")")
			for (mvvp <- createdMapValueViewPairs) println("\t\t map and value view: ("+mvvp._1+","+mvvp._2+")")
			for (refVar <- referenceVars) println("\t\t reference var = " + refVar)
		}
		
		// each tuple contains a new predicate, full method name, and some important expressions
		var newPredicates = List[(AtomicPredicate, String, List[Expression])]()
		
		for (szVarInfo <- sizeRetVarInfoList)
		{
			val newPred = new BinaryPredicate("=", ContainerExpressionUtils.createMapSizeExpr(szVarInfo._1), szVarInfo._2)

			newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(szVarInfo._1,szVarInfo._2))
		}

		for (mkvp <- createdMapKeyViewPairs)
		{
			val newPred = new UnaryPredicate("", ContainerExpressionUtils.createMapKeysViewExpr(mkvp._1, mkvp._2))

			newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(mkvp._1,mkvp._2))
		}
		
		for (mvvp <- createdMapValueViewPairs)
		{
			val newPred = new UnaryPredicate("", ContainerExpressionUtils.createMapValuesViewExpr(mvvp._1, mvvp._2))
			
			newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(mvvp._1,mvvp._2))
		}
		
		for (refVar1 <- referenceVars)
		{
			for (refVar2 <- referenceVars)
			{
				if (refVar1 != refVar2)
				{
					val newPred = new BinaryPredicate("=", refVar1, refVar2)
					
					newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(refVar1,refVar2))
				}
			}
		}
		
		for ( newPredInfo <- newPredicates )
		{
			if (containsMethodVariables(newPredInfo._3)) addPredicate(newPredInfo._2, method2predicates, newPredInfo._1)
			else if (containsObjectFields(newPredInfo._3)) addPredicate(StringUtils.extractClassName(newPredInfo._2), object2predicates, newPredInfo._1)
			else addPredicate(StringUtils.extractClassName(newPredInfo._2), static2predicates, newPredInfo._1)
		}
		
		// for each keys view 'ms' over the map 'm', add the list of queried keys for 'ms' into the list of queried keys for 'm'
		for (mkvp <- createdMapKeyViewPairs)
		{
			val tgtMapObj = mkvp._1
			val viewMapObj = mkvp._2
			
			var newQueriedKeys = List[(String,String,Expression,Expression)]()
			
			for (qk <- queriedKeysInfoList)
			{
				if (qk._3 == viewMapObj)
				{
					newQueriedKeys = newQueriedKeys :+ (qk._1, qk._2, tgtMapObj, qk._4)
				}
			}
			
			queriedKeysInfoList = queriedKeysInfoList ++ newQueriedKeys 
		}
		
		// for each values view 'ml' over the map 'm', add the list of queried values for 'ml' into the list of queried values for 'm'
		for (mvvp <- createdMapValueViewPairs)
		{
			val tgtMapObj = mvvp._1
			val viewMapObj = mvvp._2
			
			var newQueriedValues = List[(String,String,Expression,Expression)]()
			
			for (qv <- queriedValuesInfoList)
			{
				if (qv._3 == viewMapObj)
				{
					newQueriedValues = newQueriedValues :+ (qv._1, qv._2, tgtMapObj, qv._4)
				}
			}
			
			queriedValuesInfoList = queriedValuesInfoList ++ newQueriedValues 
		}
		
		return (storedKeyValueInfoList, removedKeyValueInfoList, loadedKeyValueInfoList, queriedKeysInfoList, queriedValuesInfoList, createdMapIterInfoList, retrievedIterKeyInfoList, mapReprListInfos)
	}
	
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		
		var mapReprListInfos = dataHolder.getOrElse("mapRL", List[(String,String,Expression)]()).asInstanceOf[List[(String,String,Expression)]]		
		
		// we need predicates for all assignments
		if ( ! ExpressionUtils.isStackSymbol(newValue) ) 
		{
			val newPred = new BinaryPredicate("=", ExpressionUtils.createArrayAccessExpr(arrayExpr, index), newValue)

			if (containsMethodVariables(List(arrayExpr,newValue))) addPredicate(ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), method2predicates, newPred)
			else if (containsObjectFields(List(arrayExpr,newValue))) addPredicate(ctx.getCurClassOrigName(), object2predicates, newPred)
			else addPredicate(ctx.getCurClassOrigName(), static2predicates, newPred)
		}
		
		if (BasicContainerModel.isSupportedListClass(StringUtils.getPlainClassName(elementType)))
		{
			mapReprListInfos = mapReprListInfos :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), ExpressionUtils.createArrayAccessExpr(arrayExpr, index))
		}
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
		
		dataHolder.put("mapRL", mapReprListInfos)
	}
	
	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
				
		if ( ( ! ExpressionUtils.isStackSymbol(value1) ) && ( ! ExpressionUtils.isStackSymbol(value2) ) )
		{
			val newPred = new BinaryPredicate(posOpStr, value1, value2)

			if (containsMethodVariables(List(value1,value2))) addPredicate(ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), method2predicates, newPred)
			else if (containsObjectFields(List(value1,value2))) addPredicate(ctx.getCurClassOrigName(), object2predicates, newPred)
			else addPredicate(ctx.getCurClassOrigName(), static2predicates, newPred)
		}
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]

		// get data from holder		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		var storedKeyValueInfoList = dataHolder.getOrElse("storedKV", List[(String,String,Expression,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression,Expression)]]		
		var removedKeyValueInfoList = dataHolder.getOrElse("removedKV", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		var loadedKeyValueInfoList = dataHolder.getOrElse("loadedKV", List[(String,String,Expression,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression,Expression)]]
		var queriedKeysInfoList = dataHolder.getOrElse("queriedKeys", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		var queriedValuesInfoList = dataHolder.getOrElse("queriedValues", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]		
		var createdMapIterInfoList = dataHolder.getOrElse("createdMI", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]
		var retrievedIterKeyInfoList = dataHolder.getOrElse("retrievedIK", List[(String,String,Expression,Expression)]()).asInstanceOf[List[(String,String,Expression,Expression)]]		
		var sizeRetVarInfoList = dataHolder.getOrElse("szVars", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		var createdMapKeyViewPairs = dataHolder.getOrElse("createdMKV", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		var createdMapValueViewPairs = dataHolder.getOrElse("createdMVV", List[(Expression,Expression)]()).asInstanceOf[List[(Expression,Expression)]]
		var referenceVars = dataHolder.getOrElse("refVars", List[Expression]()).asInstanceOf[List[Expression]]

		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val targetObj = piRes._1
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
		
		// each tuple contains a new predicate, full method name, and some important expressions
		var newPredicates = List[(AtomicPredicate, String, List[Expression])]()

		if ( ! skip )
		{
			if ( BasicContainerModel.isSupportedMethod(ownerClassName, tgtMethodName) )
			{
				val absOperSeq = BasicContainerModel.getAbstractOperationSeq(ownerClassName, tgtMethodName, targetObj, actualParamArray, retVarExpr,  ctx.getCurClassOrigName(), ctx.getCurMethodName(), insnIndex)
				
				for (absOper <- absOperSeq)
				{
					val absOperName = absOper._1
					val absTargetObj = absOper._2
					val absParamVals = absOper._3
					val absRetVarExprs = absOper._4
					
					if (Main.DEBUG) println("[DEBUG ContainerPredicatesGenerator.inferPredicatesForMethod] abstract operation name = " + absOperName)
					if (Main.DEBUG) println("[DEBUG ContainerPredicatesGenerator.inferPredicatesForMethod] abstract operation target = '" + absTargetObj + "'")
					
					if (Main.DEBUG)
					{
						if (absRetVarExprs.length > 0)
						{
							println("[DEBUG ContainerPredicatesGenerator.inferPredicatesForMethod] abstract operation return = '" + absRetVarExprs(0) + "'")
						}
					}
					
					if (absOperName == "get") 
					{
						loadedKeyValueInfoList = loadedKeyValueInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0), absRetVarExprs(0))
						
						queriedKeysInfoList = queriedKeysInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))
					}

					if (absOperName == "containsKey") queriedKeysInfoList = queriedKeysInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))

					if (absOperName == "containsValue") queriedValuesInfoList = queriedValuesInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))
					
					if (absOperName == "findKey") queriedValuesInfoList = queriedValuesInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))

					if (absOperName == "put") storedKeyValueInfoList = storedKeyValueInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(),  absTargetObj, absParamVals(0), absParamVals(1))
					
					if (absOperName == "putAhead") 
					{
						storedKeyValueInfoList = storedKeyValueInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0), absParamVals(1))
						queriedKeysInfoList = queriedKeysInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))
						queriedKeysInfoList = queriedKeysInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(2))
					}

					if (absOperName == "remove") removedKeyValueInfoList = removedKeyValueInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absParamVals(0))

					if (absOperName == "size") sizeRetVarInfoList = sizeRetVarInfoList :+ (absTargetObj, absRetVarExprs(0))
					
					if (absOperName == "keysView") createdMapKeyViewPairs = createdMapKeyViewPairs :+ (absTargetObj, absRetVarExprs(0))
					
					if (absOperName == "valuesView") createdMapValueViewPairs = createdMapValueViewPairs :+ (absTargetObj, absRetVarExprs(0))
					
					if (absOperName == "createIterator") 
					{
						createdMapIterInfoList = createdMapIterInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absRetVarExprs(0))
						
						// we must save assocation between iterators and maps for the purpose of generating the correct predicates
						ContainerAbstractionData.saveMapForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExprs(0), absTargetObj)	
					}
					
					if (absOperName == "getCurrent") retrievedIterKeyInfoList = retrievedIterKeyInfoList :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), absTargetObj, absRetVarExprs(0))
					
					if (absOperName == "hasMore")
					{
						val newPred1 = new BinaryPredicate("=", absRetVarExprs(0), Constants.ZERO_EXPR)
						val newPred2 = new BinaryPredicate("=", absRetVarExprs(0), Constants.ONE_EXPR)
							
						newPredicates = newPredicates :+ (newPred1, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absRetVarExprs(0)))
						newPredicates = newPredicates :+ (newPred2, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absRetVarExprs(0)))
					}
					
					if (absOperName == "increment")
					{
						if ( absParamVals(0) != absRetVarExprs(0) )
						{
							val newPred = new BinaryPredicate("=", absRetVarExprs(0), ExpressionUtils.createExprFromStr("(+ " + absParamVals(0) + " 1)"))

							newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absRetVarExprs(0), absParamVals(0)))
						}
					}
					
					if (absOperName == "decrement")
					{
						if ( absParamVals(0) != absRetVarExprs(0) )
						{
							val newPred = new BinaryPredicate("=", absRetVarExprs(0), ExpressionUtils.createExprFromStr("(- " + absParamVals(0) + " 1)"))
							
							newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absRetVarExprs(0), absParamVals(0)))
						}
					}
					
					if ((absOperName == "assign") || (absOperName == "ifeq"))
					{
						val newPred = new BinaryPredicate("=", absParamVals(0), absParamVals(1))
						
						newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absParamVals(0), absParamVals(1)))
					}
					
					if (absOperName == "ifgt")
					{
						val newPred = new BinaryPredicate(">", absParamVals(0), absParamVals(1))
		
						newPredicates = newPredicates :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(absParamVals(0), absParamVals(1)))				
					}							
				}
			}
			
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
		
		for ( newPredInfo <- newPredicates )
		{
			if (containsMethodVariables(newPredInfo._3)) addPredicate(newPredInfo._2, method2predicates, newPredInfo._1)
			else if (containsObjectFields(newPredInfo._3)) addPredicate(StringUtils.extractClassName(newPredInfo._2), object2predicates, newPredInfo._1)
			else addPredicate(StringUtils.extractClassName(newPredInfo._2), static2predicates, newPredInfo._1)
		}

		
		// put retrieved lists to the holder object		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)		
		dataHolder.put("storedKV", storedKeyValueInfoList)
		dataHolder.put("removedKV", removedKeyValueInfoList)
		dataHolder.put("loadedKV", loadedKeyValueInfoList)				
		dataHolder.put("queriedKeys", queriedKeysInfoList)
		dataHolder.put("queriedValues", queriedValuesInfoList)
		dataHolder.put("createdMI", createdMapIterInfoList)
		dataHolder.put("retrievedIK", retrievedIterKeyInfoList)
		dataHolder.put("szVars", sizeRetVarInfoList)
		dataHolder.put("createdMKV", createdMapKeyViewPairs)
		dataHolder.put("createdMVV", createdMapValueViewPairs)
		dataHolder.put("refVars", referenceVars)
	}
	
	override def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
			
		var mapReprListInfos = dataHolder.getOrElse("mapRL", List[(String,String,Expression)]()).asInstanceOf[List[(String,String,Expression)]]
		
		val tgtFieldExpr = ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName)
		
		// we need predicates for all assignments
		if ( ( ! ExpressionUtils.isStackSymbol(newValue) ) && ( ! newValue.contains(tgtFieldExpr.toString()) ) )
		{			
			val newPred = new BinaryPredicate("=", tgtFieldExpr, newValue)

			if (containsMethodVariables(List(tgtFieldExpr, newValue))) addPredicate(ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), method2predicates, newPred)
			else if (containsObjectFields(List(tgtFieldExpr, newValue))) addPredicate(ctx.getCurClassOrigName(), object2predicates, newPred)
			else addPredicate(ctx.getCurClassOrigName(), static2predicates, newPred)
		}
		
		if (BasicContainerModel.isSupportedListClass(StringUtils.getPlainClassName(fieldType)))
		{
			mapReprListInfos = mapReprListInfos :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtFieldExpr)
		}
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
		
		dataHolder.put("mapRL", mapReprListInfos)
	}
	
	override def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		
		var mapReprListInfos = dataHolder.getOrElse("mapRL", List[(String,String,Expression)]()).asInstanceOf[List[(String,String,Expression)]]

		// we need predicates for all assignments
		if ( ( ! ExpressionUtils.isStackSymbol(newValue) ) && ( ! newValue.contains(tgtVarExpr.toString()) ) )
		{
			val newPred = new BinaryPredicate("=", tgtVarExpr, newValue)

			if (containsMethodVariables(List(tgtVarExpr, newValue))) addPredicate(ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), method2predicates, newPred)
			else if (containsObjectFields(List(tgtVarExpr, newValue))) addPredicate(ctx.getCurClassOrigName(), object2predicates, newPred)
			else addPredicate(ctx.getCurClassOrigName(), static2predicates, newPred)
		}
		
		if (BasicContainerModel.isSupportedListClass(StringUtils.getPlainClassName(tgtVarType)))
		{
			mapReprListInfos = mapReprListInfos :+ (ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExpr)
		}
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
		
		dataHolder.put("mapRL", mapReprListInfos)
	}

	override def visitSwitchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, testedValue : Expression, caseValues : List[Int], caseTargets : List[Int], defaultTarget : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val static2predicates = dataHolder.get("s2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val object2predicates = dataHolder.get("o2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
		val method2predicates = dataHolder.get("m2p").get.asInstanceOf[Map[String, Set[AtomicPredicate]]]
				
		for (i <- 0 to (caseValues.size() - 1))
		{
			val caseValueStr = caseValues(i).toString()
			val caseValueExpr = ExpressionUtils.createExprFromStr(caseValueStr)
			
			if ( ! ExpressionUtils.isStackSymbol(testedValue) )
			{
				val newPred = new BinaryPredicate("=", testedValue, caseValueExpr)

				if (containsMethodVariables(List(testedValue, caseValueExpr))) addPredicate(ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), method2predicates, newPred)
				else if (containsObjectFields(List(testedValue, caseValueExpr))) addPredicate(ctx.getCurClassOrigName(), object2predicates, newPred)
				else addPredicate(ctx.getCurClassOrigName(), static2predicates, newPred)
			}
		}
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
	}	
	
	def isQueriedKey(queriedKeysInfoList : List[(String, String, Expression, Expression)], className : String, methodName : String, mapObj : Expression, keyExpr : Expression) : Boolean =
	{	
		if (containsMethodVariables(mapObj))
		{
			if (queriedKeysInfoList.contains((className, methodName, mapObj, keyExpr))) return true
		}
		else
		{
			for (qk <- queriedKeysInfoList) 
			{
				if ((qk._1 == className) && (qk._3 == mapObj) && (qk._4 == keyExpr)) return true
			}
		}
		
		return false
	}	

	def isQueriedValue(queriedValuesInfoList : List[(String, String, Expression, Expression)], className : String, methodName : String, mapObj : Expression, valueExpr : Expression) : Boolean =
	{	
		if (containsMethodVariables(mapObj))
		{
			if (queriedValuesInfoList.contains((className, methodName, mapObj, valueExpr))) return true
		}
		else
		{
			for (qv <- queriedValuesInfoList) 
			{
				if ((qv._1 == className) && (qv._3 == mapObj) && (qv._4 == valueExpr)) return true
			}
		}
		
		return false
	}
	
	private def containsMethodVariables(expr : Expression) : Boolean =
	{
		return ExpressionUtils.containsMethodVariables(expr) 
	}
	
	private def containsMethodVariables(exprList : List[Expression]) : Boolean =
	{
		for (expr <- exprList)
		{
			if (ExpressionUtils.containsMethodVariables(expr)) return true
		}
		
		return false
	}
	
	private def containsObjectFields(expr : Expression) : Boolean =
	{
		return ExpressionUtils.containsObjectFields(expr)
	}
	
	private def containsObjectFields(exprList : List[Expression]) : Boolean =
	{
		for (expr <- exprList)
		{
			if (ExpressionUtils.containsObjectFields(expr)) return true
		}
		
		return false
	}
}
