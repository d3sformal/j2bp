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

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import common.Constants
import common.Expression
import common.ExpressionUtils


object ContainerAbstractionData
{
	// map expressions (field access expression or local variable name)
	// each map is equipped with class name and method name in which scope it is valid
	var mapExprs = Set[(String, String, Expression)]()
	
	// associates iterator variables with map variables
	// each association has a scope determined by class name and method name
	val assocIterMap : Map[(String, String, Expression), List[Expression]] = new HashMap
	
	// iterator variables associated with maps that represent list containers
	var itersForLists = Set[(String, String, Expression)]()
	
	// next available index (number suffix) for labels -> this should guarantee uniqueness
	private var labelNextIndex = 1

	
	def saveMapExpression(clsName : String, mthName : String, mapExpr : Expression) =
	{
		if (ExpressionUtils.containsMethodVariables(mapExpr)) mapExprs = mapExprs + ( (clsName, mthName, mapExpr) )
		else mapExprs = mapExprs + ( (clsName, "", mapExpr) )		
	}
	
	def isMapExpression(clsName : String, mthName : String, expr : Expression) : Boolean =
	{
		if (ExpressionUtils.containsMethodVariables(expr)) return mapExprs.contains( (clsName, mthName, expr) )
		else return mapExprs.contains( (clsName, "", expr) )				
	}
	
	def isIteratorVariable(clsName : String, mthName : String, varExpr : Expression) : Boolean =
	{
		if (varExpr == BasicContainerModel.TMP_ITER_EXPR) return true

		return assocIterMap.contains( (clsName, mthName, varExpr) )
	}
	
	def saveMapForIterator(clsName : String, mthName : String, iterExpr : Expression, mapExpr : Expression) =
	{
		var assocMaps = assocIterMap.getOrElse( (clsName, mthName, iterExpr), List[Expression]() )

		if ( ! assocMaps.contains(mapExpr) ) assocMaps = assocMaps :+ mapExpr

		assocIterMap.put( (clsName, mthName, iterExpr), assocMaps)
	}
	
	def dropIterator(clsName : String, mthName : String, iterExpr : Expression)
	{
		assocIterMap.remove( (clsName, mthName, iterExpr) )
	}
	
	def getMapsForIterator(clsName : String, mthName : String, iterExpr : Expression) : List[Expression] =
	{
		return assocIterMap.getOrElse( (clsName, mthName, iterExpr), List[Expression]() )
	}

	def isIteratorWithMap(clsName : String, mthName : String, iterExpr : Expression, mapExpr : Expression) : Boolean =
	{
		val assocMaps = assocIterMap.getOrElse( (clsName, mthName, iterExpr), List[Expression]() )
			
		return assocMaps.contains(mapExpr)
	}
	
	def isIteratorForList(clsName : String, mthName : String, varExpr : Expression) : Boolean =
	{
		return itersForLists.contains( (clsName, mthName, varExpr) )
	}
	
	def saveIterForList(clsName : String, mthName : String, iterExpr : Expression) =
	{
		if (ExpressionUtils.containsMethodVariables(iterExpr)) itersForLists = itersForLists + ( (clsName, mthName, iterExpr) )
		else itersForLists = itersForLists + ( (clsName, "", iterExpr) )
	}
	
	def getUniqueLabelName(prefix : String) : String =
	{
		val lblName = prefix + String.valueOf(labelNextIndex)
		
		labelNextIndex += 1
		
		return lblName
	}

}
