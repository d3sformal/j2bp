package j2bp.containers

import scala.math._
import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label

import common.Constants
import common.Expression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.FormulaUtils
import common.ExpressionUtils

import util.StringUtils

import j2bp.Main
import j2bp.Configuration
import j2bp.AbstractionContext
import j2bp.GenUtils


class ContainerAssignmentAbstractor extends j2bp.DefaultAssignmentAbstractor
{
	override def generateAbstractStore(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, srcValueExpr : Expression, bcIndex : Int, tgtVarType : String) : Unit =
	{
		// we assume that value of the tgtVarType variable has the form "Ljava/util/List;"
		var tgtVarClassName = StringUtils.getPlainClassName(tgtVarType)
		
		if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] target variable type = '" + tgtVarClassName + "'")
		
		// call super.generateAbstractStore for variables that are not containers
		if ( ! BasicContainerModel.isSupportedClass(tgtVarClassName) )
		{
			super.generateAbstractStore(ctx, mv, tgtVarExpr, srcValueExpr, bcIndex, tgtVarType)
			return ()
		}

		if (Main.INFO) println("[INFO] assignment: target variable = " + tgtVarExpr + ", source expression = " + srcValueExpr)
		
		
		// remember the map variable
		ContainerAbstractionData.saveMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExpr)
		
		
		val tgtMapGetExprStrFrag = ContainerExpressionUtils.createMapGetExprStrFragment(tgtVarExpr)
		val tgtMapOrderBotBotExpr = ContainerExpressionUtils.createMapOrderExpr(tgtVarExpr, BasicContainerModel.BOTTOM_EXPR, BasicContainerModel.BOTTOM_EXPR)
		val tgtMapOrderExprStrFrag = ContainerExpressionUtils.createMapOrderExprStrFragment(tgtVarExpr)
		val tgtMapSizeExpr = ContainerExpressionUtils.createMapSizeExpr(tgtVarExpr)
		
		
		if (srcValueExpr.toString() == Constants.STACK_ELEM_NEWOBJ)
		{
			// source value is reference to a newly created object ("[newobj]")

			GenUtils.generateBeginAtomic(mv)
			
			// find all predicates that involve the target variable (and its aliases)
			var tgtVarPredSet = findPredicatesOverVariable(ctx, tgtVarExpr, bcIndex)

			for ( tgtPred <- tgtVarPredSet )
			{
				if (Main.INFO) println("[INFO] current target predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get

				if (ContainerPredicateUtils.containsFieldReadOverMapGet(tgtPred) && tgtPred.containsExpression(tgtMapGetExprStrFrag))
				{
					// all predicates that express field value of object stored in the map are set to false
					
					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if (tgtPred.isInstanceOf[BinaryPredicate] && tgtPred.asInstanceOf[BinaryPredicate].left.containsFunction(BasicContainerModel.FUNC_MAP_GET) && tgtPred.asInstanceOf[BinaryPredicate].right.containsFunction(BasicContainerModel.FUNC_MAP_GET))
				{
					// aliasing predicates between two map elements are set to true
					
					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = true")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)					
				}				
				else if ( (tgtPred.getOperator() == "=") && tgtPred.containsExpression(tgtMapGetExprStrFrag) && tgtPred.containsOperand(BasicContainerModel.BOTTOM_EXPR) )
				{
					// all predicates that express absence of some key-value pair in the map (i.e., that contain atomic predicate "mget(m,..) = bot") are set to true

					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = true")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( (tgtPred.getOperator() == "!=") && tgtPred.containsExpression(tgtMapGetExprStrFrag) && tgtPred.containsOperand(BasicContainerModel.BOTTOM_EXPR) )
				{
					// all predicates that express presence of some key-value pair in the map (i.e., that contain atomic predicate "mget(m,..) != bot") are set to false

					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( (tgtPred.getOperator() == "=") && tgtPred.containsExpression(tgtMapGetExprStrFrag) )	
				{
					// all predicates that express presence of some key-value pair in the map (i.e., that contain atomic predicate "mget(m,..) = v") are set to false

					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( (tgtPred.getOperator() == "!=") && tgtPred.containsExpression(tgtMapGetExprStrFrag) )	
				{
					// all predicates that express absence of some key-value pair in the map (i.e., that contain atomic predicate "mget(m,..) != v") are set to true

					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = true")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( tgtPred.containsOperand(tgtMapOrderBotBotExpr) )
				{
					// all predicates that express iteration order for an empty map (i.e., that contain a clause with the function expression "morder(m,bot,bot)") are set to true
					
					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = true")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( tgtPred.containsExpression(tgtMapOrderExprStrFrag) )	
				{
					// all predicates that express iteration order for a non-empty map (i.e., that contain a clause with the function call "morder(m,..)" that has arguments different than the "bottom" symbol) are set to false

					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( (tgtPred.getOperator() == "=") && tgtPred.containsOperand(tgtMapSizeExpr) && tgtPred.containsOperand(Constants.ZERO_EXPR) )
				{
					// all predicates with clause "size(m) = 0" operator are set to true
					
					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = true")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}				
				else if ( tgtPred.containsOperand(tgtMapSizeExpr) ) // && tgtPred does not contain "0"
				{
					// all predicates with clause "size(m) = x", where x != 0, are set to false
					
					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( tgtPred.containsExpression(BasicContainerModel.FUNC_MAP_KEYS + "(" + BasicContainerModel.FUNC_ARRAY_KEYS) && tgtPred.containsExpression(tgtVarExpr.toString()) )
				{
					// all predicates with function "mkeys", where one of the arguments is target map
				
					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if ( tgtPred.containsExpression(BasicContainerModel.FUNC_MAP_VALUES + "(" + BasicContainerModel.FUNC_ARRAY_VALUES) && tgtPred.containsExpression(tgtVarExpr.toString()) )
				{
					// all predicates with function "mvalues", where one of the arguments is target map
				
					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)

					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}				
				else if (tgtPred.isInstanceOf[BinaryPredicate] && (tgtPred.getOperator() == "=") && tgtPred.containsOperand(tgtVarExpr))				
				{
					// all predicates of the form 'v = e' (aliasing) are set to 'false'

					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)
					
					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else
				{
					// all other predicates over target variable are set to non-deterministic boolean value

					// generate call to Verify.getBoolean
					GenUtils.generateChooseBool(mv)
						
					if (Main.INFO) println("[INFO] variable name = '" + tgtPredBoolVar + "', new value = choose bool")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
			}
			
			GenUtils.generateEndAtomic(mv)
		}
		else if (ExpressionUtils.isVariableName(srcValueExpr))
		{
			// given source expression (srcValueExpr) is only a variable name (e.g., we have an assignment "m := m2")
			
			GenUtils.generateBeginAtomic(mv)
			
			// find all predicates involving the source variable			
			var srcVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), srcValueExpr.toString(), -1)

			// find all predicates that involve the target variable (and its aliases)
			var tgtVarPredSet = findPredicatesOverVariable(ctx, tgtVarExpr, bcIndex)
			
			var tgtVarPredList : List[AtomicPredicate] = tgtVarPredSet.toList
			
			
			// for each predicate over the target variable, set the value of boolean variable to the truth value of matching predicate over the source variable
				// matching predicate: "tgtVarExpr" syntactically replaced with "srcVarName"
			// for unmatched predicates over target variable, set their value to unknown (non-deterministic boolean value)
				// this includes predicates with function expressions over the target variable, for which does not exist matching predicates over source variable
			// special case: all predicates of the form 'm = m2' (aliasing) are set to 'true' automatically
			// no need for theorem prover calls in this case
			
			// load new values on the stack
			for ( tgtPred <- tgtVarPredList )
			{
				if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
			
				if (tgtPred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(tgtPred.asInstanceOf[BinaryPredicate], tgtVarExpr, srcValueExpr))
				{
					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)
					
					if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] target variable name = '" + tgtPredBoolVar + "', new value = true")
				}
				else
				{					
					val matcherSrcPred = FormulaUtils.copyWithReplace(tgtPred, tgtVarExpr.toString(), srcValueExpr.toString())
				
					val matchingSrcPredOpt = srcVarPredSet.find(_ == matcherSrcPred)
										
					if (matchingSrcPredOpt == None)
					{
						// generate call to Verify.getBoolean
						GenUtils.generateChooseBool(mv)
						
						if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = choose bool")						
					}
					else
					{
						if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] matched predicate = '" + matcherSrcPred + "'")
						
						val srcPredBoolVar = ctx.getVariableForPredicate(matchingSrcPredOpt.get).get
						
						if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] source variable name = " + srcPredBoolVar)
						
						GenUtils.generateLoadInstruction(srcPredBoolVar, mv, ctx)						
					}
				}
			}
			
			// save new values into variables (they are taken from stack in reversed order)
			for ( tgtPred <- tgtVarPredList.reverse )
			{
				if (Main.DEBUG) println("[DEBUG ContainerAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
				
				GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
			}
			
			GenUtils.generateEndAtomic(mv)
		}		
		else
		{
			// handle all other cases in a default way
			
			super.generateAbstractStore(ctx, mv, tgtVarExpr, srcValueExpr, bcIndex, tgtVarType)
			return ()
		}	
	}
}
