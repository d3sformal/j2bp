package j2bp.containers

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes

import common._

import j2bp.Main
import j2bp.Configuration
import j2bp.AbstractionContext
import j2bp.GenUtils

import util.StringUtils


class ContainerMethodCallAbstractor extends j2bp.DefaultMethodCallAbstractor with j2bp.StatementResolver
{
	override def generateLibraryCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, origParamCount : Int, retVarExpr : Expression, retVarType : String, bcIndex : Int) : Unit =
	{
		// unsupported methods are handled in default way (complete abstraction)
		if ( ! BasicContainerModel.isSupportedMethod(className, methodName) )
		{
			super.generateLibraryCall(ctx, mv, className, methodName, isStaticCall, origParamCount, retVarExpr, retVarType, bcIndex)
			return ()
		}
		
		var contPredMngr : ContainerPredicatesManager = Configuration.predicatesMngr.asInstanceOf[ContainerPredicatesManager]

		if (Main.INFO) println("[INFO] container method call = '" + className + "." + methodName + "'")

		if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] class name = '" + className + "', method name = '" + methodName + "', param count = " + origParamCount + ", return variable name = " + retVarExpr + ", return variable type = " + retVarType)

		
		// retrieve method call arguments
		
		val origArgValues = new Array[Expression](origParamCount)
	
		for (i <- 1 to origParamCount)
		{
			origArgValues(origParamCount - i) = ctx.removeExprFromStack()
		}


		// retrieve the target variable pointing to the container object upon which the method is called and all other variables aliased with the target variable
	
		val mainTargetVarExpr = ctx.removeExprFromStack()
		

		// we ignore constructors of supported container classes
		if (methodName == "<init>") return ()

		
		if (Main.INFO)
		{
			print("[INFO] target variable = '" + mainTargetVarExpr + "'")
			if (origArgValues.length > 0) print(", arguments(0) = '" + origArgValues(0) + "'")
			if (origArgValues.length > 1) print(", arguments(1) = '" + origArgValues(1) + "'")
			println(", return variable = '" + retVarExpr + "'")				
		}

		if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] main target variable of container type for updates = " + mainTargetVarExpr)
		

		val absOpSeq = BasicContainerModel.getAbstractOperationSeq(className, methodName, mainTargetVarExpr, origArgValues, retVarExpr, ctx.getCurClassOrigName(), ctx.getCurMethodName(), bcIndex)

		
		// process all abstract operations in the sequence that represents the original method call on target container object 
		
		for (absOp <- absOpSeq)
		{
			val absOperName = absOp._1
			val absTargetObj = absOp._2
			val absArgValues = absOp._3
			val absRetVarExprs = absOp._4
			
			var absRetVarExpr : Expression = null
			if (absRetVarExprs.length > 0) absRetVarExpr = absRetVarExprs(0)
			
			if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] abstract operation name = " + absOperName + ", target variable = " + absTargetObj + ", return variable = " + absRetVarExpr)
			
			if (Main.INFO) 
			{
				print("[INFO] abstract operation = '" + absOperName + "', target variable = '" + absTargetObj + "'")
				if (absArgValues.length > 0) print(", arguments(0) = '" + absArgValues(0) + "'")
				if (absArgValues.length > 1) print(", arguments(1) = '" + absArgValues(1) + "'")
				if (absArgValues.length > 2) print(", arguments(2) = '" + absArgValues(2) + "'")
				println(", return variable = '" + absRetVarExpr + "'")
			}
			
		
			if (absOperName == "increment")
			{
				// this is used only for temporary variables
				Configuration.assignAbstr.generateAbstractStore(ctx, mv, absRetVarExpr, new ArithmeticExpression("+", absArgValues(0), Constants.ONE_EXPR), bcIndex, "int")				
			}
			else if (absOperName == "decrement")
			{
				// this is used only for temporary variables
				Configuration.assignAbstr.generateAbstractStore(ctx, mv, absRetVarExpr, new ArithmeticExpression("-", absArgValues(0), Constants.ONE_EXPR), bcIndex, "int")
			}
			else if (absOperName == "assign")
			{
				// this is used only for writing to temporary variables
				Configuration.assignAbstr.generateAbstractStore(ctx, mv, absArgValues(0), absArgValues(1), bcIndex, "int")				
			}
			else if (absOperName == "label")
			{
				ctx.addLabelWithName(absArgValues(0).toString())
				
				val lbl : Label = ctx.getLabelWithName(absArgValues(0).toString())
				
				mv.visitLabel(lbl)
			}
			else if (absOperName == "goto")
			{
				val lbl : Label = ctx.getLabelWithName(absArgValues(0).toString())
				
				// generate goto instruction
				mv.visitJumpInsn(Opcodes.GOTO, lbl)
			}
			else if (absOperName == "ifgt")
			{
				// construct predicate
				val posPred = new BinaryPredicate(">", absArgValues(0), absArgValues(1))
				val negPred = new BinaryPredicate("<=", absArgValues(0), absArgValues(1))
				
				// check whether the predicate (or its negation) is defined
				val posPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), posPred, -1)
				val negPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), negPred, -1)
				
				// find boolean variable (local or field) for the predicate (or its negation)
				var predVarName = ""
				if (posPredExists) predVarName = ctx.getVariableForPredicate(posPred).get
				if (negPredExists) predVarName = ctx.getVariableForPredicate(negPred).get
				
				if (Main.INFO) 
				{
					if (predVarName == "") println("[INFO] condition predicates do not exist: pos = '" + posPred.toString() + "', neg = '" + negPred.toString() + "'")
				}
			
				// generate byte code (see generating of conditional bytecode instruction for detailed comments)
				if (predVarName != "") GenUtils.generateLoadInstruction(predVarName, mv, ctx)
				else GenUtils.generateChooseBool(mv)
				if (posPredExists || (predVarName == "")) mv.visitInsn(Opcodes.ICONST_1)
				else if (negPredExists) mv.visitInsn(Opcodes.ICONST_0)	
				mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelWithName(absArgValues(2).toString()))
			}
			else if (absOperName == "ifeq")
			{
				// construct predicate
				val posPred = new BinaryPredicate("=", absArgValues(0), absArgValues(1))
				val negPred = new BinaryPredicate("!=", absArgValues(0), absArgValues(1))
				
				// check whether the predicate (or its negation) is defined
				val posPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), posPred, -1)
				val negPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), negPred, -1)
				
				// find boolean variable (local or field) for the predicate (or its negation)
				var predVarName = ""
				if (posPredExists) predVarName = ctx.getVariableForPredicate(posPred).get
				if (negPredExists) predVarName = ctx.getVariableForPredicate(negPred).get
				
				if (Main.INFO)
				{
					if (predVarName == "") println("[INFO] condition predicates do not exist: pos = '" + posPred.toString() + "', neg = '" + negPred.toString() + "'")
				}

				// generate byte code (see generating of conditional bytecode instruction for detailed comments)
				if (predVarName != "") GenUtils.generateLoadInstruction(predVarName, mv, ctx)
				else GenUtils.generateChooseBool(mv)
				if (posPredExists || (predVarName == "")) mv.visitInsn(Opcodes.ICONST_1)
				else if (negPredExists) mv.visitInsn(Opcodes.ICONST_0)	
				mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelWithName(absArgValues(2).toString()))
			}
			else if (absOperName == "ifpred")
			{
				val tgtForm = FormulaUtils.parseFromStr(absArgValues(0).toString()).get
				
				var posTgtPredExists = false
				if (tgtForm.isInstanceOf[AtomicPredicate])
				{
					posTgtPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtForm.asInstanceOf[AtomicPredicate], -1)
				}
				
				var negTgtPredExists = false
				if (tgtForm.isInstanceOf[Negation]) 
				{
					negTgtPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtForm.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate], -1)
				}
				
				// generate byte code (see generating of conditional bytecode instruction for detailed comments)
				
				if (posTgtPredExists) 
				{
					GenUtils.generateLoadInstruction(ctx.getVariableForPredicate(tgtForm.asInstanceOf[AtomicPredicate]).get, mv, ctx)
				}
				else if (negTgtPredExists)
				{
					GenUtils.generateLoadInstruction(ctx.getVariableForPredicate(tgtForm.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]).get, mv, ctx)
				}
				else 
				{
					if (Main.INFO) println("[INFO] condition predicates do not exist: target formula = '" + tgtForm.toString() + "'")

					GenUtils.generateChooseBool(mv)
				}
				
				if (posTgtPredExists) mv.visitInsn(Opcodes.ICONST_1)
				else mv.visitInsn(Opcodes.ICONST_0)
				
				mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelWithName(absArgValues(1).toString()))
			}
			else if (absOperName != "identity")
			{
				val targetAliasingExprs = getPossibleAliasingExpressions(absTargetObj)
				
				if ((absOperName == "put") || (absOperName == "putAhead") || (absOperName == "remove") || (absOperName == "get"))
				{
					// drop assocation between temporary iterator variable and target map variable

					if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] dropped association: iterator = '" + BasicContainerModel.TMP_ITER_EXPR + "', map = '" + absTargetObj + "'")					
					
					ContainerAbstractionData.dropIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), BasicContainerModel.TMP_ITER_EXPR)
				}

				if ((absOperName == "put") || (absOperName == "putAhead"))
				{
					// force backtracking if the maximal container size might be exceeded now
				
					var mgetPred = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(absTargetObj, absArgValues(0)), BasicContainerModel.BOTTOM_EXPR)
					
					var msizePred = new BinaryPredicate(">=", ContainerExpressionUtils.createMapSizeExpr(absTargetObj), ExpressionUtils.createExprFromStr(String.valueOf(BasicContainerModel.maxSize)))
				
					val mgetPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), mgetPred, -1)
					val msizePredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), msizePred, -1)
					
					val nobacktrLabelName = ContainerAbstractionData.getUniqueLabelName("nobacktr")
					
					if (Main.INFO) println("[INFO] condition predicates do not exist: map content = '" + mgetPred.toString() + "', map size = '" + msizePred.toString() + "'")

					if (mgetPredExists) GenUtils.generateLoadInstruction(ctx.getVariableForPredicate(mgetPred).get, mv, ctx)
					else GenUtils.generateChooseBool(mv)
					mv.visitInsn(Opcodes.ICONST_0)
					mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelWithName(nobacktrLabelName))
					
					if (msizePredExists) GenUtils.generateLoadInstruction(ctx.getVariableForPredicate(msizePred).get, mv, ctx)
					else GenUtils.generateChooseBool(mv)
					mv.visitInsn(Opcodes.ICONST_0)
					mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelWithName(nobacktrLabelName))
					
					GenUtils.generateForcedBacktrack(mv)
					
					val lbl : Label = ctx.getLabelWithName(nobacktrLabelName)				
					mv.visitLabel(lbl)
				}

				if (absOperName == "createIterator")
				{
					if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] newly created pair: iterator = '" + absRetVarExpr + "', map = '" + absTargetObj + "'")
					
					// we must save assocation between iterators and maps for the purpose of generating the abstract program
					ContainerAbstractionData.saveMapForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExpr, absTargetObj)

					for (tgtAliasExpr <- targetAliasingExprs)
					{
						if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] newly created pair: iterator = '" + absRetVarExpr + "', map = '" + tgtAliasExpr + "'")
					
						ContainerAbstractionData.saveMapForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExpr, tgtAliasExpr)
					}
				}

				if (absOperName == "get")
				{
					if (retVarType.trim().length() > 0)
					{
						if (BasicContainerModel.isSupportedCollectionClass(StringUtils.getPlainClassName(retVarType)))
						{
							// remember the map variable containing the returned value of a map type
							ContainerAbstractionData.saveMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExpr)
						}
					}
				}
				
				if (absOperName == "keysView")
				{
					// remember the map variable representing the keys view
					ContainerAbstractionData.saveMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExpr)
				}
				
				if (absOperName == "valuesView")
				{
					// remember the map variable representing the keys view
					ContainerAbstractionData.saveMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExpr)
				}

				
				// generate code for the abstract operation
				
				GenUtils.generateBeginAtomic(mv)

				// update all relevant predicates (about map content and variable used to store result)
				// update predicates over the main target variable of the method call and local variables that may be aliased with the main target variable

				var updatePredSet = Set[AtomicPredicate]()
					
				updatePredSet = updatePredSet ++ contPredMngr.getPredicatesToBeUpdated(absOperName, absArgValues, absRetVarExprs, absTargetObj, ctx.getCurClassOrigName(), ctx.getCurMethodName(), bcIndex)
				
				for (tgtAliasExpr <- targetAliasingExprs)
				{
					if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] aliased variable = " + tgtAliasExpr)
						
					updatePredSet = updatePredSet ++ contPredMngr.getPredicatesToBeUpdated(absOperName, absArgValues, absRetVarExprs, tgtAliasExpr, ctx.getCurClassOrigName(), ctx.getCurMethodName(), bcIndex)
				}
				
					
				if (Main.DEBUG) FormulaUtils.printAtomPredSet(updatePredSet, "[DEBUG ContainerMethodCallAbstractor.generateLibraryCall] relevant predicates that may be updated for target variable:")
	
				// set new values of all boolean variables corresponding to relevant predicates that capture effect of the container method
				// we determine the new values according to semantics (effects) of the container method 
					// we consider predicates over the container object, method arguments, and variable used to store return value
					
				if (Main.INFO) println("[INFO] update predicates count = " + updatePredSet.size())
				
					
				// predicates returned from the getPredicatesToBeUpdated method make the left-hand-side list
				resolveContainerMethodCall(ctx, mv, updatePredSet, absOperName, absTargetObj, absArgValues, absRetVarExpr, bcIndex)
				
				GenUtils.generateEndAtomic(mv)
			}
		}
		
		if ( BasicContainerModel.hasMethodUnusedReturnValue(className, methodName) )
		{
			ctx.clearTempReturnVariableName()	
		}
		
		if ( BasicContainerModel.hasMethodReturnValue(className, methodName) )
		{
			// if the original method has a return value then we must put something on the symbolic stack
			// in most cases we use a dummy value that is thrown away (all predicates over the variable used to store the result are updated in the generated abstraction for the method call)
				
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_DUMMY))
		}		
	}
	
	protected def resolveContainerMethodCall(ctx : AbstractionContext, mv : MethodVisitor, updatePredSet : Set[AtomicPredicate], operationName : String, mainTargetExpr : Expression, argValues : Array[Expression], retVarExpr : Expression, bcIndex : Int) =
	{
		// we need a list here
		val updatePredList = updatePredSet.toList


		// solve the container method call for given sets of predicates (with calls of theorem prover) and generate necessary code

		// weakest preconditions for predicates and supported map operations are computed in advance here
		
		val updatePred2PosWP : Map[AtomicPredicate, LogicFormula] = new HashMap
		val updatePred2NegWP : Map[AtomicPredicate, LogicFormula] = new HashMap


		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started creating weakest preconditions for container method call")		

		// create weakest preconditions for operation and each predicate to be updated		
		for (upPred <- updatePredList) 
		{
			createWeakestPreconditions(upPred, operationName, mainTargetExpr, argValues, retVarExpr, updatePred2PosWP, updatePred2NegWP)
			
			if ( ! updatePred2PosWP.contains(upPred) )
			{
				// default positive weakest precondition is the updated predicate
				updatePred2PosWP.put(upPred, upPred)
			}
			
			if ( ! updatePred2NegWP.contains(upPred) )
			{
				// default negative weakest precondition is the negation of the positive one
				updatePred2NegWP.put(upPred, FormulaUtils.negate(updatePred2PosWP.getOrElse(upPred, upPred)))
			}
		}
		
		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished creating weakest preconditions for container method call")

		
		// compute new truth value of each predicate to be updated separately
		// new truth values are stored on the stack
		for (upPred <- updatePredList) 
		{
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started processing next updated predicate")
			
			if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.resolveContainerMethodCall] updated predicate = '" + upPred.toString() + "'")
				
			val upPredPosWP = updatePred2PosWP.get(upPred).get
			val upPredNegWP = updatePred2NegWP.get(upPred).get
		
			
			// create list of predicates that determine result			
			var resultPredSet = getPredicatesToDetermineResult(ctx, upPred, operationName, retVarExpr, upPredPosWP, upPredNegWP, bcIndex)
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] acquired predicates that determine result")
			
			
			if (Main.INFO) println("[INFO] updated predicate = '" + upPred.toString() + "', result predicates count = " + resultPredSet.size())

			if (Main.INFO) println("[INFO] weakest precondition (positive) = '" + upPredPosWP + "'")
			
			if (Main.INFO) FormulaUtils.printAtomPredSet(resultPredSet, "[INFO] result determining predicates:")
			
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(resultPredSet, "[DEBUG ContainerMethodCallAbstractor.resolveContainerMethodCall] relevant predicates that may determine result:")

			
			resolve(ctx, mv, mainTargetExpr, Set[AtomicPredicate](upPred), resultPredSet, updatePred2PosWP, updatePred2NegWP, false)
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished processing updated predicate")
		}

		// save new truth values into boolean variables (they are taken from stack in reversed order)
		for (upPred <- updatePredList.reverse)
		{
			if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.resolveContainerMethodCall] updated predicate = '" + upPred.toString() + "'")

			var updatedPredBoolVar = ctx.getVariableForPredicate(upPred).get

			GenUtils.generateStoreInstruction(updatedPredBoolVar, mv, ctx)
		}
	}
	
	
	private def createWeakestPreconditions(upPred : AtomicPredicate, operationName : String, mainTargetExpr : Expression, argValues : Array[Expression], retVarExpr : Expression, updatePred2PosWP : Map[AtomicPredicate, LogicFormula], updatePred2NegWP : Map[AtomicPredicate, LogicFormula]) =
	{
		if (Main.DEBUG) println("[DEBUG ContainerMethodCallAbstractor.createWeakestPreconditions] updated predicate = '" + upPred.toString() + "'")
		
		val isMapGetPred = upPred.containsFunction(BasicContainerModel.FUNC_MAP_GET)
		val isMapSizePred = upPred.containsFunction(BasicContainerModel.FUNC_MAP_SIZE)
		val isMapOrderPred = upPred.containsFunction(BasicContainerModel.FUNC_MAP_ORDER)
		val isMapKeysPred = upPred.containsFunction(BasicContainerModel.FUNC_MAP_KEYS)
		val isMapValuesPred = upPred.containsFunction(BasicContainerModel.FUNC_MAP_VALUES)
			
		var isMapGetAliasingPred = false
		
		var containsFieldReadOverMapGet = false
		
		if (isMapGetPred)
		{
			if (upPred.isInstanceOf[BinaryPredicate])
			{
				val binUpPred = upPred.asInstanceOf[BinaryPredicate]
				
				if (binUpPred.left.containsFunction(BasicContainerModel.FUNC_MAP_GET) && binUpPred.right.containsFunction(BasicContainerModel.FUNC_MAP_GET)) isMapGetAliasingPred = true
			}
		}
		
		if (isMapGetPred)
		{
			containsFieldReadOverMapGet = ContainerPredicateUtils.containsFieldReadOverMapGet(upPred)
		}
		
		
		// prepare expressions and clauses used in the weakest preconditions

		var mgetFuncExpr : FunctionExpression = null
		var msizeFuncExpr : FunctionExpression = null
		var morderFuncExpr : FunctionExpression = null
		var mkeysFuncExpr : FunctionExpression = null
		var mvaluesFuncExpr : FunctionExpression = null
				
		var mapObj : Expression = null
		var keyExpr : Expression = null
		var mapValueExpr : Expression = null
		var firstPosExpr : Expression = null
		var secondPosExpr : Expression = null
		var szExpr : Expression = null
		var viewObj : Expression = null
		var fieldValueExpr : Expression = null
		
		// these are for aliasing between map elements
		var aliasingMapGetExpr1 : Expression = null
		var aliasingMapGetExpr2 : Expression = null
		var aliasingMapExpr1 : Expression = null;
		var aliasingMapExpr2 : Expression = null;
		var aliasingKeyExpr1 : Expression = null;
		var aliasingKeyExpr2 : Expression = null;
		
		if (isMapGetPred) 
		{
			if (isMapGetAliasingPred)
			{
				val binUpPred = upPred.asInstanceOf[BinaryPredicate]
				
				aliasingMapGetExpr1 = binUpPred.left
				aliasingMapGetExpr2 = binUpPred.right
				
				aliasingMapExpr1 = aliasingMapGetExpr1.asInstanceOf[FunctionExpression].args(1)
				aliasingKeyExpr1 = aliasingMapGetExpr1.asInstanceOf[FunctionExpression].args(2)

				aliasingMapExpr2 = aliasingMapGetExpr2.asInstanceOf[FunctionExpression].args(1)
				aliasingKeyExpr2 = aliasingMapGetExpr2.asInstanceOf[FunctionExpression].args(2)
			}
			else
			{
				mgetFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(upPred, BasicContainerModel.FUNC_MAP_GET).head
				
				mapObj = mgetFuncExpr.args(1)
				keyExpr = mgetFuncExpr.args(2)
				
				if (containsFieldReadOverMapGet)
				{
					val fieldAccessExpr = FormulaUtils.extractOperandWithFunc(upPred, BasicContainerModel.FUNC_MAP_GET)
					fieldValueExpr = FormulaUtils.extractOtherOperandFromBinPred(upPred.asInstanceOf[BinaryPredicate], fieldAccessExpr)
				}
				else
				{
					mapValueExpr = FormulaUtils.extractOtherOperandFromBinPred(upPred.asInstanceOf[BinaryPredicate], mgetFuncExpr)
				}
			}
		}
		
		if (isMapSizePred)
		{
			msizeFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(upPred, BasicContainerModel.FUNC_MAP_SIZE).head
			
			mapObj = msizeFuncExpr.args(1)
			
			szExpr = FormulaUtils.extractOtherOperandFromBinPred(upPred.asInstanceOf[BinaryPredicate], msizeFuncExpr)
		}
		
		if (isMapOrderPred)
		{
			morderFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(upPred, BasicContainerModel.FUNC_MAP_ORDER).head
				
			mapObj = morderFuncExpr.args(1)
			firstPosExpr = morderFuncExpr.args(2)
			secondPosExpr = morderFuncExpr.args(3)
		}
		
		if (isMapKeysPred)
		{
			mkeysFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(upPred, BasicContainerModel.FUNC_MAP_KEYS).head
				
			mapObj = mkeysFuncExpr.args(1)
			viewObj = mkeysFuncExpr.args(2)
		}

		if (isMapValuesPred)
		{
			mvaluesFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(upPred, BasicContainerModel.FUNC_MAP_VALUES).head
				
			mapObj = mvaluesFuncExpr.args(1)
			viewObj = mvaluesFuncExpr.args(2)
		}	
		
		var mgetTargetExpr : Expression = null
		if (argValues.length > 0) mgetTargetExpr = ContainerExpressionUtils.createMapGetExpr(mainTargetExpr, argValues(0))
		
		val msizeTargetExpr = ContainerExpressionUtils.createMapSizeExpr(mainTargetExpr)
		
		// just shortcuts to improve source code readability
		val logicAnyExpr = BasicContainerModel.LOGIC_ANY_EXPR
		val bottomExpr = BasicContainerModel.BOTTOM_EXPR

		var mapsAliasedClause : LogicFormula = null 
		var mapsNotAliasedClause : LogicFormula = null
		
		if (mapObj != null) 
		{
			mapsAliasedClause = new BinaryPredicate("=", mainTargetExpr, mapObj)
			mapsNotAliasedClause = new Negation(new BinaryPredicate("=", mainTargetExpr, mapObj))
		}
		
		var mapEqualsLogicVarClause = new BinaryPredicate("=", mainTargetExpr, BasicContainerModel.LOGIC_MAP_EXPR)

		
		// create actual weakest preconditions
		
		if (operationName == "containsKey")
		{
			if (upPred.containsOperand(retVarExpr) && (upPred.getOperator() == "="))
			{
				val mgetLogicVarExpr = ContainerExpressionUtils.createMapGetExpr(BasicContainerModel.LOGIC_MAP_EXPR, argValues(0))
								
				var exValueFormula = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new Negation(new BinaryPredicate("=", mgetLogicVarExpr, bottomExpr)) ))
				
				var noValueFormula = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new BinaryPredicate("=", mgetLogicVarExpr, bottomExpr) )) 
				
				if (upPred.containsOperand(Constants.ONE_EXPR)) 
				{
					updatePred2PosWP.put(upPred, exValueFormula)
					updatePred2NegWP.put(upPred, noValueFormula)
				}
				else if (upPred.containsOperand(Constants.ZERO_EXPR))
				{
					updatePred2PosWP.put(upPred, noValueFormula)
					updatePred2NegWP.put(upPred, exValueFormula)
				}
			}
		}
		
		if (operationName == "containsValue")
		{
			if (upPred.containsOperand(retVarExpr) && (upPred.getOperator() == "="))
			{
				val mgetLogicVarExpr = ContainerExpressionUtils.createMapGetExpr(BasicContainerModel.LOGIC_MAP_EXPR, BasicContainerModel.LOGIC_KEY_EXPR)
				
				// we use existential quantification over "flk" and "flm" so that both formulas contain the same atomic predicates
				
				var exKeyFormula = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new BinaryPredicate("=", mgetLogicVarExpr, argValues(0)) ))
				
				var noKeyFormula = new Negation(new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new BinaryPredicate("=", mgetLogicVarExpr, argValues(0)) )) )
				
				if (upPred.containsOperand(Constants.ONE_EXPR)) 
				{
					updatePred2PosWP.put(upPred, exKeyFormula)
					updatePred2NegWP.put(upPred, noKeyFormula)
				}
				else if (upPred.containsOperand(Constants.ZERO_EXPR))
				{
					updatePred2PosWP.put(upPred, noKeyFormula)
					updatePred2NegWP.put(upPred, exKeyFormula)
				}
			}
		}
		
		if (operationName == "size")
		{
			val msizeExprStr = ContainerExpressionUtils.createMapSizeExprStr(BasicContainerModel.LOGIC_MAP_EXPR)
			
			if (upPred.containsOperand(retVarExpr))
			{					
				val posWP = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), msizeExprStr) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "get")
		{
			val mgetLogicVarExpr = ContainerExpressionUtils.createMapGetExpr(BasicContainerModel.LOGIC_MAP_EXPR, argValues(0))
			
			var isIdentityMapGetPred = false
			var isFieldReadOverRetVar = false

			if (isMapGetPred && upPred.isInstanceOf[BinaryPredicate])
			{
				if ((upPred.getOperator() == "=") && upPred.containsOperand(retVarExpr)) isIdentityMapGetPred = true
			}
						
			if (upPred.isInstanceOf[BinaryPredicate])
			{
				val binUpPred = upPred.asInstanceOf[BinaryPredicate]
			
				if (ExpressionUtils.isFieldAccessPath(binUpPred.left))
				{
					val fapTgtObj = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(binUpPred.left)					
					if (fapTgtObj == retVarExpr) isFieldReadOverRetVar = true
				}
				
				if (ExpressionUtils.isFieldAccessPath(binUpPred.right))
				{
					val fapTgtObj = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(binUpPred.right)					
					if (fapTgtObj == retVarExpr) isFieldReadOverRetVar = true
				}
			}			
			
			if (isIdentityMapGetPred)
			{
				var posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, mapObj), new BinaryPredicate("=", argValues(0), keyExpr) )), new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mainTargetExpr, argValues(0)), mgetFuncExpr) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isFieldReadOverRetVar)
			{
				val posWP = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), mgetLogicVarExpr.toString()) )) 
					
				updatePred2PosWP.put(upPred, posWP)
			}			
			else if (upPred.containsOperand(retVarExpr) && upPred.containsOperand(Constants.NULL_EXPR))
			{
				var noRealValueFormula = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new Disjunction(List[LogicFormula]( new BinaryPredicate("=", mgetLogicVarExpr, bottomExpr), new BinaryPredicate("=", mgetLogicVarExpr, Constants.NULL_EXPR) )) ))
			
				if (upPred.getOperator() == "=")
				{
					updatePred2PosWP.put(upPred, noRealValueFormula)
				}
				else if (upPred.getOperator() == "!=") 
				{
					updatePred2PosWP.put(upPred, new Negation(noRealValueFormula))
				}
			}
			else if ( upPred.containsOperand(retVarExpr) && upPred.isInstanceOf[BinaryPredicate] && ( (upPred.getOperator() == "=") || (upPred.getOperator() == "!=") || (upPred.getOperator() == "<") || (upPred.getOperator() == "<=") || (upPred.getOperator() == ">") || (upPred.getOperator() == ">=") ) )
			{
				// this correctly handles all the relational operators

				var posWP = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), mgetLogicVarExpr.toString()) )) 
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (upPred.containsExpression(retVarExpr.toString()))
			{
				var posWP = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new BinaryPredicate("=", mgetLogicVarExpr, logicAnyExpr), new Negation(new BinaryPredicate("=", logicAnyExpr, bottomExpr)), FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), BasicContainerModel.VAR_LOGIC_ANY) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "findKey")
		{
			var isIdentityMapGetPred = false

			if (isMapGetPred && upPred.isInstanceOf[BinaryPredicate])
			{
				if ((upPred.getOperator() == "=") && (keyExpr == retVarExpr)) 
				{
					isIdentityMapGetPred = true
				
					val posWP = new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, mapObj), new BinaryPredicate("=", argValues(0), mapValueExpr) ))
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			
			if ( ( ! isIdentityMapGetPred ) && upPred.containsOperand(retVarExpr) )
			{
				val mgetLogicVarExpr = ContainerExpressionUtils.createMapGetExpr(BasicContainerModel.LOGIC_MAP_EXPR, BasicContainerModel.LOGIC_KEY_EXPR)
				
				val posWP = new Conjunction(List[LogicFormula]( mapEqualsLogicVarClause, new BinaryPredicate("=", mgetLogicVarExpr, argValues(0)), FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), BasicContainerModel.VAR_LOGIC_KEY) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}

		if (operationName == "put")
		{
			if (isMapGetPred)
			{
				if (isMapGetAliasingPred)
				{
					val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new BinaryPredicate("=", argValues(0), aliasingKeyExpr1), new BinaryPredicate("=", aliasingMapGetExpr2, argValues(1)) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2), new BinaryPredicate("=", argValues(0), aliasingKeyExpr2), new BinaryPredicate("=", aliasingMapGetExpr1, argValues(1)) )), new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr1)) )), new Disjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr2)) )), upPred )) ))
					
					updatePred2PosWP.put(upPred, posWP)
				}
				else if (containsFieldReadOverMapGet)
				{
					val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new BinaryPredicate("=", argValues(0), keyExpr), FormulaUtils.copyWithReplace(upPred, mgetFuncExpr.toString(), argValues(1).toString()) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )), new Conjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", argValues(0), keyExpr)), upPred )) ))
					
					updatePred2PosWP.put(upPred, posWP)
				}
				else
				{
					val mapUpdateExpr = ContainerExpressionUtils.createMapUpdateExpr(mainTargetExpr, argValues(0), argValues(1)) 
				
					val posWP = FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_MAP, mapUpdateExpr.toString())
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapSizePred)
			{
				val mgetClause = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, argValues(0)), bottomExpr)
				
				val mapResizeExpr = ContainerExpressionUtils.createMapResizeExpr(mainTargetExpr, "+")
			
				val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, mgetClause )), mapsNotAliasedClause )), FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_SIZE, mapResizeExpr.toString()) )), new Conjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new Negation(mgetClause) )), upPred )) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				val firstPosExprEqualKeyClause = new BinaryPredicate("=", firstPosExpr, argValues(0))
				val firstPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", firstPosExpr, argValues(0)))
				val secondPosExprEqualKeyClause = new BinaryPredicate("=", secondPosExpr, argValues(0))
				val secondPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", secondPosExpr, argValues(0)))
				
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, secondPosExprEqualKeyClause, new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, bottomExpr)) )), new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, secondPosExprNotEqualKeyClause )), mapsNotAliasedClause )), upPred )) ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprEqualKeyClause )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				else if ((firstPosExpr == bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))
				}
				else // if ((firstPosExpr != bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprNotEqualKeyClause, new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( secondPosExprEqualKeyClause, new Negation(new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, firstPosExpr), bottomExpr)) )), upPred )) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "putAhead")
		{
			if (isMapGetPred)
			{
				if (isMapGetAliasingPred)
				{
					val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new BinaryPredicate("=", argValues(0), aliasingKeyExpr1), new BinaryPredicate("=", aliasingMapGetExpr2, argValues(1)) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2), new BinaryPredicate("=", argValues(0), aliasingKeyExpr2), new BinaryPredicate("=", aliasingMapGetExpr1, argValues(1)) )), new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr1)) )), new Disjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr2)) )), upPred )) ))
					
					updatePred2PosWP.put(upPred, posWP)
				}
				else if (containsFieldReadOverMapGet)
				{
					val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new BinaryPredicate("=", argValues(0), keyExpr), FormulaUtils.copyWithReplace(upPred, mgetFuncExpr.toString(), argValues(1).toString()) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )), new Conjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", argValues(0), keyExpr)), upPred )) ))
					
					updatePred2PosWP.put(upPred, posWP)
				}
				else
				{
					val mapUpdateExpr = ContainerExpressionUtils.createMapUpdateExpr(mainTargetExpr, argValues(0), argValues(1)) 
				
					val posWP = FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_MAP, mapUpdateExpr.toString())
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapSizePred)
			{
				val mgetClause = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, argValues(0)), bottomExpr)
				
				val mapResizeExpr = ContainerExpressionUtils.createMapResizeExpr(mainTargetExpr, "+")
			
				val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, mgetClause )), mapsNotAliasedClause )), FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_SIZE, mapResizeExpr.toString()) )), new Conjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new Negation(mgetClause) )), upPred )) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				val firstPosExprEqualKeyClause = new BinaryPredicate("=", firstPosExpr, argValues(0))
				val firstPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", firstPosExpr, argValues(0)))
				val secondPosExprEqualKeyClause = new BinaryPredicate("=", secondPosExpr, argValues(0))
				val secondPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", secondPosExpr, argValues(0)))
				
				val firstPosExprEqualKeyBehindClause = new BinaryPredicate("=", firstPosExpr, argValues(2))
				val firstPosExprNotEqualKeyBehindClause = new Negation(new BinaryPredicate("=", firstPosExpr, argValues(2)))
				val secondPosExprEqualKeyBehindClause = new BinaryPredicate("=", secondPosExpr, argValues(2))
				val secondPosExprNotEqualKeyBehindClause = new Negation(new BinaryPredicate("=", secondPosExpr, argValues(2)))
				
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, secondPosExprEqualKeyClause, new Disjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, bottomExpr)), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, argValues(2))) )) )), new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, secondPosExprNotEqualKeyClause, secondPosExprNotEqualKeyBehindClause )), mapsNotAliasedClause )), upPred )) ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprEqualKeyClause, new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, argValues(2)), bottomExpr) )), new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( mapsNotAliasedClause, firstPosExprNotEqualKeyClause )), upPred )) ))
				}
				else if ((firstPosExpr == bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))
				}
				else // if ((firstPosExpr != bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprNotEqualKeyClause, new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( secondPosExprEqualKeyClause, firstPosExprNotEqualKeyBehindClause, new Negation(new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, firstPosExpr), bottomExpr)) )), new Conjunction(List[LogicFormula]( secondPosExprNotEqualKeyBehindClause, upPred )), )) )), new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprEqualKeyClause, secondPosExprEqualKeyBehindClause )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				
				updatePred2PosWP.put(upPred, posWP)
			}				
		}
		
		if (operationName == "remove")
		{
			if (isMapGetPred)
			{
				if (isMapGetAliasingPred)
				{
					val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), upPred )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new BinaryPredicate("=", aliasingMapExpr1, aliasingMapExpr2), new BinaryPredicate("=", aliasingKeyExpr1, aliasingKeyExpr2) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula](	new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr1)), upPred )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", argValues(0), aliasingKeyExpr1), new BinaryPredicate("=", aliasingMapGetExpr2, bottomExpr) )) )) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", argValues(0), aliasingKeyExpr2)), upPred )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", argValues(0), aliasingKeyExpr2), new BinaryPredicate("=", aliasingMapGetExpr1, bottomExpr) )) )) )) ))
							
					updatePred2PosWP.put(upPred, posWP)
				}
				else if (containsFieldReadOverMapGet)
				{
					val posWP = new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( mapsNotAliasedClause, new Negation(new BinaryPredicate("=", argValues(0), keyExpr)) )), upPred ))
										
					updatePred2PosWP.put(upPred, posWP)
				}
				else
				{
					val mapUpdateExpr = ContainerExpressionUtils.createMapUpdateExpr(mainTargetExpr, argValues(0), bottomExpr) 
				
					val posWP = FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_MAP, mapUpdateExpr.toString())  
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapSizePred)
			{
				val mgetClause = new Negation(new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, argValues(0)), bottomExpr))
				
				val mapResizeExpr = ContainerExpressionUtils.createMapResizeExpr(mainTargetExpr, "-")
			
				val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, mgetClause )), mapsNotAliasedClause )), FormulaUtils.copyWithReplace(upPred, BasicContainerModel.FUNC_ARRAY_SIZE, mapResizeExpr.toString()) )), new Conjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new Negation(mgetClause) )), upPred )) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				val firstPosExprEqualKeyClause = new BinaryPredicate("=", firstPosExpr, argValues(0))
				val firstPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", firstPosExpr, argValues(0)))
				val secondPosExprEqualKeyClause = new BinaryPredicate("=", secondPosExpr, argValues(0))
				val secondPosExprNotEqualKeyClause = new Negation(new BinaryPredicate("=", secondPosExpr, argValues(0)))
							
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, secondPosExprNotEqualKeyClause, new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, argValues(0))), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, argValues(0), secondPosExpr)) )), upPred )) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprNotEqualKeyClause, new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, firstPosExpr, argValues(0))), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, argValues(0), bottomExpr)) )), upPred )) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				else if ((firstPosExpr == bottomExpr) && (secondPosExpr == bottomExpr))
				{					
					val mgetClause = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mapObj, argValues(0)), bottomExpr)
					
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mgetClause, upPred )), new Conjunction(List[LogicFormula]( FormulaUtils.negate(mgetClause), new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, argValues(0))), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, argValues(0), bottomExpr)) )) )) )) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				else // if ((firstPosExpr != bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, firstPosExprNotEqualKeyClause, secondPosExprNotEqualKeyClause, new Disjunction(List[LogicFormula]( upPred, new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, firstPosExpr, argValues(0))), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, argValues(0), secondPosExpr)) )) )) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "clear")
		{
			if (isMapGetPred)
			{
				if (isMapGetAliasingPred)
				{
					val posWP = new Disjunction(List[LogicFormula](	new Conjunction(List[LogicFormula]( new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), upPred )), new Conjunction(List[LogicFormula](	new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new BinaryPredicate("=", aliasingMapExpr1, aliasingMapExpr2) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2)), new BinaryPredicate("=", aliasingMapGetExpr2, bottomExpr) )), new Conjunction(List[LogicFormula]( new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr2), new Negation(new BinaryPredicate("=", mainTargetExpr, aliasingMapExpr1)), new BinaryPredicate("=", aliasingMapGetExpr1, bottomExpr) )) ))

					updatePred2PosWP.put(upPred, posWP)
				}
				else if (containsFieldReadOverMapGet)
				{
					val posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))
										
					updatePred2PosWP.put(upPred, posWP)
				}
				else
				{
					val posWP = new Disjunction(List[LogicFormula](new Conjunction(List[LogicFormula]( mapsAliasedClause, new BinaryPredicate("=", mapValueExpr, bottomExpr) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
					
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapSizePred)
			{
				val posWP = new Disjunction(List[LogicFormula]( new Conjunction(List[LogicFormula]( mapsAliasedClause, new BinaryPredicate("=", szExpr, Constants.ZERO_EXPR) )), new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))
				}
				else if ((firstPosExpr == bottomExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Disjunction(List[LogicFormula]( mapsAliasedClause, new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred )) ))
				}
				else // if ((firstPosExpr != bottomExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsNotAliasedClause, upPred ))					
				}
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "keysView")
		{
			if (upPred.containsOperand(retVarExpr) && upPred.isInstanceOf[BinaryPredicate] && (upPred.getOperator() == "="))
			{
				val aliasedViewObj = FormulaUtils.extractOtherOperandFromBinPred(upPred.asInstanceOf[BinaryPredicate], retVarExpr)

				val posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapKeysViewExpr(mainTargetExpr, aliasedViewObj)) 
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapGetPred)
			{
				val exValueFormula = new Negation(new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mainTargetExpr, keyExpr), bottomExpr))
				
				val noValueFormula = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mainTargetExpr, keyExpr), bottomExpr)
				
				if (mapObj == retVarExpr)
				{
					if (mapValueExpr == Constants.ONE_EXPR)
					{
						updatePred2PosWP.put(upPred, exValueFormula)
						updatePred2NegWP.put(upPred, noValueFormula)
					}
					else if ((mapValueExpr == Constants.ZERO_EXPR) || (mapValueExpr == bottomExpr))
					{
						updatePred2PosWP.put(upPred, noValueFormula)
						updatePred2NegWP.put(upPred, exValueFormula)
					}
				}
			}
			else if (isMapSizePred)
			{
				val posWP = FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), mainTargetExpr.toString())
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				val posWP = FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), mainTargetExpr.toString())
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapKeysPred)
			{
				if (viewObj == retVarExpr)
				{
					val posWP = new BinaryPredicate("=", mainTargetExpr, mapObj)
				
					updatePred2PosWP.put(upPred, posWP)
				}
				else if (mapObj == retVarExpr)
				{
					val posWP = Constants.FALSE_PRED
					
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapValuesPred)
			{
				if ((viewObj == retVarExpr) || (mapObj == retVarExpr))
				{
					val posWP = Constants.FALSE_PRED
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
		}
		
		if (operationName == "valuesView")
		{
			if (upPred.containsOperand(retVarExpr) && upPred.isInstanceOf[BinaryPredicate] && (upPred.getOperator() == "="))
			{
				val aliasedViewObj = FormulaUtils.extractOtherOperandFromBinPred(upPred.asInstanceOf[BinaryPredicate], retVarExpr)

				val posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapValuesViewExpr(mainTargetExpr, aliasedViewObj)) 
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapGetPred)
			{
				if (containsFieldReadOverMapGet)
				{
					val mgetLogicKeyExprStr = ContainerExpressionUtils.createMapGetExprStr(mainTargetExpr, BasicContainerModel.LOGIC_KEY_EXPR)
					
					val posWP = FormulaUtils.copyWithReplace(upPred, mgetFuncExpr.toString(), mgetLogicKeyExprStr)

					updatePred2PosWP.put(upPred, posWP)
				}
				else
				{
					val posWP = new BinaryPredicate("=", ContainerExpressionUtils.createMapGetExpr(mainTargetExpr, BasicContainerModel.LOGIC_KEY_EXPR), mapValueExpr)

					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapSizePred)
			{
				val posWP = FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), mainTargetExpr.toString())
				
				updatePred2PosWP.put(upPred, posWP)
			}
			else if (isMapOrderPred)
			{
				var posWP : LogicFormula = null
				
				if (mapObj == retVarExpr)
				{
					if ((firstPosExpr == bottomExpr) && (secondPosExpr != bottomExpr))
					{
						posWP = new BinaryPredicate("=", secondPosExpr, Constants.ONE_EXPR)
					}
					else if ((firstPosExpr != bottomExpr) && (secondPosExpr == bottomExpr))
					{
						posWP = new BinaryPredicate("=", firstPosExpr, msizeTargetExpr)
					}
					else if ((firstPosExpr == bottomExpr) && (secondPosExpr == bottomExpr))
					{
						posWP = new BinaryPredicate("=", msizeTargetExpr, Constants.ZERO_EXPR)
					}
					else // if ((firstPosExpr != bottomExpr) && (secondPosExpr != bottomExpr))
					{
						posWP = new Conjunction(List[LogicFormula]( new BinaryPredicate("<", firstPosExpr, secondPosExpr), new BinaryPredicate("<=", Constants.ONE_EXPR, firstPosExpr), new BinaryPredicate("<=", secondPosExpr, msizeTargetExpr) ))					
					}
					
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapKeysPred)
			{
				if ((viewObj == retVarExpr) || (mapObj == retVarExpr))
				{
					val posWP = Constants.FALSE_PRED
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (isMapValuesPred)
			{
				if (viewObj == retVarExpr)
				{
					val posWP = new BinaryPredicate("=", mainTargetExpr, mapObj)
				
					updatePred2PosWP.put(upPred, posWP)
				}
				else if (mapObj == retVarExpr)
				{
					val posWP = Constants.FALSE_PRED
				
					updatePred2PosWP.put(upPred, posWP)
				}
			}
		}
		
		if (operationName == "createIterator")
		{
			if (isMapOrderPred)
			{
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr == retVarExpr))
				{
					posWP = mapsAliasedClause
				}
				else if ((firstPosExpr == retVarExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsAliasedClause, new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, bottomExpr)) ))						
				}
				else if ((firstPosExpr == retVarExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( mapsAliasedClause, new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, secondPosExpr)) ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == retVarExpr))
				{
					posWP = Constants.FALSE_PRED
				}
				
				if (posWP != null)
				{
					updatePred2PosWP.put(upPred, posWP)
				}
			}
			else if (upPred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(upPred.asInstanceOf[BinaryPredicate], retVarExpr))
			{
				val posWP = Constants.FALSE_PRED
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}
		
		if (operationName == "hasMore")
		{
			var posWP : LogicFormula = null
			
			if ((upPred.getOperator() == "=") && upPred.containsOperand(retVarExpr))
			{
				posWP = new Negation(new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(BasicContainerModel.LOGIC_MAP_EXPR, mainTargetExpr, bottomExpr)))				
			}
			else if ((upPred.getOperator() == "!=") && upPred.containsOperand(retVarExpr))
			{
				posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(BasicContainerModel.LOGIC_MAP_EXPR, mainTargetExpr, bottomExpr))
			}
			
			if (posWP != null)
			{
				if (upPred.containsOperand(Constants.ONE_EXPR))
				{
					updatePred2PosWP.put(upPred, posWP)
				}
				else // if (upPred.containsOperand(Constants.ZERO_EXPR))
				{
					updatePred2PosWP.put(upPred, new Negation(posWP))
				}
			}
		}
		
		if (operationName == "getCurrent")
		{
			var isIdentityMOrderPred = false

			if (isMapOrderPred)
			{
				if ((firstPosExpr == retVarExpr) && (secondPosExpr == mainTargetExpr)) isIdentityMOrderPred = true
			}
			
			if (isIdentityMOrderPred)
			{
				val posWP = Constants.TRUE_PRED
				
				updatePred2PosWP.put(upPred, posWP)
			}		
			else if (upPred.containsOperand(retVarExpr))
			{
				val morderPred = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(BasicContainerModel.LOGIC_MAP_EXPR, BasicContainerModel.LOGIC_KEY_EXPR, mainTargetExpr))
					
				val posWP = new Conjunction(List[LogicFormula]( morderPred, FormulaUtils.copyWithReplace(upPred, retVarExpr.toString(), BasicContainerModel.VAR_LOGIC_KEY) ))
				
				updatePred2PosWP.put(upPred, posWP)
			}
		}		
			
		if (operationName == "moveNext")
		{
			if (isMapOrderPred)
			{
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr == mainTargetExpr))
				{
					posWP = Constants.FALSE_PRED
				}
				else if ((firstPosExpr == mainTargetExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, mainTargetExpr, BasicContainerModel.LOGIC_KEY_EXPR)), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.LOGIC_KEY_EXPR, bottomExpr)) ))
				}
				else if ((firstPosExpr == mainTargetExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = new Conjunction(List[LogicFormula]( new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, mainTargetExpr, BasicContainerModel.LOGIC_KEY_EXPR)), new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.LOGIC_KEY_EXPR, secondPosExpr)), new Negation(new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.BOTTOM_EXPR, secondPosExpr))) ))
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == mainTargetExpr))
				{
					posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, mainTargetExpr, firstPosExpr))
				}
				
				if (posWP != null)
				{
					updatePred2PosWP.put(upPred, posWP)
				}
			}
		}
		
		if (operationName == "moveLast")
		{
			if (isMapOrderPred)
			{
				var posWP : LogicFormula = null
				
				if ((firstPosExpr == bottomExpr) && (secondPosExpr == mainTargetExpr))
				{
					posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, bottomExpr, bottomExpr))
				}
				else if ((firstPosExpr == mainTargetExpr) && (secondPosExpr == bottomExpr))
				{
					posWP = Constants.TRUE_PRED					
				}
				else if ((firstPosExpr == mainTargetExpr) && (secondPosExpr != bottomExpr))
				{
					posWP = Constants.FALSE_PRED
				}
				else if ((firstPosExpr != bottomExpr) && (secondPosExpr == mainTargetExpr))
				{
					posWP = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, firstPosExpr, bottomExpr))
				}
				
				if (posWP != null)
				{
					updatePred2PosWP.put(upPred, posWP)
				}
			}
		}
	}

	
	private def getPredicatesToDetermineResult(ctx : AbstractionContext, upPred : AtomicPredicate, operationName : String, retVarExpr : Expression, posWP : LogicFormula, negWP : LogicFormula, bcIndex : Int) : Set[AtomicPredicate] =
	{
		val atomicPredsWP = FormulaUtils.extractAtomicPredicates(posWP) ++ FormulaUtils.extractAtomicPredicates(negWP)
	
		val inputPredSet = Configuration.predicatesMngr.getPredicatesOverStaticFields(ctx.getCurClassOrigName()) ++ Configuration.predicatesMngr.getPredicatesOverObjectFields(ctx.getCurClassOrigName()) ++ Configuration.predicatesMngr.getPredicatesOverMethodVariables(ctx.getCurClassOrigName(), ctx.getCurMethodName(), bcIndex)
				
		var resultPreds = Set[AtomicPredicate]()
	
		
		// add each predicate available from the input set that is equal to some atomic predicate in the weakest precondition
		for (atomPredWP <- atomicPredsWP)
		{
			if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), atomPredWP, bcIndex)) resultPreds = resultPreds + atomPredWP
		}


		// take all predicates in the input set that match some atomic predicate in the weakest precondition (e.g., when there is a constant value in the input predicate at the same location as a logic variable in the atomic predicate from the weakest precondition)
		
		var candidateMatchingPreds = atomicPredsWP

		// we do not have to consider the negative weakest precondition explicitly because it contains the same atomic predicates
		val varexpr2matches = Configuration.predicateSemModel.findMatchingExpressions(posWP, inputPredSet, ctx.getCurClassOrigName(), ctx.getCurMethodName())
		
		for ( (varExpr, matchingExprSet) <- varexpr2matches )
		{
			if (Main.DEBUG) 
			{
				println("[DEBUG ContainerMethodCallAbstractor.getPredicatesToDetermineResult] variable = " + varExpr)
				ExpressionUtils.printSet(matchingExprSet, "[DEBUG ContainerMethodCallAbstractor.getPredicatesToDetermineResult] matching expressions:")
			}				
			
			var newCandidatePreds = Set[AtomicPredicate]()
		
			for (candidatePred <- candidateMatchingPreds)
			{
				for (matchExpr <- matchingExprSet)
				{
					newCandidatePreds = newCandidatePreds + FormulaUtils.copyWithReplace(candidatePred, varExpr.toString(), matchExpr.toString())
				}
			}
			
			candidateMatchingPreds = candidateMatchingPreds ++ newCandidatePreds
		}

		if (Main.DEBUG) FormulaUtils.printAtomPredList(candidateMatchingPreds, "[DEBUG ContainerMethodCallAbstractor.getPredicatesToDetermineResult] candidate matching predicates:")
			
		for (candidatePred <- candidateMatchingPreds)
		{
			if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), candidatePred, bcIndex)) resultPreds = resultPreds + candidatePred
		}
			
	
		// for each atomic predicate in the weakest precondition, extract all operands that do not contain any logic variables and then add to the list all input predicates over some of these operands (but ignore identity predicates of the form v = v)
		// if the weakest precondition contains an aliasing predicate m_1 = m_2 for two map variables m_1,m_2 and no atomic predicates over mget(map,m_1,...) or mget(map,m_2,...), then add only the predicate m_1 = m_2 and ignore operands of the aliasing predicate further	
	
		for (atomPredWP <- atomicPredsWP)
		{
			var ignorePred = false
			
			if (atomPredWP.isInstanceOf[BinaryPredicate] && (atomPredWP.asInstanceOf[BinaryPredicate].left == atomPredWP.asInstanceOf[BinaryPredicate].right)) ignorePred = true
			
			if ( ! ignorePred )
			{
				var ignoreOperands = false
				
				if (atomPredWP.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(atomPredWP.asInstanceOf[BinaryPredicate]))
				{
					val binAtomPredWP = atomPredWP.asInstanceOf[BinaryPredicate]
			
					if (ContainerAbstractionData.isMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), binAtomPredWP.left) || ContainerAbstractionData.isMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), binAtomPredWP.right))
					{
						var existsPredOverMapGet = false
						
						val mgetExprStr1 = ContainerExpressionUtils.createMapGetExprStrFragment(binAtomPredWP.left)
						val mgetExprStr2 = ContainerExpressionUtils.createMapGetExprStrFragment(binAtomPredWP.right)
						
						for (apred <- atomicPredsWP)
						{
							if (apred.containsExpression(mgetExprStr1) || apred.containsExpression(mgetExprStr2)) existsPredOverMapGet = true
						}
						
						if ( ! existsPredOverMapGet ) ignoreOperands = true
					}
				}
				
				if ( ! ignoreOperands )
				{
					val atomPredOperands = FormulaUtils.extractOperands(atomPredWP)
					
					for (operand <- atomPredOperands if (operand != BasicContainerModel.BOTTOM_EXPR))
					{
						if ( ! ExpressionUtils.containsLogicVariables(operand) )
						{
							resultPreds = resultPreds ++ Configuration.predicatesMngr.getPredicatesWithOperand(ctx.getCurClassOrigName(), ctx.getCurMethodName(), operand, bcIndex)
						}
					}
				}
			}
		}
		
		
		//  take predicates that express equality of variables used as arguments for the functions "mget" and "morder" (only arguments that represent keys) with other expressions
		
		var keyVarExprs = Set[Expression]()
		
		for (atomPredWP <- atomicPredsWP)
		{
			val atomPredFuncExprs = FormulaUtils.extractFunctionExpressionsRecursively(atomPredWP)
			
			for (funcExpr <- atomPredFuncExprs)
			{
				if (funcExpr.name == BasicContainerModel.FUNC_MAP_GET)
				{
					if (ExpressionUtils.isVariableName(funcExpr.args(2))) keyVarExprs = keyVarExprs + funcExpr.args(2)
				}
				
				if (funcExpr.name == BasicContainerModel.FUNC_MAP_ORDER)
				{
					if ( ExpressionUtils.isVariableName(funcExpr.args(2)) && ( ! ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), funcExpr.args(2)) ) ) keyVarExprs = keyVarExprs + funcExpr.args(2)
					
					if ( ExpressionUtils.isVariableName(funcExpr.args(3)) && ( ! ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), funcExpr.args(3)) ) ) keyVarExprs = keyVarExprs + funcExpr.args(3)					
				}
			}
		}
		
		for (keyExpr <- keyVarExprs if (keyExpr != BasicContainerModel.BOTTOM_EXPR))
		{
			val keyExprPreds = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), keyExpr.toString(), bcIndex)
					
			for (pred <- keyExprPreds)
			{
				if (pred.containsOperand(keyExpr) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
			}
		}


		// if the weakest precondition contains an atomic predicate of the form "mget(mupdate(map,m',k',v'),m,k) = v", then we must take all input predicates with the function "mget" over map variables m,m' and key expressions k,k', equality predicates over k,k',v', and aliasing predicate m = m' 
		
		for (atomPredWP <- atomicPredsWP)
		{
			val mgetFuncExprs = FormulaUtils.extractFunctionExpressionsTopLevel(atomPredWP, BasicContainerModel.FUNC_MAP_GET)
			
			for (mgetFunc <- mgetFuncExprs)
			{
				// we have "update" inside "mget"
				if (mgetFunc.args(0).isInstanceOf[FunctionExpression] && (mgetFunc.args(0).asInstanceOf[FunctionExpression].name == Constants.ARRAY_UPDATE_OPER))
				{
					val mupdateFunc = mgetFunc.args(0).asInstanceOf[FunctionExpression]
					
					val mapVar1 = mgetFunc.args(1)
					val keyExpr1 = mgetFunc.args(2)
					val mapVar2 = mupdateFunc.args(1)
					val keyExpr2 = mupdateFunc.args(2)
					val newValue = mupdateFunc.args(3)
		
					val mgetExprStr1 = ContainerExpressionUtils.createMapGetExprStr(mapVar1, keyExpr1)
					val mgetExprStr2 = ContainerExpressionUtils.createMapGetExprStr(mapVar2, keyExpr2)
					
					resultPreds = resultPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), mgetExprStr1.toString(), bcIndex)
					
					resultPreds = resultPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), mgetExprStr2.toString(), bcIndex)
			
					for (pred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), keyExpr1.toString(), bcIndex))
					{
						if (pred.containsOperand(keyExpr1) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
					}
					
					for (pred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), keyExpr2.toString(), bcIndex))
					{
						if (pred.containsOperand(keyExpr2) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
					}
					
					if (newValue != BasicContainerModel.BOTTOM_EXPR)
					{
						for (pred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), newValue.toString(), bcIndex))
						{
							if (pred.containsOperand(newValue) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
						}
					}
					
					for (pred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), mapVar1.toString(), bcIndex))
					{
						if (pred.containsOperand(mapVar1) && pred.containsOperand(mapVar2) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
					}
				}
			}
		}
		
		
		// do similar thing as above for atomic predicates of the form "msize(mresize(msz,m',sz'),m) = sz"
	
		for (atomPredWP <- atomicPredsWP)
		{
			val msizeFuncExprs = FormulaUtils.extractFunctionExpressionsTopLevel(atomPredWP, BasicContainerModel.FUNC_MAP_SIZE)
			
			for (msizeFunc <- msizeFuncExprs)
			{
				// we have "update" inside "msize"
				if (msizeFunc.args(0).isInstanceOf[FunctionExpression] && (msizeFunc.args(0).asInstanceOf[FunctionExpression].name == Constants.ARRAY_UPDATE_OPER))
				{
					val mresizeFunc = msizeFunc.args(0).asInstanceOf[FunctionExpression]
					
					val mapVar1 = msizeFunc.args(1)
					val mapVar2 = mresizeFunc.args(1)
					val newSize = mresizeFunc.args(2)
		
					val msizeExprStr1 = ContainerExpressionUtils.createMapSizeExprStr(mapVar1)
					val msizeExprStr2 = ContainerExpressionUtils.createMapSizeExprStr(mapVar2)
					
					resultPreds = resultPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), msizeExprStr1.toString(), bcIndex)
					
					resultPreds = resultPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), msizeExprStr2.toString(), bcIndex)
			
					val newSizePreds = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), newSize.toString(), bcIndex)
					for (pred <- newSizePreds)
					{
						if (pred.containsOperand(newSize) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
					}
					
					for (pred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), mapVar1.toString(), bcIndex))
					{
						if (pred.containsOperand(mapVar1) && pred.containsOperand(mapVar2) && ((pred.getOperator() == "=") || (pred.getOperator() == "!="))) resultPreds = resultPreds + pred
					}
				}
			}
		}


		// take equality predicates over operands of the form "msize(msz,m)" in already collected result-determining predicates
		
		var newMapSizeOperands = Set[Expression]()
		
		for (resPred <- resultPreds)
		{
			for (operand <- FormulaUtils.extractOperands(resPred) if operand.isInstanceOf[FunctionExpression])
			{
				val funcOperand = operand.asInstanceOf[FunctionExpression]

				if (funcOperand.name == BasicContainerModel.FUNC_MAP_SIZE) newMapSizeOperands = newMapSizeOperands + funcOperand
			}
		}

		for (msizeOperand <- newMapSizeOperands)
		{
			val msizePreds = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), msizeOperand.toString(), bcIndex)

			resultPreds = resultPreds ++ msizePreds
		}

				
		// set of result determining predicates cannot be empty
		if (resultPreds.size == 0) 
		{
			// predicate representing "true"
			resultPreds = resultPreds + Constants.TRUE_PRED
		}

		
		// drop all predicates over the variable expression used to store the return value
		if (retVarExpr != null)
		{
			resultPreds = resultPreds.filterNot(p => p.containsExpression(retVarExpr.toString()))
		}
		

		return resultPreds
	}
	
	
	private def getPossibleAliasingExpressions(targetObj : Expression) : Set[Expression] =
	{
		var aliasingExprs = Set[Expression]()
		
		val aliasingPreds = Configuration.predicatesMngr.asInstanceOf[ContainerPredicatesManager].getAliasingPredicates()
				
		for (aliasPred <- aliasingPreds)
		{
			val binAliasPred = aliasPred.asInstanceOf[BinaryPredicate]
			
			var aliasExpr : Expression = null
			
			if (binAliasPred.left == targetObj) aliasExpr = binAliasPred.right
			else if (binAliasPred.right == targetObj) aliasExpr = binAliasPred.left
			
			if (aliasExpr != null) aliasingExprs = aliasingExprs + aliasExpr
		}
		
		return aliasingExprs
	}			
	

	override def postprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String) =
	{
		if ( BasicContainerModel.hasMethodReturnValue(ownerClassName, tgtMethodName) )
		{
			// if the original method has a return value then we must put something on the symbolic stack 
			// in most cases we use a dummy value that is thrown away (all predicates over the variable used to store result are updated in the abstraction for the method call)
		
			if ( ( ! BasicContainerModel.hasMethodUnusedReturnValue(ownerClassName, tgtMethodName) ) && (ctx.getTempReturnVariableName() != "") )
			{
				// store implicit variable used to store result on stack for further processing
				ctx.addExprToStack(new Expression(ctx.getTempReturnVariableName()))
			}
			else 
			{
				ctx.addExprToStack(new Expression(Constants.STACK_ELEM_DUMMY))
			}
		}
	}
}
