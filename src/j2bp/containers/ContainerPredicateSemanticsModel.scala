package j2bp.containers

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.immutable.List
import scala.collection.JavaConversions._
import scala.util.matching.Regex

import common._

import util.StringUtils

import j2bp.Configuration
import j2bp.Main


class ContainerPredicateSemanticsModel extends j2bp.DefaultPredicateSemanticsModel
{
	override def derivePermanentSupportingFormulas() = 
	{
		// create a list of all predicates
		
		var allPredicates = Configuration.predicatesMngr.getAllPredicates()
		
		
		// collect all expressions that identify a map object 
		// they are used (1) as the second argument to functions "mget", "msize", and "morder", and (2) as second and third argument to functions "mkeys" and "mvalues"
		// we must also take variables aliased with map expressions
		
		var mapExprSet = Set[Expression]()
		
		for (pred <- allPredicates)
		{			
			val predFuncList = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (func <- predFuncList)
			{
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_GET)) 
				{
					mapExprSet = mapExprSet + func.args(1)
				}
				
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_SIZE))
				{
					mapExprSet = mapExprSet + func.args(1)
				}
				
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_ORDER)) 
				{
					mapExprSet = mapExprSet + func.args(1)
				}
				
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_KEYS)) 
				{
					mapExprSet = mapExprSet + func.args(1)
					mapExprSet = mapExprSet + func.args(2)
				}
				
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_VALUES)) 
				{
					mapExprSet = mapExprSet + func.args(1)
					mapExprSet = mapExprSet + func.args(2)
				}
			}			
		}
		
		for (aliasPred <- Configuration.predicatesMngr.getAliasingPredicates())
		{
			val binAliasPred = aliasPred.asInstanceOf[BinaryPredicate]
			
			var aliasedMapExprs = Set[Expression]()
			
			for (mapExpr <- mapExprSet)
			{
				if (binAliasPred.left == mapExpr) aliasedMapExprs = aliasedMapExprs + binAliasPred.right
				if (binAliasPred.right == mapExpr) aliasedMapExprs = aliasedMapExprs + binAliasPred.left
			}
			
			mapExprSet = mapExprSet ++ aliasedMapExprs
		}
		
		
		// collect all expressions that identify an iterator object (that are used as the third or fourth argument to function "morder" and they cannot be arguments to "mget")
		
		var iterExprSet = Set[Expression]()
		
		// collect all third and fourth arguments to function "morder" that are not "bot"
		for (pred <- allPredicates)
		{			
			val predFuncList = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (func <- predFuncList)
			{
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_ORDER)) 
				{
					val thirdArg = func.args(2)
					val fourthArg = func.args(3)
					
					if (thirdArg != BasicContainerModel.BOTTOM_EXPR)
					{
						iterExprSet = iterExprSet + thirdArg
					}
					 
					if (fourthArg != BasicContainerModel.BOTTOM_EXPR)
					{
						iterExprSet = iterExprSet + fourthArg
					}
				}
			}
		}
		
		// remove all items from the iterExprList that are keys
		for (pred <- allPredicates)
		{			
			val predFuncList = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (func <- predFuncList)
			{
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_GET)) 
				{
					iterExprSet = iterExprSet.filterNot(e => e == func.args(2))
				}
			}
		}


		// collect all expressions that represent keys (that are used as the third argument to function "mget")
		
		var keyExprSet = Set[Expression]()
		
		// collect all third arguments to function "mget"
		for (pred <- allPredicates)
		{			
			val predFuncList = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (func <- predFuncList)
			{
				if (func.name.startsWith(BasicContainerModel.FUNC_MAP_GET)) 
				{
					keyExprSet = keyExprSet + func.args(2)
				}
			}
		}		
		
		
		// value of any variable that represents a map object must differ from the values of all integer expressions that the results of "mget" are compared with
		
		for (pred <- allPredicates)
		{			
			if (pred.containsFunction(BasicContainerModel.FUNC_MAP_GET)) 
			{
				val predOperands = FormulaUtils.extractOperands(pred)
				
				for (operand <- predOperands)
				{
					if ( ! ExpressionUtils.isFunctionExpr(operand, BasicContainerModel.FUNC_MAP_GET) )
					{
						var valueExpr = operand
					
						if ( ( ! mapExprSet.contains(valueExpr) ) && ExpressionUtils.isVariableName(valueExpr) )
						{
							// we have a variable that does not refer to a map object -> create inequality support predicate with each expression that refers to a map object
							
							for (mapExpr <- mapExprSet) 
							{
								addSupportFormula( new BinaryPredicate("!=", valueExpr, mapExpr) )
							}
						}
					}
				}
			}
		}
		
		
		// value of any variable that represents an iterator object must differ from the values of all integer expressions that represent keys
		
		for (iterExpr <- iterExprSet)
		{
			for (keyExpr <- keyExprSet)
			{
				addSupportFormula( new BinaryPredicate("!=", iterExpr, keyExpr) )
			}
		}
		
		
		// value of any variable that represents a map object must differ from the values of all integer expressions that represent iterator objects
		
		for (mapExpr <- mapExprSet)
		{
			for (iterExpr <- iterExprSet)
			{
				addSupportFormula( new BinaryPredicate("!=", mapExpr, iterExpr) )
			}
		}
		
		
		// add support predicates of the form "not (mget(m,k) = v) => mget(m,k) = bot" where antecedent of the implication is conjunction over all possible 'v' and 'not (mget(m,k) != bot)'
		// add support predicates of the form "(mget(m,k) = v) => not (mget(m,k) = bot)" where antecedent of the implication is disjunction over all possible 'v'
		// we consider only such 'v' that is a variable name or a constant expression
		
		for ( pred <- allPredicates )
		{
			val predFuncList = FormulaUtils.extractFunctionOperands(pred)
			
			for (funcExpr <- predFuncList)
			{
				if ((funcExpr.name == BasicContainerModel.FUNC_MAP_GET) && ( ! funcExpr.contains(Constants.QUANT_VAR_PREFIX) ) && ( ! funcExpr.contains(Constants.LOGIC_VAR_PREFIX) ) )
				{
					var tgtMapObj = funcExpr.args(1)
					var tgtKeyExpr = funcExpr.args(2)
					
					val mgetExpr = ContainerExpressionUtils.createMapGetExpr(tgtMapObj, tgtKeyExpr)

					var absentKeyValueForms = List[LogicFormula]()
					var presentKeyValueForms = List[LogicFormula]()

					for (mgetPred <- allPredicates if mgetPred.containsOperand(mgetExpr.toString()))
					{
						val mgetPredOperands = FormulaUtils.extractOperands(mgetPred)
						
						for (operand <- mgetPredOperands)
						{
							if ( ! operand.contains(mgetExpr.toString()) )
							{
								var valueExpr = operand
						
								if ((valueExpr != BasicContainerModel.BOTTOM_EXPR) && (ExpressionUtils.isVariableName(valueExpr) || ExpressionUtils.isConstantExpression(valueExpr)))
								{
									absentKeyValueForms = absentKeyValueForms :+ new Negation(new BinaryPredicate("=", mgetExpr, valueExpr))
									presentKeyValueForms = presentKeyValueForms :+ new BinaryPredicate("=", mgetExpr, valueExpr)
								}
							}
						}
					}
					
					absentKeyValueForms = absentKeyValueForms :+ new Negation(new BinaryPredicate("!=", mgetExpr, BasicContainerModel.BOTTOM_EXPR))
					
					if (absentKeyValueForms.size > 0)
					{
						var absentKeyValueConjunct : LogicFormula = null
						if (absentKeyValueForms.size > 1) absentKeyValueConjunct = new Conjunction(absentKeyValueForms)
						else absentKeyValueConjunct = absentKeyValueForms.head

						val newForm = new Implication(absentKeyValueConjunct, new BinaryPredicate("=", mgetExpr, BasicContainerModel.BOTTOM_EXPR))

						addSupportFormula(newForm)
					}
					
					if (presentKeyValueForms.size > 0)
					{
						var presentKeyValueDisjunct : LogicFormula = null
						if (presentKeyValueForms.size > 1) presentKeyValueDisjunct = new Disjunction(presentKeyValueForms)
						else presentKeyValueDisjunct = presentKeyValueForms.head

						val newForm = new Implication(presentKeyValueDisjunct, new Negation(new BinaryPredicate("=", mgetExpr, BasicContainerModel.BOTTOM_EXPR)))

						addSupportFormula(newForm)
					}
				}
			}
		}
		
		
		// add support predicates of the form "morder(m,k1,k2) => not morder(m,k2,k1)"
		
		for ( pred <- allPredicates )
		{
			val predFuncList = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (funcExpr <- predFuncList)
			{
				if (funcExpr.name == BasicContainerModel.FUNC_MAP_ORDER)
				{
					var mapObj = funcExpr.args(1)
					var keyExpr1 = funcExpr.args(2)
					var keyExpr2 = funcExpr.args(3)
					
					if ( (keyExpr1 != BasicContainerModel.BOTTOM_EXPR) && (keyExpr2 != BasicContainerModel.BOTTOM_EXPR) )
					{
						// k1,k2
						val newClause1 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr1, keyExpr2))
						val newClause2 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr2, keyExpr1))
						val newPred1 = new Implication(newClause1, new Negation(newClause2))
						val newPred2 = new Implication(newClause2, new Negation(newClause1))						
						addSupportFormula(newPred1)
						addSupportFormula(newPred2)
						
						// k1,flk
						val newClause3 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr1, BasicContainerModel.LOGIC_KEY_EXPR))
						val newClause4 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.LOGIC_KEY_EXPR, keyExpr1))
						val newPred3 = new Implication(newClause3, new Negation(newClause4))
						val newPred4 = new Implication(newClause4, new Negation(newClause3))						
						addSupportFormula(newPred3)
						addSupportFormula(newPred4)
						
						// k2,flk
						val newClause5 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, keyExpr2, BasicContainerModel.LOGIC_KEY_EXPR))
						val newClause6 = new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, BasicContainerModel.LOGIC_KEY_EXPR, keyExpr2))
						val newPred5 = new Implication(newClause5, new Negation(newClause6))
						val newPred6 = new Implication(newClause6, new Negation(newClause5))
						addSupportFormula(newPred5)
						addSupportFormula(newPred6)
					}
				}
			}
		}
		
		
		// value of any program variable must differ from "bot"		
		for (pred <- allPredicates)
		{	
			val predVarNames = FormulaUtils.extractVariableNames(pred)
			
			for (vname <- predVarNames)
			{
				if ( ! Configuration.predicatesMngr.isReservedName(vname) )
				{
					addSupportFormula( new BinaryPredicate("!=", new Expression(vname), BasicContainerModel.BOTTOM_EXPR) )
				}
			}
		}


		// value of the symbol "bot" must differ from any integer constant used in the predicates
		for (pred <- allPredicates)
		{
			val predVarExprs = FormulaUtils.extractVariableExpressions(pred)

			for (vexpr <- predVarExprs)
			{
				if (ExpressionUtils.isConstantExpression(vexpr))
				{
					addSupportFormula( new BinaryPredicate("!=", vexpr, BasicContainerModel.BOTTOM_EXPR) )
				}
			}
		}


		// flk != bot
		addSupportFormula( new BinaryPredicate("!=", BasicContainerModel.LOGIC_KEY_EXPR, BasicContainerModel.BOTTOM_EXPR) )
		
		// fla != bot
		addSupportFormula( new BinaryPredicate("!=", BasicContainerModel.LOGIC_ANY_EXPR, BasicContainerModel.BOTTOM_EXPR) )
	}
	
	
	override def getTemporarySupportingFormulasResultSet(updatedPred : AtomicPredicate, weakPrecond : LogicFormula, resdetPreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Set[LogicFormula] =
	{
		var tempSupForms = Set[LogicFormula]()
		
		val wpAtomicPreds = FormulaUtils.extractAtomicPredicates(weakPrecond)
	

		// add support formulas that express difference of "bot" and constants used in the weakest precondition
		for (wpPred <- wpAtomicPreds)
		{
			val predVarExprs = FormulaUtils.extractVariableExpressions(wpPred)

			for (vexpr <- predVarExprs)
			{
				if (ExpressionUtils.isConstantExpression(vexpr))
				{
					val newForm = new BinaryPredicate("!=", vexpr, BasicContainerModel.BOTTOM_EXPR)

					tempSupForms = tempSupForms + newForm
				}
			}
		}

		
		// add temporary support formulas with the structure "mget(m,k) = bot" if there is no mget expression for given key 
		// we do not add such temporary support formulas if the key is logic variable or if the first argument (internal function) is "update"

		for (wpPred <- wpAtomicPreds)
		{
			val predFuncs = FormulaUtils.extractFunctionExpressionsRecursively(wpPred)
			
			for (funcExpr <- predFuncs)
			{
				if ( (funcExpr.name == BasicContainerModel.FUNC_MAP_GET) && ( ! ExpressionUtils.isLogicVariable(funcExpr.args(1)) ) && ( ! ExpressionUtils.isLogicVariable(funcExpr.args(2)) ) )
				{
					val tgtMapObj = funcExpr.args(1)
					val keyExpr = funcExpr.args(2)

					var containsUpdate = false
					
					if (ExpressionUtils.isUpdateExpr(funcExpr.args(0))) containsUpdate = true
					
					if ( ! containsUpdate )
					{
						// get all mget expressions that are possibly equivalent to the given one (funcExpr)
						
						var equivMapExprs = Set[Expression]()
						
						equivMapExprs = equivMapExprs + ContainerExpressionUtils.createMapGetExpr(tgtMapObj, keyExpr)
						
						for ( resPred <- resdetPreds )
						{
							if (resPred.isInstanceOf[BinaryPredicate])
							{
								val binResPred = resPred.asInstanceOf[BinaryPredicate]				
								
								if (binResPred.containsOperand(keyExpr))
								{
									val equivExpr = FormulaUtils.extractOtherOperandFromBinPred(binResPred, keyExpr)
									
									equivMapExprs = equivMapExprs + ContainerExpressionUtils.createMapGetExpr(tgtMapObj, equivExpr)
								}
							}								
						}
						
						// check for presence of equivalent mget expressions
						
						var mgetExists = false
						
						for ( resPred <- resdetPreds )
						{
							for ( operand <- FormulaUtils.extractOperands(resPred) )
							{
								if (equivMapExprs.contains(operand)) mgetExists = true
							}
						}
						
						if ( ! mgetExists )
						{
							val newForm = new BinaryPredicate("=", funcExpr, BasicContainerModel.BOTTOM_EXPR)
							
							tempSupForms = tempSupForms + newForm
						}
					}
				}
			}
		}
		
		
		return tempSupForms
	}
	
	
	override def getTemporarySupportingFormulasResultCube(updatedPred : AtomicPredicate, weakPrecond : LogicFormula, resdetPreds : Set[AtomicPredicate], resdetCube : Set[LogicFormula], varnames2matchexprs : Map[Expression, Set[Expression]], curClassOrigName : String, curMethodName : String) : (Set[LogicFormula], Map[Expression,Set[Expression]]) =
	{
		var tempSupForms = Set[LogicFormula]()

		var logicvar2matchexprs : Map[Expression, Set[Expression]] = new HashMap
		

		val wpAtomicPreds = FormulaUtils.extractAtomicPredicates(weakPrecond)
	
		
		// add temporary support predicates of the form "flk = 2", "flm = m1", and "fla = <something>"

		for ( (varNameExpr, matchingExprSet) <- varnames2matchexprs )
		{
			if (ExpressionUtils.isLogicVariable(varNameExpr))
			{
				var expr2disjunct : Map[Expression,LogicFormula] = new HashMap

				// true -> the cube contains only negated predicates for all matching expressions
				var allWithNegatedPreds = true

				// matching expressions for which the cube contains some positive predicates (that are not negated)
				var exprsWithPosPreds = Set[Expression]()
		
				for (matchExpr <- matchingExprSet)
				{
					// we ignore the matching expression (i.e., do not generate any clause for it) if all predicates from the result-determining set that match the expression with the logic variable are negated in the cube
					var onlyNegatedResPreds = true

					var possibleClauses = List[LogicFormula]()
					
					possibleClauses = possibleClauses :+ new BinaryPredicate("=", varNameExpr, matchExpr)
					
					// select elements of the result determining cube that have 'matchExpr' at the position of 'varNameExpr' in some atomic predicate from the weakest precondition
					for (resCubeElem <- resdetCube)
					{
						val resPred = FormulaUtils.extractAtomicPredicateFromLiteral(resCubeElem)
						
						var containsMatchExpr = false
						
						for (wpPred <- wpAtomicPreds)
						{
							val samePosExprs = FormulaUtils.findExprsWithMatchingPosition(wpPred, varNameExpr, resPred)

							// the result determining predicate has 'matchExpr' at the position of 'varNameExpr' in the atomic predicate from WP							
							if (samePosExprs.contains(matchExpr)) containsMatchExpr = true
						}
						
						if (containsMatchExpr) 
						{
							if (resCubeElem.isInstanceOf[AtomicPredicate] && resdetPreds.contains(resCubeElem.asInstanceOf[AtomicPredicate])) onlyNegatedResPreds = false

							possibleClauses = possibleClauses :+ resCubeElem
						}
					}
					
					if ( ! onlyNegatedResPreds )
					{
						allWithNegatedPreds = false
						exprsWithPosPreds = exprsWithPosPreds + matchExpr
					}

					if (possibleClauses.size > 1) expr2disjunct.put(matchExpr, new Conjunction(possibleClauses))
					else if (possibleClauses.size == 1) expr2disjunct.put(matchExpr, possibleClauses.head)
				}

				// if there are some matching expressions with only negated predicates in the cube and some expressions with some positive predicates, then we do not generate any clauses for those with only negated predicates
				// if there are only negated predicates in the cube for all matching expressions, then we keep them all (generate clauses for all of them)
				
				var possibleDisjuncts = List[LogicFormula]()

				var usedMatchingExprs = Set[Expression]()

				if (allWithNegatedPreds)
				{
					for ( (mexpr, disjunctForm) <- expr2disjunct )
					{
						possibleDisjuncts = possibleDisjuncts :+ disjunctForm

						usedMatchingExprs = usedMatchingExprs + mexpr
					}
				}
				else
				{
					for (mexpr <- exprsWithPosPreds)
					{
						possibleDisjuncts = possibleDisjuncts :+ expr2disjunct.get(mexpr).get

						usedMatchingExprs = usedMatchingExprs + mexpr
					}
				}
									
				if (possibleDisjuncts.size > 1) tempSupForms = tempSupForms + new Disjunction(possibleDisjuncts)
				else if (possibleDisjuncts.size == 1) tempSupForms = tempSupForms + possibleDisjuncts.head

				logicvar2matchexprs.put(varNameExpr, usedMatchingExprs)
			}
		}
	
		
		return (tempSupForms, logicvar2matchexprs)
	}
	

	override def getConflictingLiterals(headPred : AtomicPredicate, resdetPredSet : Set[AtomicPredicate], weakPrecond : LogicFormula, curClassOrigName : String, curMethodName : String) : Set[Set[LogicFormula]] =	
	{
		var conflictingTuples = super.getConflictingLiterals(headPred, resdetPredSet, weakPrecond, curClassOrigName, curMethodName)
	
		
		if (ContainerPredicateUtils.isMapOrderPredicate(headPred))
		{
			val morderFunc1 = ContainerPredicateUtils.extractMapOrderFuncExpr(headPred)	
			
			val mapObj1 = morderFunc1.args(1)
			val posExpr11 = morderFunc1.args(2)
			val posExpr12 = morderFunc1.args(3)
		
			for (resPred <- resdetPredSet if (resPred != headPred))		
			{
				// tells whether the pair (headPred, resPred) is conflicting
				var isConflictPred = false
				
				if (ContainerPredicateUtils.isMapOrderPredicate(resPred))
				{		
					val morderFunc2 = ContainerPredicateUtils.extractMapOrderFuncExpr(resPred)
								
					val mapObj2 = morderFunc2.args(1)
					val posExpr21 = morderFunc2.args(2)
					val posExpr22 = morderFunc2.args(3)
					
					if (mapObj1 == mapObj2) 
					{
						// morder(mit,m,c1,bot), morder(mit,m,c2,bot) for c1 != c2
						if ((posExpr12 == BasicContainerModel.BOTTOM_EXPR) && (posExpr22 == BasicContainerModel.BOTTOM_EXPR) && ExpressionUtils.isConstantValue(posExpr11) && ExpressionUtils.isConstantValue(posExpr21) && (posExpr11 != posExpr21)) isConflictPred = true
						
						// morder(mit,m,bot,c1), morder(mit,m,bot,c2) for c1 != c2						
						if ((posExpr11 == BasicContainerModel.BOTTOM_EXPR) && (posExpr21 == BasicContainerModel.BOTTOM_EXPR) && ExpressionUtils.isConstantValue(posExpr12) && ExpressionUtils.isConstantValue(posExpr22) && (posExpr12 != posExpr22)) isConflictPred = true
						
						// morder(mit,m,e1,e2), morder(mit,m,e2,e1) for e1 != bot and e2 != bot
						if ((posExpr11 == posExpr22) && (posExpr21 == posExpr12) && (posExpr11 != BasicContainerModel.BOTTOM_EXPR) && (posExpr12 != BasicContainerModel.BOTTOM_EXPR) && (posExpr21 != BasicContainerModel.BOTTOM_EXPR) && (posExpr22 != BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
							
						if ((posExpr11 == BasicContainerModel.BOTTOM_EXPR) && (posExpr12 == BasicContainerModel.BOTTOM_EXPR))
						{
							// morder(mit,m,bot,bot), morder(mit,m,d,bot)
							// morder(mit,m,bot,bot), morder(mit,m,bot,d)
							// morder(mit,m,bot,bot), morder(mit,m,d1,d2)
							if ((posExpr21 != BasicContainerModel.BOTTOM_EXPR) || (posExpr22 != BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
						}
						
						if ((posExpr21 == BasicContainerModel.BOTTOM_EXPR) && (posExpr22 == BasicContainerModel.BOTTOM_EXPR))
						{
							// morder(mit,m,d,bot), morder(mit,m,bot,bot)
							// morder(mit,m,bot,d), morder(mit,m,bot,bot)
							// morder(mit,m,d1,d2), morder(mit,m,bot,bot)
							if ((posExpr11 != BasicContainerModel.BOTTOM_EXPR) || (posExpr12 != BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
						}
						
						if (posExpr11 == posExpr21)
						{
							// morder(mit,m,e,v), morder(mit,m,e,bot)
							if ((posExpr12 != BasicContainerModel.BOTTOM_EXPR) && ( ! ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, posExpr12) ) && (posExpr22 == BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
							
							// morder(mit,m,e,bot), morder(mit,m,e,v)
							if ((posExpr12 == BasicContainerModel.BOTTOM_EXPR) && (posExpr22 != BasicContainerModel.BOTTOM_EXPR) && ( ! ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, posExpr22) )) isConflictPred = true
							
							// morder(mit,m,e,c1), morder(mit,m,e,c2) for c1 != c2
							if (ExpressionUtils.isConstantValue(posExpr12) && ExpressionUtils.isConstantValue(posExpr22) && (posExpr12 != posExpr22)) isConflictPred = true
						}
						
						if (posExpr12 == posExpr22)
						{
							// morder(mit,m,v,e), morder(mit,m,bot,e)
							if ((posExpr11 != BasicContainerModel.BOTTOM_EXPR) && ( ! ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, posExpr11) ) && (posExpr21 == BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
							
							// morder(mit,m,bot,e), morder(mit,m,v,e)
							if ((posExpr11 == BasicContainerModel.BOTTOM_EXPR) && (posExpr21 != BasicContainerModel.BOTTOM_EXPR) && ( ! ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, posExpr21) )) isConflictPred = true
							
							// morder(mit,m,c1,e), morder(mit,m,c2,e) for c1 != c2
							if (ExpressionUtils.isConstantValue(posExpr11) && ExpressionUtils.isConstantValue(posExpr21) && (posExpr11 != posExpr21)) isConflictPred = true
						}						
					}
				}
								
				if (resPred.containsFunction(BasicContainerModel.FUNC_MAP_GET))
				{
					val mgetOperand = FormulaUtils.extractOperandWithFunc(resPred, BasicContainerModel.FUNC_MAP_GET)
					val valueOperand = FormulaUtils.extractOtherOperandFromBinPred(resPred.asInstanceOf[BinaryPredicate], mgetOperand)
					
					if ( ! valueOperand.containsFunction(BasicContainerModel.FUNC_MAP_GET) )
					{
						val mapObj2 = mgetOperand.asInstanceOf[FunctionExpression].args(1)
						val keyExpr = mgetOperand.asInstanceOf[FunctionExpression].args(2)
						
						if ((mapObj1 == mapObj2) && (valueOperand == BasicContainerModel.BOTTOM_EXPR) && (resPred.getOperator() == "="))
						{
							// morder(mit,m,k1,k2), mget(map,m,k1) = bot
							// morder(mit,m,k1,k2), mget(map,m,k2) = bot
							if ((posExpr11 == keyExpr) || (posExpr12 == keyExpr)) isConflictPred = true
						}
						
						if ((mapObj1 == mapObj2) && (valueOperand != BasicContainerModel.BOTTOM_EXPR) && (resPred.getOperator() == "="))
						{
							// morder(mit,m,bot,bot), mget(map,m,k) != bot							
							if ((posExpr11 == BasicContainerModel.BOTTOM_EXPR) && (posExpr12 == BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true
						}
					}
				}
				
				if (resPred.containsFunction(BasicContainerModel.FUNC_MAP_SIZE))
				{
					val msizeOperand = FormulaUtils.extractOperandWithFunc(resPred, BasicContainerModel.FUNC_MAP_SIZE)
					val mapObj2 = msizeOperand.asInstanceOf[FunctionExpression].args(1)
					val szExpr = FormulaUtils.extractOtherOperandFromBinPred(resPred.asInstanceOf[BinaryPredicate], msizeOperand)
					
					// morder(mit,m,bot,bot), msize(msz,m) != 0
					if ((mapObj1 == mapObj2) && (resPred.getOperator() == "=") && (szExpr != Constants.ZERO_EXPR) && (posExpr11 == BasicContainerModel.BOTTOM_EXPR) && (posExpr12 == BasicContainerModel.BOTTOM_EXPR)) isConflictPred = true			
				}
				
				if (isConflictPred)
				{
					val newCfTuple = Set[LogicFormula](resPred)
					conflictingTuples = conflictingTuples + newCfTuple
				}
			}
		}
		
		if (FormulaUtils.containsOperandWithTopLevelFunction(headPred, BasicContainerModel.FUNC_MAP_GET))
		{
			var isHeadPredMgetAlias = false			
			if (headPred.asInstanceOf[BinaryPredicate].left.containsFunction(BasicContainerModel.FUNC_MAP_GET) && headPred.asInstanceOf[BinaryPredicate].right.containsFunction(BasicContainerModel.FUNC_MAP_GET)) isHeadPredMgetAlias = true
			
			for (resPred <- resdetPredSet if (resPred != headPred))
			{
				// tells whether the pair (headPred, resPred) is conflicting
				var isConflictResPred = false
			
				if (FormulaUtils.containsOperandWithTopLevelFunction(resPred, BasicContainerModel.FUNC_MAP_GET))
				{
					var isResPredMgetAlias = false
					if (resPred.asInstanceOf[BinaryPredicate].left.containsFunction(BasicContainerModel.FUNC_MAP_GET) && resPred.asInstanceOf[BinaryPredicate].right.containsFunction(BasicContainerModel.FUNC_MAP_GET)) isResPredMgetAlias = true
					
					if (( ! isHeadPredMgetAlias ) && ( ! isResPredMgetAlias ))
					{
						// the case of no predicate being an alias between two "mget" expressions

						val mgetOperand1 = FormulaUtils.extractOperandWithFunc(headPred, BasicContainerModel.FUNC_MAP_GET)
						val mgetOperand2 = FormulaUtils.extractOperandWithFunc(resPred, BasicContainerModel.FUNC_MAP_GET)
						
						val mapObj1 = mgetOperand1.asInstanceOf[FunctionExpression].args(1)
						val mapObj2 = mgetOperand2.asInstanceOf[FunctionExpression].args(1)
					
						val valueOperand1 = FormulaUtils.extractOtherOperandFromBinPred(headPred.asInstanceOf[BinaryPredicate], mgetOperand1)
						val valueOperand2 = FormulaUtils.extractOtherOperandFromBinPred(resPred.asInstanceOf[BinaryPredicate], mgetOperand2)
							
						if ((headPred.getOperator() == "=") && (resPred.getOperator() == "="))
						{
							if (mgetOperand1 == mgetOperand2)
							{
								// mget expressions over same maps and keys
								
								// mget(map,m,k) = c1, mget(map,m,k) = c2 for c1 != c2
								if (ExpressionUtils.isConstantValue(valueOperand1) && ExpressionUtils.isConstantValue(valueOperand2) && (valueOperand1 != valueOperand2)) isConflictResPred = true
								
								// mget(map,m,k) = bot, mget(map,m,k) != bot
								if ((valueOperand1 == BasicContainerModel.BOTTOM_EXPR) && (valueOperand2 != BasicContainerModel.BOTTOM_EXPR)) isConflictResPred = true
								
								// mget(map,m,k) != bot, mget(map,m,k) = bot
								if ((valueOperand2 == BasicContainerModel.BOTTOM_EXPR) && (valueOperand1 != BasicContainerModel.BOTTOM_EXPR)) isConflictResPred = true
								
								// mget(map,m,k) = v1, mget(map,m,k) = v2, v1 != v2
								if (ExpressionUtils.isVariableName(valueOperand1) && ExpressionUtils.isVariableName(valueOperand2))
								{		
									val cfAtomPred = new BinaryPredicate("=", valueOperand1, valueOperand2)
									
									if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
									{
										val newCfTuple = Set[LogicFormula](resPred, new Negation(cfAtomPred))						
										conflictingTuples = conflictingTuples + newCfTuple
									}
								}
							}
							else if (mapObj1 == mapObj2)
							{
								// mget expressions over same maps
								
								val keyExpr1 = mgetOperand1.asInstanceOf[FunctionExpression].args(2)
								val keyExpr2 = mgetOperand2.asInstanceOf[FunctionExpression].args(2)
								
								// mget(map,m,k1) = bot, mget(map,m,k2) != bot, mget(map,m,k1) = mget(map,m,k2)
								// mget(map,m,k1) != bot, mget(map,m,k2) = bot, mget(map,m,k1) = mget(map,m,k2)
								if ( ((valueOperand1 == BasicContainerModel.BOTTOM_EXPR) && (valueOperand2 != BasicContainerModel.BOTTOM_EXPR)) || ((valueOperand1 != BasicContainerModel.BOTTOM_EXPR) && (valueOperand2 == BasicContainerModel.BOTTOM_EXPR)) )
								{
									val cfAtomPred = new BinaryPredicate("=", mgetOperand1, mgetOperand2)
									
									if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
									{
										val newCfTuple = Set[LogicFormula](resPred, cfAtomPred)						
										conflictingTuples = conflictingTuples + newCfTuple
									}
								}
								
								// mget(map,m,c) = bot, mget(map,m,k) != bot, k = c
								// mget(map,m,k) = bot, mget(map,m,c) != bot, k = c
								if ( (ExpressionUtils.isConstantValue(keyExpr1) && (valueOperand1 == BasicContainerModel.BOTTOM_EXPR) && (valueOperand2 != BasicContainerModel.BOTTOM_EXPR)) || (ExpressionUtils.isConstantValue(keyExpr2) && (valueOperand1 == BasicContainerModel.BOTTOM_EXPR) && (valueOperand2 != BasicContainerModel.BOTTOM_EXPR)) )
								{
									val cfAtomPred = new BinaryPredicate("=", keyExpr1, keyExpr2)
									
									if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
									{
										val newCfTuple = Set[LogicFormula](resPred, cfAtomPred)						
										conflictingTuples = conflictingTuples + newCfTuple
									}
								}
								
								// mget(map,m,k1) = v1, mget(map,m,k2) = v2, mget(map,m,k1) = mget(map,m,k2), v1 != v2
								// mget(map,m,k1) = v1, mget(map,m,k2) = v2, mget(map,m,k1) != mget(map,m,k2), v1 = v2
								if ((valueOperand1 != BasicContainerModel.BOTTOM_EXPR) && ( ! ExpressionUtils.isConstantExpression(valueOperand1) ) && (valueOperand2 != BasicContainerModel.BOTTOM_EXPR) && ( ! ExpressionUtils.isConstantExpression(valueOperand2) ))
								{
									val cfAtomPred1 = new BinaryPredicate("=", mgetOperand1, mgetOperand2)
									val cfAtomPred2 = new BinaryPredicate("=", valueOperand1, valueOperand2)
									
									if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred1, -1) && Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred2, -1))
									{
										val newCfTuple1 = Set[LogicFormula](resPred, cfAtomPred1, new Negation(cfAtomPred2))
										conflictingTuples = conflictingTuples + newCfTuple1
																		
										val newCfTuple2 = Set[LogicFormula](resPred, new Negation(cfAtomPred1), cfAtomPred2)
										conflictingTuples = conflictingTuples + newCfTuple2
									}
								}
							}
							else
							{
								// possibly different maps
								
								val keyExpr1 = mgetOperand1.asInstanceOf[FunctionExpression].args(2)
								val keyExpr2 = mgetOperand2.asInstanceOf[FunctionExpression].args(2)
								
								// tuple { mget(map,m1,k1) = v, v = c1, mget(map,m2,k2) = c2 } is conflicting if (1) c1 != c2, (2) m1 = m2 (cube contains literal m1 = m2 or the expressions m1 and m2 are identical), and (3) k1 = k2 (cube contains literal k1 = k2 or the expressions k1 and k2 are identical)								
								if ((valueOperand1 != BasicContainerModel.BOTTOM_EXPR) && ( ! ExpressionUtils.isConstantExpression(valueOperand1) ) && ExpressionUtils.isConstantExpression(valueOperand2))
								{
									val cfAtomPred1 = new BinaryPredicate("=", valueOperand1, valueOperand2)
									val cfAtomPred2 = new BinaryPredicate("=", mapObj1, mapObj2)
									val cfAtomPred3 = new BinaryPredicate("=", keyExpr1, keyExpr2)
									
									if ( Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred1, -1) && ( ((mapObj1 != mapObj2) && Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred2, -1)) || (mapObj1 == mapObj2) || ((keyExpr1 != keyExpr2) && Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred3, -1)) || (keyExpr1 == keyExpr2) ) )
									{
										var newCfTuple = Set[LogicFormula](new Negation(cfAtomPred1))
									
										if (mapObj1 != mapObj2) newCfTuple = newCfTuple + cfAtomPred2
										if (keyExpr1 != keyExpr2) newCfTuple = newCfTuple + cfAtomPred3
									
										conflictingTuples = conflictingTuples + newCfTuple
									}
								}
							}
						}
					}
				}
								
				if (isConflictResPred)
				{
					val newCfTuple = Set[LogicFormula](resPred)
					conflictingTuples = conflictingTuples + newCfTuple
				}
			}			
		}
		
		
		if (headPred.containsFunction(BasicContainerModel.FUNC_MAP_SIZE))
		{
			val msizeOperand1 = FormulaUtils.extractOperandWithFunc(headPred, BasicContainerModel.FUNC_MAP_SIZE)
			val mapObj1 = msizeOperand1.asInstanceOf[FunctionExpression].args(1)
			val szExpr1 = FormulaUtils.extractOtherOperandFromBinPred(headPred.asInstanceOf[BinaryPredicate], msizeOperand1)
			
			for (resPred <- resdetPredSet if (resPred != headPred))
			{
				// tells whether the pair (headPred, resPred) is conflicting
				var isConflictPred = false
		
				if (resPred.containsFunction(BasicContainerModel.FUNC_MAP_SIZE))
				{				
					val msizeOperand2 = FormulaUtils.extractOperandWithFunc(resPred, BasicContainerModel.FUNC_MAP_SIZE)	
					val mapObj2 = msizeOperand2.asInstanceOf[FunctionExpression].args(1)
					val szExpr2 = FormulaUtils.extractOtherOperandFromBinPred(resPred.asInstanceOf[BinaryPredicate], msizeOperand2)
				
					// msize(msz,m) = c1, msize(msz,m) = c2 for c1 != c2
					if ((msizeOperand1 == msizeOperand2) && (headPred.getOperator() == "=") && (resPred.getOperator() == "="))
					{
						if (ExpressionUtils.isConstantValue(szExpr1) && ExpressionUtils.isConstantValue(szExpr2) && (szExpr1 != szExpr2)) isConflictPred = true
												
						// tuple { msize(msz,m1) = v, v = c1, msize(msz,m2) = c2 } is conflicting if (1) c1 != c2 and (2) m1 = m2 (the cube contains literal m1 = m2 or the expressions m1 and m2 are identical)								
						if (( ! ExpressionUtils.isConstantExpression(szExpr1) ) && ExpressionUtils.isConstantExpression(szExpr2))
						{
							val cfAtomPred1 = new BinaryPredicate("=", szExpr1, szExpr2)
							val cfAtomPred2 = new BinaryPredicate("=", mapObj1, mapObj2)
							
							if ( Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred1, -1) && ( ((mapObj1 != mapObj2) && Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred2, -1)) || (mapObj1 == mapObj2) ) )
							{							
								var newCfTuple = Set[LogicFormula](new Negation(cfAtomPred1))
							
								if (mapObj1 != mapObj2) newCfTuple = newCfTuple + cfAtomPred2
														
								conflictingTuples = conflictingTuples + newCfTuple
							}
						}
					}
					
					if ((msizeOperand1 == msizeOperand2) && (headPred.getOperator() == "=") && (resPred.getOperator() == "<"))
					{							
						if (ExpressionUtils.isConstantValue(szExpr1) && ( ! ExpressionUtils.isConstantValue(szExpr2) ))
						{
							val binResPred = resPred.asInstanceOf[BinaryPredicate]
						
							// msize(msz,m) = c1, v >= c1, v < msize(msz,m)
							if (binResPred.left == szExpr2)
							{
								val cfAtomPred = new BinaryPredicate(">=", szExpr2, szExpr1)
								
								if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
								{
									val newCfTuple = Set[LogicFormula](resPred, cfAtomPred)
									conflictingTuples = conflictingTuples + newCfTuple
								}
							}
							
							// msize(msz,m) = c1, v <= c1, msize(msz,m) < v
							if (binResPred.right == szExpr2)
							{
								val cfAtomPred = new BinaryPredicate("<=", szExpr2, szExpr1)
								
								if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
								{
									val newCfTuple = Set[LogicFormula](resPred, cfAtomPred)
									conflictingTuples = conflictingTuples + newCfTuple
								}
							}
						}
					}
				}
				
				if (resPred.containsFunction(BasicContainerModel.FUNC_MAP_GET))
				{
					val mgetOperand = FormulaUtils.extractOperandWithFunc(resPred, BasicContainerModel.FUNC_MAP_GET)
					val valueOperand = FormulaUtils.extractOtherOperandFromBinPred(resPred.asInstanceOf[BinaryPredicate], mgetOperand)
				
					// ignore aliasing between two "mget" expressions
					if ( ! valueOperand.containsFunction(BasicContainerModel.FUNC_MAP_GET) )
					{
						val mapObj2 = mgetOperand.asInstanceOf[FunctionExpression].args(1)
						
						// msize(msz,m) = 0, mget(map,m,k) != bot
						if ((mapObj1 == mapObj2) && (resPred.getOperator() == "=") && (valueOperand != BasicContainerModel.BOTTOM_EXPR) && (szExpr1 == Constants.ZERO_EXPR)) isConflictPred = true
					}
				}
								
				if (isConflictPred)
				{
					val newCfTuple = Set[LogicFormula](resPred)
					conflictingTuples = conflictingTuples + newCfTuple
				}
			}
		}
			
		return conflictingTuples
	}
	
	
	override def isAmbiguousCube(resdetCube : Set[LogicFormula], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : Boolean =
	{
		// if the cube contains literal "not (= tv c)" for every equality predicate over "tv", then it is ambiguous
		// we test all possible temporary variables (tmpk, tmpl, tmpi, tmpb)
		
		for ( (tempVarName, tempVarPreds) <- tempvar2eqlpreds )
		{
			var onlyNegations = true
			
			for (tvPred <- tempVarPreds)
			{
				// some predicate over the temporary variable that is not negated
				if (resdetCube.contains(tvPred)) onlyNegations = false
			}
			
			if (onlyNegations) return true
		}		
				
		return false	
	}
	
	
	override def getIndexesForUnambiguousPredicates(resdetPredList : List[AtomicPredicate], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : List[(String,List[Int])] =
	{
		// a given predicate is not ambiguous if it specifies a particular value for some temporary variable (e.g., it has the form "= tv c")
		// we consider all possible temporary variables (tmpk, tmpl, tmpi, tmpb)
		
		var tmpvar2indexes = List[(String,List[Int])]()
		
		for ( (tempVarName, tempVarPreds) <- tempvar2eqlpreds )
		{
			var unambgIndexes = List[Int]()
			
			var curIndex = 0
			
			for (resdetPred <- resdetPredList)
			{
				if (tempVarPreds.contains(resdetPred)) unambgIndexes = unambgIndexes :+ curIndex
				
				curIndex += 1	
			}
			
			tmpvar2indexes = tmpvar2indexes :+ ( (tempVarName, unambgIndexes) )
		}		
				
		return tmpvar2indexes	
	}
	
	
	override def isIrrelevantCubeForAliasing(resdetCube : Set[LogicFormula], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : Boolean =
	{
		for ( (aliasingPred, relevantPreds) <- alias2relevantpreds )
		{
			// irrelevant cube: the aliasing predicate is negated in the cube and there exists some relevant predicate that is not negated
			
			val negatedAliasPred = new Negation(aliasingPred)
			
			if (resdetCube.contains(negatedAliasPred))
			{
				for (relevPred <- relevantPreds)
				{
					if (resdetCube.contains(relevPred)) return true					
				}
			}
		}
	
		return false	
	}
	
	
	override def getIndexesForAliasingRelevantPredicates(resdetPredList : List[AtomicPredicate], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : List[(Int,List[Int])] =
	{
		var indexesAliasRelevPreds = List[(Int,List[Int])]()
		
		for ( (aliasingPred, relevantPreds) <- alias2relevantpreds )
		{
			var aliasingPredIdx = -1
			var relevantPredIndexes = List[Int]()
			
			var curIndex = 0
			
			for (resdetPred <- resdetPredList)
			{
				if (resdetPred == aliasingPred) aliasingPredIdx = curIndex
				
				if (relevantPreds.contains(resdetPred)) relevantPredIndexes = relevantPredIndexes :+ curIndex
				
				curIndex += 1
			}
			
			indexesAliasRelevPreds = indexesAliasRelevPreds :+ ( (aliasingPredIdx, relevantPredIndexes) )
		}
		
		return indexesAliasRelevPreds
	}
	
	
	override def isRelevantPredicateOverAliasedExpr(pred : AtomicPredicate, targetVarExpr : Expression, aliasedExpr : Expression) : Boolean =
	{
		if (ContainerPredicateUtils.isMapOrderPredicate(pred))
		{
			val morderFunc = ContainerPredicateUtils.extractMapOrderFuncExpr(pred)
			
			if (morderFunc.args(1) == aliasedExpr) return true
		}
		
		if (pred.containsFunction(BasicContainerModel.FUNC_MAP_GET))
		{
			val binPred = pred.asInstanceOf[BinaryPredicate]
			
			var mgetFuncsList = FormulaUtils.extractFunctionExpressionsRecursively(binPred, BasicContainerModel.FUNC_MAP_GET)
				
			for (mgetFunc <- mgetFuncsList)
			{
				if (mgetFunc.args(1) == aliasedExpr) return true
			}
		}

		if (pred.containsFunction(BasicContainerModel.FUNC_MAP_SIZE))
		{
			val binPred = pred.asInstanceOf[BinaryPredicate]
			
			var msizeFuncsList = FormulaUtils.extractFunctionExpressionsRecursively(binPred, BasicContainerModel.FUNC_MAP_SIZE)
			
			for (msizeFunc <- msizeFuncsList)
			{
				if (msizeFunc.args(1) == aliasedExpr) return true
			}
		}
		
		return false
	}
	
	
	override def findMatchingExpressions(inputForm : LogicFormula, availablePreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Map[Expression, Set[Expression]] =
	{
		// we copy all the code from the methods in superclass because using inheritance would be quite difficult in this case and it would obfuscate the control flow (splitting the method, multiple calls of the superclass methods) 
		
		var matchingExprs : Map[Expression, Set[Expression]] = new HashMap

		val inputFormAtomPreds = FormulaUtils.extractAtomicPredicates(inputForm)
		
		for (atomPred <- inputFormAtomPreds)
		{
			val atomPredVarNames = FormulaUtils.extractVariableNames(atomPred)
			
			// try to find matches for each variable name
			for (vname <- atomPredVarNames)
			{
				if ( ( ! BasicContainerModel.isSpecialArrayName(vname) ) && (vname != BasicContainerModel.BOTTOM_STR) )
				{
					val varNameExpr = new Expression(vname)
					
					var possibleMatches = Set[Expression]()
					
					for (availPred <- availablePreds)
					{
						possibleMatches = possibleMatches ++ FormulaUtils.findExprsWithMatchingPosition(atomPred, varNameExpr, availPred)
					}

					// aliasing predicate defines a possible match for a logic variable
					if (ExpressionUtils.isLogicVariable(varNameExpr) && atomPred.isInstanceOf[BinaryPredicate])
					{
						val binAtomPred = atomPred.asInstanceOf[BinaryPredicate]

						if (FormulaUtils.isAliasingPredicate(binAtomPred, varNameExpr))
						{
							possibleMatches = possibleMatches + FormulaUtils.extractOtherOperandFromBinPred(binAtomPred, varNameExpr)
						}
					}
						
					if ( ! ExpressionUtils.isLogicVariable(varNameExpr) )
					{
						// program variable can match only constant expressions						
						possibleMatches = possibleMatches.filter(e => ExpressionUtils.isConstantExpression(e))
					}
					
					if (ExpressionUtils.isLogicVariable(varNameExpr))
					{
						// bot symbol cannot match a logic variable
						possibleMatches = possibleMatches - BasicContainerModel.BOTTOM_EXPR
					}

					if (varNameExpr == BasicContainerModel.LOGIC_MAP_EXPR)
					{
						// keep only map expressions		
						possibleMatches = possibleMatches.filter(e => ContainerAbstractionData.isMapExpression(curClassOrigName, curMethodName, e))
					}					
					
					if (possibleMatches.size() > 0)
					{
						matchingExprs.put(varNameExpr, matchingExprs.getOrElse(varNameExpr, Set[Expression]()) ++ possibleMatches)
					}
				}
			}
		}
		
		
		// for each variable keep only expressions that (i) match all occurrences of the given variable in the input formula and (ii) have the same type
				
		for (tgtVarNameExpr <- matchingExprs.keys)
		{
			val possibleMatches = matchingExprs.getOrElse(tgtVarNameExpr, Set[Expression]())
			
			if (Main.DEBUG) 
			{
				println("[DEBUG ContainerPredicateSemanticsModel.findMatchingExpressions] target variable = " + tgtVarNameExpr)
				ExpressionUtils.printSet(possibleMatches, "[DEBUG ContainerPredicateSemanticsModel.findMatchingExpressions] possible matches:")
			}
			
			var dropMatches = Set[Expression]()
				
			for (posMatchExpr <- possibleMatches)
			{
				for (inputPred <- inputFormAtomPreds)
				{
					var dropMatchPred = true
						
					var possibleMatchExprPreds = Set[AtomicPredicate]()
					
					possibleMatchExprPreds = possibleMatchExprPreds + FormulaUtils.copyWithReplace(inputPred, tgtVarNameExpr.toString(), posMatchExpr.toString())
					
					// try all combinations where other variables are replaced with their matches
					
					for (otherVarExpr <- matchingExprs.keys if (tgtVarNameExpr != otherVarExpr))
					{
						val otherVarMatches = matchingExprs.getOrElse(otherVarExpr, Set[Expression]())
						
						var newMatchExprPreds = Set[AtomicPredicate]()
						
						for (matchExprPred <- possibleMatchExprPreds)
						{
							newMatchExprPreds = newMatchExprPreds + matchExprPred
							
							for (otherMatchExpr <- otherVarMatches)
							{
								newMatchExprPreds = newMatchExprPreds + FormulaUtils.copyWithReplace(matchExprPred, otherVarExpr.toString(), otherMatchExpr.toString())
							}
						}
						
						possibleMatchExprPreds = newMatchExprPreds
					}
					
					// drop matching predicates that contain logic variables
					possibleMatchExprPreds = possibleMatchExprPreds.filterNot(e => FormulaUtils.containsLogicVariables(e))
					
					// equality predicate between a logic variables and the candidate matching expression
					if (inputPred.isInstanceOf[BinaryPredicate] && (inputPred.getOperator() == "="))
					{
						if (inputPred.containsOperand(posMatchExpr.toString()))
						{
							val otherVarExpr = FormulaUtils.extractOtherOperandFromBinPred(inputPred.asInstanceOf[BinaryPredicate], posMatchExpr)
						
							if (ExpressionUtils.isLogicVariable(otherVarExpr)) dropMatchPred = false
						}
					}
					
					for (matchExprPred <- possibleMatchExprPreds)
					{
						if (availablePreds.contains(matchExprPred) || matchExprPred.isAlwaysTrue()) dropMatchPred = false
						
						// binary predicate over two integer constants
						if (matchExprPred.isInstanceOf[BinaryPredicate])
						{
							val binMEPred = matchExprPred.asInstanceOf[BinaryPredicate]

							if (ExpressionUtils.isConstantValue(binMEPred.left) && ExpressionUtils.isConstantValue(binMEPred.right)) dropMatchPred = false
						}
						
						// predicates "v = bot" for 'v' being some of the matching expressions (such predicates appear inside negations)
						if (matchExprPred.isInstanceOf[BinaryPredicate])
						{
							if ((matchExprPred.getOperator() == "=") && matchExprPred.containsOperand(BasicContainerModel.BOTTOM_STR))
							{
								val otherVarExpr = FormulaUtils.extractOtherOperandFromBinPred(matchExprPred.asInstanceOf[BinaryPredicate], BasicContainerModel.BOTTOM_EXPR)
								
								if (possibleMatches.contains(otherVarExpr)) dropMatchPred = false
							}
						}
						
						// identity between two map variables
						if (matchExprPred.isInstanceOf[BinaryPredicate]	&& (matchExprPred.getOperator() == "="))
						{
							val binMatchExprPred = matchExprPred.asInstanceOf[BinaryPredicate]
							
							if (ContainerAbstractionData.isMapExpression(curClassOrigName, curMethodName, binMatchExprPred.left) && (binMatchExprPred.left == binMatchExprPred.right)) dropMatchPred = false
						}						
						
						// temporary variables "tmpk", "tmpl", and "tmpi" can match only integer constants or some other variables
						if ((tgtVarNameExpr == BasicContainerModel.TMP_KEY_EXPR) || (tgtVarNameExpr == BasicContainerModel.TMP_LOCAL_EXPR) || (tgtVarNameExpr == BasicContainerModel.TMP_INDEX_EXPR))
						{
							if (ExpressionUtils.isVariableName(posMatchExpr) || ExpressionUtils.isConstantValue(posMatchExpr)) dropMatchPred = false
							else dropMatchPred = true
						}
						
						// logic variable "fla" (used for map values) cannot match the symbol "bottom"
						if ((tgtVarNameExpr == BasicContainerModel.LOGIC_ANY_EXPR) && (posMatchExpr == BasicContainerModel.BOTTOM_EXPR)) dropMatchPred = true
						
						// logic variable "fla" (used for map values) cannot match the temporary variable "tmpk" (used for map keys)
						if ((tgtVarNameExpr == BasicContainerModel.LOGIC_ANY_EXPR) && (posMatchExpr == BasicContainerModel.TMP_KEY_EXPR)) dropMatchPred = true
						
						// logic variable representing a map cannot match integer constant or some program variable that does not have a map type
						if (tgtVarNameExpr == BasicContainerModel.LOGIC_MAP_EXPR)
						{
							if (ExpressionUtils.isConstantValue(posMatchExpr)) dropMatchPred = true
							
							if (ExpressionUtils.isVariableName(posMatchExpr))
							{
								if ( ! ContainerAbstractionData.isMapExpression(curClassOrigName, curMethodName, posMatchExpr) ) dropMatchPred = true							
							}
						}
						
						// check predicates with "morder" if they contain associated maps and iterator variables
						if (matchExprPred.isInstanceOf[UnaryPredicate])
						{
							val matchExprPredFuncs = FormulaUtils.extractFunctionOperands(matchExprPred)
							
							for (morderFunc <- matchExprPredFuncs if morderFunc.name == BasicContainerModel.FUNC_MAP_ORDER)
							{
								var existsIterKeyExpr = false
								var firstIterNotAssoc = false
								var secondIterNotAssoc = false
								
								if (ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, morderFunc.args(2)))
								{
									existsIterKeyExpr = true
									
									if ( ! ContainerAbstractionData.isIteratorWithMap(curClassOrigName, curMethodName, morderFunc.args(2), morderFunc.args(1)) ) firstIterNotAssoc = true									
								}

								if (ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, morderFunc.args(3)))								
								{
									existsIterKeyExpr = true
									
									if ( ! ContainerAbstractionData.isIteratorWithMap(curClassOrigName, curMethodName, morderFunc.args(3), morderFunc.args(1)) ) secondIterNotAssoc = true
								}
								
								if (existsIterKeyExpr)
								{
									if (firstIterNotAssoc && secondIterNotAssoc) dropMatchPred = true
								}
							}							
						}
					}
					
					// possible matches for an iterator variable cannot be an integer constant
					if (ContainerAbstractionData.isIteratorVariable(curClassOrigName, curMethodName, tgtVarNameExpr))
					{
						if (ExpressionUtils.isConstantValue(posMatchExpr)) dropMatchPred = true						
					}
					
					
					if (dropMatchPred)
					{
						dropMatches = dropMatches + posMatchExpr
					}
				}
			}

			// keep all matches defined by aliasing predicates in the weakest precondition
			for (inputPred <- inputFormAtomPreds)
			{
				if (inputPred.isInstanceOf[BinaryPredicate])
				{
					val binInputPred = inputPred.asInstanceOf[BinaryPredicate]
					
					dropMatches = dropMatches.filterNot(e => FormulaUtils.isAliasingPredicate(binInputPred, tgtVarNameExpr, e))
				}
			}

			val filteredPossibleMatches = possibleMatches.filterNot(e => dropMatches.contains(e))

			if (Main.DEBUG) 
			{
				ExpressionUtils.printSet(dropMatches, "[DEBUG ContainerPredicateSemanticsModel.findMatchingExpressions] dropped matches:")
				ExpressionUtils.printSet(filteredPossibleMatches, "[DEBUG ContainerPredicateSemanticsModel.findMatchingExpressions] filtered possible matches:")
			}
			
			matchingExprs.put(tgtVarNameExpr, filteredPossibleMatches)				
		}
		
		// drop empty sets of matching expressions
		matchingExprs = matchingExprs.filter( e => (e._2.size() > 0) )
		
		return matchingExprs
	}
	
}
