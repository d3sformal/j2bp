package j2bp

import java.io.Writer
import java.io.OutputStreamWriter

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.matching.Regex

import common._

import util.StringUtils


class YicesSMTProvider extends SMTProvider
{
	protected var cacheForm2SMT : Map[String, String] = new HashMap
	protected var cacheExpr2SMT : Map[String, String] = new HashMap
	
	protected var cacheValidityQuery2Result : Map[(Set[LogicFormula], LogicFormula), Boolean] = new HashMap
	
	protected var numSolverCalls : Long = 0
	
	
	/**
	 * This method checks validity of the formula "(resdetFormulas[1] and ... and resdetFormulas[N]) => weakPrecond".
	 */
	def checkValidity(ctx : AbstractionContext, weakPrecond : LogicFormula, resdetCube : Set[LogicFormula]) : Boolean =
	{
		var queryResultOpt = cacheValidityQuery2Result.get( (resdetCube, weakPrecond) )
		
		if (queryResultOpt != None) return queryResultOpt.get
		
		
		var smtInput : String = ""
		
		if (Main.DEBUG) println("[DEBUG YicesSMTProvider.checkValidity] weakest precondition: " + weakPrecond.toString())

		
		val reservedVarNames = Configuration.predicatesMngr.getReservedVariableNames()
		val reservedFunctions = Configuration.predicatesMngr.getReservedFunctionSignatures()
		val reservedConstants = Configuration.predicatesMngr.getReservedConstants()
		
		
		var formVarNames = Set[String]()
		var formFieldNames = Set[String]()
		
		
		// extract variable names and field names from the cube and weakest precondition
		
		for ( formElem <- (resdetCube + weakPrecond) )
		{
			for (vname <- FormulaUtils.extractVariableNames(formElem))
			{
				if ( ! Configuration.predicatesMngr.isReservedName(vname) ) 
				{
					formVarNames = formVarNames + vname
				}
			}
			
			formFieldNames = formFieldNames ++ FormulaUtils.extractFieldNames(formElem)
		}
			
		if (Main.DEBUG) StringUtils.printStringSet("[DEBUG YicesSMTProvider.checkValidity] variable names from the whole formula: ", formVarNames)
		
		if (Main.DEBUG) StringUtils.printStringSet("[DEBUG YicesSMTProvider.checkValidity] field names from the whole formula: ", formFieldNames)

		
		// write type declarations
	
		var declaredVars = List[String]()
		
		for (vname <- formVarNames)
		{
			if ( ! declaredVars.contains(vname) )
			{
				smtInput = smtInput + ("(define " + convertNameToSMT(vname) + "::int)\n")
				declaredVars = declaredVars :+ vname
			}
		}

		var declaredFunctions = List[String]()
		
		for (fieldName <- formFieldNames)
		{
			if ( ! declaredFunctions.contains(fieldName) )
			{
				smtInput = smtInput + ("(define " + convertNameToSMT(fieldName) + "::(-> int int))\n")
				declaredFunctions = declaredFunctions :+ fieldName
			}
		}

		
		// built-in variables and functions
				
		for (rvar <- reservedVarNames)
		{
			if ( ! declaredVars.contains(rvar) )
			{
				smtInput = smtInput + ("(define " + rvar + "::int)\n")
				declaredVars = declaredVars :+ rvar
			}
		}			
		
		for (rfunc <- reservedFunctions)
		{
			if ( ! declaredFunctions.contains(rfunc._1) )
			{
				smtInput = smtInput + ("(define " + rfunc._1 + "::(-> " + rfunc._2 + " " + rfunc._3 + "))\n")
				declaredFunctions = declaredFunctions :+ rfunc._1
			}
		}
		
		for (rconst <- reservedConstants)
		{
			smtInput = smtInput + ("(define " + rconst._1 + "::int (" + rconst._2 + "))\n")
		}
		
		
		// we check validity of the formula by checking satisfiability of its negation
		
		// generate formula from predicates

		smtInput = smtInput + "(assert (not (=> "
	
		if (resdetCube.size > 1)
		{
			smtInput = smtInput + "("
			
			smtInput = smtInput + "and"
			
			for (cubeElem <- resdetCube)  
			{
				smtInput = smtInput + (" " + convertFormulaToSMT(cubeElem))
			}

			smtInput = smtInput + ")"
		}
		else if (resdetCube.size == 1)
		{
			smtInput = smtInput + convertFormulaToSMT(resdetCube.head)
		}
		else
		{
			smtInput = smtInput + "true"
		}
		
		smtInput = smtInput + (" " + convertFormulaToSMT(weakPrecond))
		
		smtInput = smtInput + " ) ) )\n"
		
		// run the satisfiability check
		smtInput = smtInput + "(check)\n"
		
				
		// run the Yices smt solver to read from stdin given as normal string
		// yices reads from stdin and prints to stdout (and errors go to stderr)
		
		var smtCommand = "./tools/yices"
		if (Main.DEBUG) smtCommand = smtCommand + " -e"
		
		val smtResult : (Int, String, String) = util.Process.runCmdWithStrOutputs(smtCommand, util.Process.string2outstream(smtInput))
		
		numSolverCalls += 1
		
		if (Main.DEBUG) 
		{
			println("[DEBUG YicesSMTProvider.checkValidity] input for the SMT solver:")
			val smtInputLines : Array[String] = smtInput.split("\n")
			for (line <- smtInputLines) 
			{
				if ( ! line.startsWith("(define") ) println("\t" + line)
			}

			println("[DEBUG YicesSMTProvider.checkValidity] smt result: status = " + smtResult._1 + ", output = ")			
			val smtOutputLines : Array[String] = smtResult._2.split("\n")
			for (line <- smtOutputLines) println("\t" + line)
		}
			
		// some error occurred
		if (smtResult._1 != 0) 
		{
			println("[ERROR] solver failed, error message = " + smtResult._3)
			
			println("[ERROR] input for the SMT solver:")
			val smtInputLines : Array[String] = smtInput.split("\n")
			for (line <- smtInputLines) println("\t" + line)
			
			return false
		}
		
		val smtOut = smtResult._2

		
		var queryResult = true
		
		if (smtOut.startsWith("sat")) queryResult = false
		
		cacheValidityQuery2Result.put( (resdetCube, weakPrecond), queryResult )
		
		return queryResult
	}
	
	
	def resetCounters() =
	{
		numSolverCalls = 0	
	}
	
	def getCountersValue() : Long =
	{
		return numSolverCalls
	}
	
	
	private def convertFormulaToSMT(form : LogicFormula) : String =
	{
		var formSMTOpt = cacheForm2SMT.get(form.toString())
		
		if (formSMTOpt != None) return formSMTOpt.get
		
		
		var formSMT = ""
		
		if (form.isInstanceOf[BinaryPredicate])
		{
			val binPred = form.asInstanceOf[BinaryPredicate]
			
			if (binPred.op == "!=") formSMT = "(not (= " + convertExprToSMT(binPred.left) + " " + convertExprToSMT(binPred.right) + "))"
			else formSMT = "(" + binPred.op + " " + convertExprToSMT(binPred.left) + " " + convertExprToSMT(binPred.right) + ")"
		}
		else if (form.isInstanceOf[UnaryPredicate])
		{
			val unPred = form.asInstanceOf[UnaryPredicate]

			if (unPred.op == "") formSMT = convertExprToSMT(unPred.arg)
			else formSMT = "(" + unPred.op + " " + convertExprToSMT(unPred.arg) + ")"
		}
		else if (form.isInstanceOf[Negation])
		{
			val negForm = form.asInstanceOf[Negation]
			
			formSMT = "(not " + convertFormulaToSMT(negForm.clause) + ")"
		}
		else if (form.isInstanceOf[Conjunction])
		{
			val conjForm = form.asInstanceOf[Conjunction]

			var conjFormStr = "(and" 

			for (cl <- conjForm.clauses)
			{
				conjFormStr = conjFormStr + (" " + convertFormulaToSMT(cl))
			}

			conjFormStr = conjFormStr + ")"
		
			formSMT = conjFormStr
		}
		else if (form.isInstanceOf[Disjunction])
		{
			val disjForm = form.asInstanceOf[Disjunction]

			var disjFormStr = "(or" 

			for (cl <- disjForm.clauses)
			{
				disjFormStr = disjFormStr + (" " + convertFormulaToSMT(cl))
			}

			disjFormStr = disjFormStr + ")"
		
			formSMT = disjFormStr
		}
		else if (form.isInstanceOf[Implication])
		{
			val implyForm = form.asInstanceOf[Implication]
			
			formSMT = "(=> " + convertFormulaToSMT(implyForm.ante) + " " + convertFormulaToSMT(implyForm.cons) + ")"
		}
		else if (form.isInstanceOf[ExistentialQuantification])
		{
			val existForm = form.asInstanceOf[ExistentialQuantification]
			
			formSMT = "(exists (" + existForm.quantVarName + "::int) " + convertFormulaToSMT(existForm.clause) + ")"
		}
		else
		{
			// default -> should not happen
			formSMT = convertNameToSMT(form.toString())
		}
		
		cacheForm2SMT.put(form.toString(), formSMT)

		return formSMT
	}
	
	private def convertExprToSMT(expr : Expression) : String =
	{
		var exprSMTOpt = cacheExpr2SMT.get(expr.toString())
		
		if (exprSMTOpt != None) return exprSMTOpt.get

		// translate function symbols into the standard theory of function updates

		var exprSMT = ""

		if (expr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = expr.asInstanceOf[FunctionExpression]
			
			if (funcExpr.name == Constants.ARRAY_UPDATE_OPER)
			{
				// convert function expression "update(f,args,e)" to "(update f (args) e)"

				exprSMT = "(" + Constants.ARRAY_UPDATE_OPER + " " + funcExpr.args(0)
				
				exprSMT += " ("
				
				for (i <- 1 to (funcExpr.args.length - 2))
				{
					if (i > 1) exprSMT += " "
					
					exprSMT = exprSMT + convertExprToSMT(funcExpr.args(i))
				}
				
				exprSMT += ") "
				
				exprSMT = exprSMT + convertExprToSMT(funcExpr.args(funcExpr.args.length - 1))
				
				exprSMT = exprSMT + ")"
			}
			else
			{
				// the first argument can be "update" so we must convert it too
				
				exprSMT = "(" + convertExprToSMT(funcExpr.args(0))
			
				for (i <- 1 to (funcExpr.args.length - 1))
				{
					exprSMT = exprSMT + " " + convertExprToSMT(funcExpr.args(i))
				}
			
				exprSMT = exprSMT + ")"
			}
		}
		else if (expr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = expr.asInstanceOf[ArithmeticExpression]
			
			exprSMT = "(" + getOperatorString(arExpr.op) + " " + convertExprToSMT(arExpr.left) + " " + convertExprToSMT(arExpr.right) + ")"			
		}
		else if (ExpressionUtils.isFieldAccessPath(expr)) 
		{
			val fapTgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(expr)
		
			val fieldNames = ExpressionUtils.extractFieldNamesFromFieldAccessPath(expr)

			exprSMT = convertExprToSMT(fapTgtObjExpr)
			
			for (fname <- fieldNames)
			{
				exprSMT = "(" + fname + " " + exprSMT + ")"
			}
		}
		else
		{
			exprSMT = convertNameToSMT(expr.toString())
		}

		cacheExpr2SMT.put(expr.toString(), exprSMT)

		return exprSMT
	}
	
	private def convertNameToSMT(vname : String) : String =
	{
		return vname.replace('.', '_').replace('[', '_').replace(']', '_')	
	}
	
	private def getOperatorString(op : String) : String =
	{
		if (op == "%") return "mod"
		
		return op
	}
}
