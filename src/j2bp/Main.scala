package j2bp

import java.io.File
import java.io.FileWriter

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.io.Source._

import com.ibm.wala.ipa.callgraph.AnalysisScope
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod}
import com.ibm.wala.types.TypeReference
import com.ibm.wala.shrikeBT._ 

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.analysis.Analyzer
import org.objectweb.asm.tree.analysis.BasicVerifier

import common.Constants
import common.Expression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.Negation
import common.ExpressionUtils
import common.FormulaUtils
import common.Property

import util.StringUtils

import j2bp.analysis.LivenessAnalyzer
import j2bp.analysis.ValuesAnalyzer
import j2bp.containers.BasicContainerModel


object Main
{
	val INFO = Configuration.printInfoMsgs
	val DEBUG = Configuration.printDebugMsgs
	
	
	/**
	 * command-line arguments:
	 * 1) main class of the component
	 * 2) file with names of classes making the component
	 * 3) directory containing the component
	 * 4) file containing the user-defined input predicates
	 * 5) file containing the user-defined input properties
	 * 6) file containing enforced backtrack location
	 * 7) exclusion file for WALA
	 * 8) output directory
	 */
	def main(args: Array[String]) =
	{
		val mainClassName : String = args(0)
		val progClassesFileName : String = args(1)
		val progDirName : String = args(2)
		val maxContainerSizeStr : String = args(3)
		val predicatesFileName : String = args(4)
		val propertiesFileName : String = args(5)
		val settingsFileName : String = args(6)
		val walaExclusionFileName : String = args(7)
		val outputDirName : String = args(8)

		
		printCurrentTime("[J2BP] start")
		
				
		// true -> support for containers		
		Configuration.init(true)

		// maximal possible container size
		BasicContainerModel.maxSize = java.lang.Integer.parseInt(maxContainerSizeStr)
		
		// set all counters to 0
		Configuration.smtProvider.resetCounters()
		
		
		var progClasses = List[String]()
		
		if (progClassesFileName != "none")
		{
			for (clsName <- fromFile(progClassesFileName).getLines()) progClasses = progClasses :+ clsName
		}
		else
		{
			progClasses = progClasses :+ mainClassName
		}

		if (INFO) 
		{
			println("[INFO] main class of the program: " + mainClassName)
			
			print("[INFO] all classes: " + mainClassName)
			for (clsName <- progClasses)
			{
				if (clsName != mainClassName) print(", " + clsName)
			}
			println("")
			
			println("[INFO] program directory: " + progDirName)
			
			println("[INFO] wala exclusion file: " + walaExclusionFileName)
		}
		
		
		// load settings
		
		var backtrackCodeLocations = List[(String,Int)]()
		
		if (settingsFileName != "none")
		{
			for (line <- fromFile(settingsFileName).getLines())
			{
				if (line.startsWith("forcebacktr"))
				{
					val codeLocsStr = line.substring(12)
					val codeLocStrArray = codeLocsStr.split(";")
					
					for ( i <- 0 to (codeLocStrArray.length - 1) )
					{
						val codeLocStr = codeLocStrArray(i).split(":")	
									
						val fullMethodName = codeLocStr(0)
						val bytecodePos = java.lang.Integer.parseInt(codeLocStr(1))
					
						backtrackCodeLocations = backtrackCodeLocations :+ (fullMethodName, bytecodePos)
						
						if (INFO) println("[INFO] forced backtrack location: method name = " + fullMethodName + ", bytecode index = " + bytecodePos)
					}
				}				
			}			
		}

		
		// initialize class hierarchy, compute call graph, etc
		WALAUtils.initLibrary(progDirName, walaExclusionFileName, mainClassName)		
		
		
		// compute possible values for each method local variable
		val mthvar2possvalues = ValuesAnalyzer.computePossibleValues(progClasses)
		
		if (Configuration.autoGenProperties)
		{
			// use linear bytecode scan of individual methods to find all built-in properties for method calls 
			val genPropList : List[Property] = Configuration.propertiesGnrt.inferProperties(progClasses)
		
			// save all generated properties into manager
			Configuration.propertiesMngr.loadGeneratedProperties(genPropList)
		}

		// load additional user-defined properties
		if (propertiesFileName != "none") Configuration.propertiesMngr.loadPropertiesFromFile(propertiesFileName)

		
		printCurrentTime("[J2BP] init completed")
				
	
		println("[J2BP] properties:")
		Configuration.propertiesMngr.printAllPropertiesToConsole("\t")

		
		val suffixFile : FileWriter = new FileWriter(outputDirName + "/" + mainClassName.replace('.', '_') + "_suffixes")
				
		var propID = 0
						
		// generate abstract program for each property
		for (prop <- Configuration.propertiesMngr.getAllProperties())
		{
			propID = propID + 1
			
			printCurrentTime("[J2BP] started generating abstraction for property " + propID)
	
			// init new context
			val mainCtx = initNewContext(progClasses)
			
			mainCtx.setMainClass(mainClassName)
			
			mainCtx.setAbsClassNameSuffix("PA"+propID)
			
			mainCtx.setCurrentProperties(List[Property](prop))
			
			mainCtx.setForcedBacktrackLocations(backtrackCodeLocations)
			
			
			// reset data structures
			Configuration.predicatesMngr.clearAll()
			Configuration.predicateSemModel.clearAll()

			
			println("[J2BP] generating abstraction " + propID + " for the property " + prop.toString())

			suffixFile.write("PA"+propID+"\n")

			
			if (Configuration.autoGenPredicates)
			{
				// generate predicates
	
				// use linear bytecode scan of individual methods to identify all necessary predicates over containers (size, content, added values) 
				val genPredLists : (Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]]) = Configuration.predicatesGnrt.inferPredicates(progClasses, mthvar2possvalues)
				
				// save all generated predicates into manager
				Configuration.predicatesMngr.loadGeneratedPredicates(genPredLists._1, genPredLists._2, genPredLists._3)
		
				if (Main.INFO)
				{
					println("[INFO] predicates generated:")
					Configuration.predicatesMngr.printAllPredicatesToConsole("\t")
				}
				
				// filter out generated predicates that are unnecessary for given property
				val reqPredicatesWithMethodNames = Configuration.propertiesMngr.getAtomicPredicatesWithMethodName(mainCtx.getCurrentProperties())
				Configuration.predicatesMngr.dropUnnecessaryPredicates(reqPredicatesWithMethodNames, progClasses)
			}
			
			// load additional user-defined predicates		
			if (predicatesFileName != "none") Configuration.predicatesMngr.loadPredicatesFromFile(predicatesFileName)
						
	
			// eliminate quantifiers from the property
			
			var qfPropList = List[Property]()
			
			for ( (fullMthName, bcPos) <- prop.codeLocations )
			{
				// identify matching expressions for each quantified variable
				
				val clsName = StringUtils.extractClassName(fullMthName)
				val shortMthName = StringUtils.extractShortMethodName(fullMthName)
				
				val matchingExprs = Configuration.predicateSemModel.findMatchingExpressions(prop.formula, Configuration.predicatesMngr.getAllPredicatesForCodeLocation(fullMthName, bcPos), clsName, shortMthName)
				
				var logvar2values : Map[Expression, Set[Expression]] = new HashMap
				
				for ( (varExpr, matchingExprSet) <- matchingExprs )
				{
					if (ExpressionUtils.isLogicVariable(varExpr)) logvar2values.put(varExpr, matchingExprSet) 
				}
				
				// replace subformulas with conjunctions or disjunctions properly
				val newExistQuantFreeFormula = FormulaUtils.eliminateQuantifiers(prop.formula, logvar2values)
				
				// we assume that remaining logic variables are implicitly universally quantified
				val newImplUnivQuantFormula = FormulaUtils.eliminateImplicitUniversalQuantification(newExistQuantFreeFormula, logvar2values)
			
				val newQuantFreeProp = new Property(newImplUnivQuantFormula, List[(String,Int)]( (fullMthName, bcPos) ))
	
				println("[J2BP] quantifier free property formula: " + newQuantFreeProp.formula)
				
				qfPropList = qfPropList :+ newQuantFreeProp
			}
						
			// set again the current properties
			mainCtx.setCurrentProperties(qfPropList)
			
			
			// load atomic predicates in the property formula
			for (qfProp <- qfPropList)
			{
				for ( (fullMthName, bcPos) <- qfProp.codeLocations )
				{
					val shortMthName = StringUtils.extractShortMethodName(fullMthName)
					val className = StringUtils.extractClassName(fullMthName)
					
					for (atomPred <- FormulaUtils.extractAtomicPredicates(qfProp.formula))
					{
						var skip = false
						
						if (FormulaUtils.containsLogicVariables(atomPred)) skip = true
						
						if (atomPred.isInstanceOf[BinaryPredicate])
						{
							val binAtomPred = atomPred.asInstanceOf[BinaryPredicate]
							if (binAtomPred.left == binAtomPred.right) skip = true
						}
						
						if ( ! skip )
						{
							if (Configuration.predicatesMngr.isPredicateOverMethodVariables(atomPred)) 
							{
								Configuration.predicatesMngr.addMethodPredicate(className, shortMthName, atomPred)
							}
							else if (Configuration.predicatesMngr.isPredicateOverObjectFields(atomPred)) 
							{
								Configuration.predicatesMngr.addObjectPredicate(className, atomPred)
							}
							else
							{
								Configuration.predicatesMngr.addStaticPredicate(className, atomPred)
							}	
						}
					}
				}
			}

			
			// drop all predicates with multiplication
			Configuration.predicatesMngr.dropPredicatesWithNonlinearArithmetic()
		
			// we do not want cached records with dropped predicates
			Configuration.predicatesMngr.invalidateCaches()
			
			Configuration.predicatesMngr.clearMissingPredicates()

			
			// compute live bytecode ranges for local variables in each method in each program class
			// scope depends on available predicates so it has to be called when predicates are loaded (generated)
			val mthvar2livebcidxs = LivenessAnalyzer.computeLivenessInfo(progClasses, Configuration.varScopeDetector)

							
			// compute all possible predicates over result variables for each method
			JavaCodeAbstractor.findResultPredicates(mainCtx, progClasses)
	
			mainCtx.resetTempReturnVariables()
			
	
			// derive all necessary supporting predicates from the available input predicates
			Configuration.predicateSemModel.derivePermanentSupportingFormulas()
			
			// use liveness information to determine scope (bytecode indexes) for predicates
			Configuration.predicatesMngr.defineScopeForPredicatesOverMethodVars(mthvar2livebcidxs)
			
			
			println("[J2BP] predicates (filtered and loaded):")
			Configuration.predicatesMngr.printAllPredicatesToConsole("\t")
			
						
			// compute basic information about the generated abstract methods
			
			for (clsName <- progClasses)
			{
				val cls = WALAUtils.findClass(clsName)
			
				for (mth <- cls.getDeclaredMethods())
				{
					val mthName = WALAUtils.getShortMethodName(mth)
					
					val mthSig = mth.getSignature()
					
					val mthDescr = mthSig.substring(mthSig.indexOf('('))
					
					val origParamCount = mainCtx.getMethodOrigParamCount(clsName, mthName)
	
					
					// get relevant classes for computing the method parameters and the presence of a return value
					
					var relParamClassNames = List[String](clsName)
					var relRetvalClassNames = List[String](clsName)
					
					var clsSuper = cls.getSuperclass()
					while (clsSuper != null)
					{
						relParamClassNames = relParamClassNames :+ WALAUtils.getClassName(clsSuper)
						relRetvalClassNames = relRetvalClassNames :+ WALAUtils.getClassName(clsSuper)
						
						clsSuper = clsSuper.getSuperclass()
					}
					
					for (clsItf <- cls.getAllImplementedInterfaces())
					{
						relParamClassNames = relParamClassNames :+ WALAUtils.getClassName(clsItf)
						relRetvalClassNames = relRetvalClassNames :+ WALAUtils.getClassName(clsItf)
					}
					
					for ( subClassName <- WALAUtils.findSubclassesWithMethod(clsName, mthName, mthDescr, progClasses) ) relParamClassNames = relParamClassNames :+ subClassName
	
					
					// compute the number of parameters for abstract method
					// we consider superclasses, implemented interfaces, and all subclasses				
	
					
					// compute number of abstract parameters				
					
					var absParamCount = 0
			
					for (rpClsName <- relParamClassNames)
					{
						var rpClsAbsParamCount = 0
						
						for (i <- 1 to origParamCount)
						{
							val formalParamName = Constants.MTHPARAM_PREFIX + i
							
							val formalParamPreds = Configuration.predicatesMngr.getPredicatesOverExpr(rpClsName, mthName, formalParamName, -1)
							
							rpClsAbsParamCount = rpClsAbsParamCount + formalParamPreds.size()
						}
						
						if (rpClsAbsParamCount > absParamCount) absParamCount = rpClsAbsParamCount
					}
					
					// update the class and all its subclasses with the new information
					
					mainCtx.storeMethodAbsParamCount(clsName, mthName, absParamCount)
					
					for ( subClassName <- WALAUtils.findSubclassesWithMethod(clsName, mthName, mthDescr, progClasses) )
					{
						mainCtx.storeMethodAbsParamCount(subClassName, mthName, absParamCount)
					}
					
	
					// determine whether the abstract method has a return value
					// consider number of predicates over result variables for the method in the target class and all its superclasses and implemented interfaces
					// consider also number of predicates over method output parameters
	
					var resPredCount = mainCtx.getResultPredicatesForMethod(clsName, mthName).size + mainCtx.getOutputParamPredicatesCount(clsName, mthName)
					
					for (rrClsName <- relRetvalClassNames)
					{
						var rrClsPredCount = mainCtx.getResultPredicatesForMethod(rrClsName, mthName).size + mainCtx.getOutputParamPredicatesCount(rrClsName, mthName)
	
						if (rrClsPredCount > resPredCount) resPredCount = rrClsPredCount					
					}
	
					mainCtx.storeMethodAbsResultType(clsName, mthName, (resPredCount > 0))
				}
			}
			
			if (Main.INFO) 
			{
				for (clsName <- progClasses)
				{
					val cls = WALAUtils.findClass(clsName)
				
					for (mth <- cls.getDeclaredMethods())
					{
						val mthName = WALAUtils.getShortMethodName(mth)
	
						println("[INFO] class name = " + clsName + ", method name = " + mthName + ", orig param count = " + mainCtx.getMethodOrigParamCount(clsName, mthName) + ", abstract param count = " + mainCtx.getMethodAbsParamCount(clsName, mthName) + ", orig return value = " + (mth.getReturnType() != TypeReference.Void) + ", abstract return value = " + mainCtx.hasMethodAbsReturnValue(clsName, mthName))
					}
				}
			}
			
			
			// generate boolean abstraction of each class
			
			for (className <- progClasses)
			{
				val cls = WALAUtils.findClass(className)
					
				JavaCodeAbstractor.generateAbstractionForClass(mainCtx, className, outputDirName)
			}
			
			printCurrentTime("[J2BP] finished generating abstraction for property " + propID)
			
			println("[J2BP] total number of SMT solver calls = " + Configuration.smtProvider.getCountersValue())
						
			if (Configuration.collectMissingPredicates)
			{
				FormulaUtils.printAtomPredSet(Configuration.predicatesMngr.getMissingPredicates(), "[J2BP] missing input predicates:")
			}
		}
		
		suffixFile.close()
	}

	def initNewContext(progClasses : List[String]) : AbstractionContext =
	{
		val ctx = new AbstractionContext()
		
		ctx.setProgramClasses(progClasses)
		
		for (className <- progClasses)
		{
			val cls = WALAUtils.findClass(className)
				
			// store original numbers of method parameters
			for (mth <- cls.getDeclaredMethods())
			{
				JavaCodeUtils.loadOrigMethodInfo(ctx, className, mth.asInstanceOf[IBytecodeMethod])
			}
		}
		
		return ctx
	}
	
	def printCurrentTime(message : String) =
	{
		val fmt : java.text.SimpleDateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
		println(message + " at " + fmt.format(new java.util.Date()))
	}
	
	def printCurrentTimeMS(message : String) =
	{
		val fmt : java.text.SimpleDateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
		println(message + " : current time = " + fmt.format(new java.util.Date()))
	}
}
