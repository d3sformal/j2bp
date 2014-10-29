package j2bp.analysis

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 

import common.Constants
import common.ExpressionUtils

import j2bp.Main
import j2bp.WALAUtils
import j2bp.ExecutionVisitor
import j2bp.ExecutionSimulator


object LivenessAnalyzer
{
	def computeLivenessInfo(progClasses : List[String], scopeDetector : ExecutionVisitor) : Map[(String,String), List[(Int,Int)]] =
	{
		// liveness info: map from (full method name, local variable name) to the list of bytecode ranges
		var mthvar2livebcidxs : Map[(String,String), List[(Int,Int)]] = new HashMap

		
		// compute liveness analysis for each local variable in each method of program classes 

		val ctx = Main.initNewContext(progClasses)
		
		for (clsName <- ctx.getProgramClasses())
		{
			val cls = WALAUtils.findClass(clsName)
			
			ctx.initCurClass(clsName)
			
			for (mth <- cls.getDeclaredMethods())
			{				
				val mthName = WALAUtils.getFullMethodName(mth)
				
				val mthNameShort = WALAUtils.getShortMethodName(mth)
				
				val mthParamCount = ctx.getMethodOrigParamCount(clsName, mthNameShort)
				
				val mthIsAbstract = cls.isInterface() || mth.isAbstract()
				
				ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic())
				
				
				var localVarNamesRefs : Map[String, (Int, Int)] = new HashMap				

				var dataHolder : Map[String,java.lang.Object] = new HashMap
			
				dataHolder.put("varnamesrefs", localVarNamesRefs)
				
				ExecutionSimulator.simulateMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface() || mth.isAbstract(), ctx, scopeDetector, dataHolder)
				
				localVarNamesRefs = dataHolder.get("varnamesrefs").get.asInstanceOf[Map[String, (Int, Int)]]
				
				
				for ( (localVarName, lvNameRefs) <- localVarNamesRefs )
				{
					val firstRefBCIndex = lvNameRefs._1
					val lastRefBCIndex = lvNameRefs._2
			
					var liveBCRange = (firstRefBCIndex, lastRefBCIndex)
						
					var liveBCIdxs = mthvar2livebcidxs.getOrElse( (mthName, localVarName), List[(Int,Int)]() )
						
					liveBCIdxs = liveBCIdxs :+ liveBCRange
						
					mthvar2livebcidxs.put( (mthName, localVarName), liveBCIdxs )
				}
			}
		}

		if (Main.INFO)
		{
			println("[INFO] liveness information for method variables:")
			
			for ( (mthvar, liveBCIndexes) <- mthvar2livebcidxs )
			{
				print("\t variable identification = " + mthvar._1 + "." + mthvar._2 + ", live bytecode ranges = ")
				
				for (bcRange <- liveBCIndexes) print("(" + bcRange._1 + "," + bcRange._2 + ") ")
				println("")				
			}
		}
		
		return mthvar2livebcidxs
	}
}
