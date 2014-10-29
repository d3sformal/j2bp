package j2bp

import j2bp.analysis.DefaultVarScopeDetector
import j2bp.containers._


object Configuration
{
	var predicatesMngr : PredicatesManager = null
	
	var predicateSemModel : PredicateSemanticsModel = null

	var predicatesGnrt : PredicatesGenerator = null
	
	var assignAbstr : AssignmentAbstractor = null
	
	var mthcallAbstr : MethodCallAbstractor = null
	
	var propertiesMngr : PropertiesManager = null
	
	var propertiesGnrt : PropertiesGenerator = null
	
	var varScopeDetector : ExecutionVisitor = null
	
	
	val smtProvider : SMTProvider = new YicesSMTProvider
	
	
	val printInfoMsgs = true
	val printDebugMsgs = false
	
	// false -> only single thread programs
	val multipleThreads = false
	
	val autoGenProperties = false
	val autoGenPredicates = false
	
	// determines whether to compute results by (1) processing cubes in the order of increasing size or (2) just considering cubes of the maximal size
	val processCubesWithIncSize = false
	
	// collect atomic predicates that make a part of some weakest precondition but are not available in the input set (user-defined or automatically inferred)
	val collectMissingPredicates = true
	
	
	def init(withContainers : Boolean) =
	{
		if (withContainers)
		{
			predicatesMngr = new ContainerPredicatesManager
			predicateSemModel = new ContainerPredicateSemanticsModel
			predicatesGnrt = new ContainerPredicatesGenerator
			assignAbstr = new ContainerAssignmentAbstractor
			mthcallAbstr = new ContainerMethodCallAbstractor			
			propertiesMngr = new ContainerPropertiesManager
			propertiesGnrt = new ContainerPropertiesGenerator			
			varScopeDetector = new ContainerVarScopeDetector
		}
		else
		{
			predicatesMngr = new DefaultPredicatesManager
			predicateSemModel = new DefaultPredicateSemanticsModel
			predicatesGnrt = new DefaultPredicatesGenerator
			assignAbstr = new DefaultAssignmentAbstractor
			mthcallAbstr = new DefaultMethodCallAbstractor
			propertiesMngr = new DefaultPropertiesManager
			propertiesGnrt = new DefaultPropertiesGenerator
			varScopeDetector = new DefaultVarScopeDetector
		}
	}
}
