package common


object Constants
{
	val BOOL_STATICFIELD_NAME_PREFIX : String = "bs"
	val BOOL_OBJECTFIELD_NAME_PREFIX : String = "bo"
	val BOOL_MTHPARAM_NAME_PREFIX : String = "bp"
	val BOOL_LOCALVAR_NAME_PREFIX : String = "bv"

	val MTHPARAM_PREFIX : String = "param"
	val LOCALVAR_PREFIX : String = "local"
	
	val STACK_ELEM_DUMMY : String = "[dummy]"
	
	val STACK_ELEM_NEWOBJ : String = "[newobj]"
	
	val STACK_ELEM_RETVAL_DET : String = "[retvaldet]"
	val STACK_ELEM_RETVAL_NDT : String = "[retvalndt]"
		
	val LOCKMNGR_CLASS : String = "runtime.BooleanLockManager"
	val LOCKMNGR_LOCK : String = "lock"
	val LOCKMNGR_IS_LOCKED : String = "isLocked"
	val LOCKMNGR_WAIT_FOR_LOCK : String = "waitForLock"
	val LOCKMNGR_UNLOCK : String = "unlock"
	val LOCKMNGR_WAIT : String = "wait"
	val LOCKMNGR_NOTIFY : String = "notify"
	
	val FUNC_FIELD_READ : String = "fread"
	val FUNC_FIELD_WRITE : String = "fwrite"
	
	val FUNC_ARRAY_READ : String = "aread"
	val FUNC_ARRAY_WRITE : String = "awrite"	
	val FUNC_ARRAY : String = "arr"

	val ARRAY_UPDATE_OPER : String = "update"

	val TEMP_VAR_PREFIX : String = "tmp"
	
	val TEMP_RETVAR_PREFIX : String = TEMP_VAR_PREFIX + "r"
	
	val RESULT_VAR_MARK : String = "[result]"
	val OUTPUT_PARAM_MARK : String = "[output]"
	
	val VALUE_NULL : String = "null"
	val NULL_EXPR = new Expression(VALUE_NULL)
	
	val QUANT_VAR_PREFIX : String = "q"
	
	val LOGIC_VAR_PREFIX : String = "fl"

	val PROPERTY_VAR : String = "prop"
	
	val ZERO_EXPR = new Expression("0")
	val ONE_EXPR = new Expression("1")
	
	val EMPTY_STR_EXPR = new StringExpression("")
	
	val TRUE_PRED : AtomicPredicate = new BinaryPredicate("=", new Expression("1"), new Expression("1"))
	val FALSE_PRED : AtomicPredicate = new BinaryPredicate("=", new Expression("1"), new Expression("0"))
	
	val PROP_HOLDS_PRED : AtomicPredicate = new BinaryPredicate("=", new Expression(PROPERTY_VAR), new Expression("1"))
}
