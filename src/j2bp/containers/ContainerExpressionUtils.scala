package j2bp.containers

import common.Constants
import common.Expression
import common.FunctionExpression
import common.ArithmeticExpression


object ContainerExpressionUtils
{
	def createMapGetExprStr(targetMapObj : Expression, keyExpr : Expression) : String =
	{
		return BasicContainerModel.FUNC_MAP_GET + "(" + BasicContainerModel.FUNC_ARRAY_MAP + "," + targetMapObj.toString() + "," + keyExpr.toString() + ")"
	}
	
	def createMapGetExpr(targetMapObj : Expression, keyExpr : Expression) : Expression =
	{
		return new FunctionExpression(BasicContainerModel.FUNC_MAP_GET, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_MAP), targetMapObj, keyExpr))
	}
	
	def createMapGetExprStrFragment(targetMapObj : Expression) : String =
	{
		return BasicContainerModel.FUNC_MAP_GET + "(" + BasicContainerModel.FUNC_ARRAY_MAP + "," + targetMapObj.toString()
	}
	
	def createMapUpdateExpr(targetMapObj : Expression, keyExpr : Expression, valueExpr : Expression) : Expression =
	{
		return new FunctionExpression(Constants.ARRAY_UPDATE_OPER, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_MAP), targetMapObj, keyExpr, valueExpr))
	}
	
	def createMapSizeExprStr(targetMapObj : Expression) : String =
	{
		return BasicContainerModel.FUNC_MAP_SIZE + "(" + BasicContainerModel.FUNC_ARRAY_SIZE + "," + targetMapObj.toString() + ")"
	}
	
	def createMapSizeExpr(targetMapObj : Expression) : Expression =
	{
		return new FunctionExpression(BasicContainerModel.FUNC_MAP_SIZE, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_SIZE), targetMapObj))
	}
	
	def createMapResizeExpr(targetMapObj : Expression, arithOp : String) : Expression =
	{
		return new FunctionExpression(Constants.ARRAY_UPDATE_OPER, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_SIZE), targetMapObj, new ArithmeticExpression(arithOp, new FunctionExpression(BasicContainerModel.FUNC_MAP_SIZE, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_SIZE), targetMapObj)), Constants.ONE_EXPR)))
	}			

	def createMapOrderExprStr(targetMapObj : Expression, posExpr1 : Expression, posExpr2 : Expression) : String =
	{
		return BasicContainerModel.FUNC_MAP_ORDER + "(" + BasicContainerModel.FUNC_ARRAY_ITER + "," + targetMapObj.toString() + "," + posExpr1.toString() + "," + posExpr2.toString() + ")"
	}

	def createMapOrderExprStrFragment(targetMapObj : Expression) : String =
	{
		return BasicContainerModel.FUNC_MAP_ORDER + "(" + BasicContainerModel.FUNC_ARRAY_ITER + "," + targetMapObj.toString()
	}
	
	def createMapOrderExpr(targetMapObj : Expression, posExpr1 : Expression, posExpr2 : Expression) : Expression =
	{
		return new FunctionExpression(BasicContainerModel.FUNC_MAP_ORDER, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_ITER), targetMapObj, posExpr1, posExpr2))
	}
	
	def createMapKeysViewExpr(targetMapObj : Expression, viewMapObj : Expression) : Expression =
	{
		return new FunctionExpression(BasicContainerModel.FUNC_MAP_KEYS, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_KEYS), targetMapObj, viewMapObj))
	}
	
	def createMapValuesViewExpr(targetMapObj : Expression, viewMapObj : Expression) : Expression =
	{
		return new FunctionExpression(BasicContainerModel.FUNC_MAP_VALUES, Array[Expression](new Expression(BasicContainerModel.FUNC_ARRAY_VALUES), targetMapObj, viewMapObj))
	}
		
}
