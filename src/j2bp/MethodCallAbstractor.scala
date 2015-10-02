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
package j2bp

import org.objectweb.asm.MethodVisitor

import common.Expression


abstract class MethodCallAbstractor
{
	def generateInternalCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, dynTargetClasses : List[String], bcIndex : Int)
	
	def generateLibraryCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, paramCount : Int, retVarExpr : Expression, retVarType : String, bcIndex : Int) : Unit
	
	def isIgnoredLibraryMethod(className : String, methodName : String) : Boolean
	
	def preprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String, tgtMethodParamCount : Int, isStaticCall : Boolean) : (Expression, Array[Expression], Boolean)
	
	def postprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String)
}
