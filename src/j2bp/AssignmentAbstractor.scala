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

import common.AtomicPredicate
import common.Expression


abstract class AssignmentAbstractor
{
	def generateAbstractStore(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, srcValueExpr : Expression, bcIndex : Int, tgtVarType : String) : Unit

	def generateAbstractPutfield(ctx : AbstractionContext, mv : MethodVisitor, tgtObj : Expression, fieldName : String, srcValueExpr : Expression, bcIndex : Int, fieldType : String)

	def generateVariableLoad(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, tgtPred : AtomicPredicate, srcValueExpr : Expression, srcPredSet : Set[AtomicPredicate]) 	
}
