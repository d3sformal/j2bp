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
package util;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.analysis.*;


public class BytecodeChecker
{
	public static void main(String[] args)
	{
		String className = args[0];
		String methodName = args[1];
		
		try
		{
			ClassReader cr = new ClassReader(className);
			ClassNode cn = new ClassNode();
			cr.accept(cn, 0);
	
			for (int i = 0; i < cn.methods.size(); i++)
			{
				MethodNode mn = (MethodNode) cn.methods.get(i);
				
				if (mn.name.equals(methodName))
				{
					System.out.println("METHOD CODE");
					System.out.println("===========");
					
					for (int j = 0; j < mn.instructions.size(); j++)
					{
						AbstractInsnNode insn = mn.instructions.get(j);
						System.out.println(String.valueOf(j+1) + ":" + insn);
					}
					
					System.out.println("ANALYSIS");
					System.out.println("========");
					
					Analyzer bca = new Analyzer(new SimpleVerifier());
					bca.analyze(className.replace('.', '/'), mn);
				}
			}
		}
		catch (Exception ex)
		{
			System.out.println(ex.getMessage());
			ex.printStackTrace();
		}
	}
}
