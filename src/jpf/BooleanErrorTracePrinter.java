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
package jpf;

import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;

import gov.nasa.jpf.Config;
import gov.nasa.jpf.ListenerAdapter;
import gov.nasa.jpf.search.Search;
import gov.nasa.jpf.jvm.JVM;
import gov.nasa.jpf.jvm.ThreadInfo;
import gov.nasa.jpf.jvm.MethodInfo;
import gov.nasa.jpf.jvm.ClassInfo;
import gov.nasa.jpf.jvm.ElementInfo;
import gov.nasa.jpf.jvm.StaticElementInfo;
import gov.nasa.jpf.jvm.DynamicArea;
import gov.nasa.jpf.jvm.StaticArea;
import gov.nasa.jpf.jvm.StackFrame;
import gov.nasa.jpf.jvm.LocalVarInfo;
import gov.nasa.jpf.jvm.bytecode.Instruction;
import gov.nasa.jpf.jvm.bytecode.FieldInstruction;
import gov.nasa.jpf.jvm.bytecode.PUTFIELD;
import gov.nasa.jpf.jvm.bytecode.PUTSTATIC;
import gov.nasa.jpf.jvm.bytecode.LocalVariableInstruction;
import gov.nasa.jpf.jvm.bytecode.ISTORE;


public class BooleanErrorTracePrinter extends ListenerAdapter
{
	private String includePrefix = "";
	
	private List<AssignmentInfo> curTransition;
	
	private List< List<AssignmentInfo> > curPath;
	
	
	public BooleanErrorTracePrinter(Config cfg)
	{
		includePrefix = cfg.getString("betprinter.include", "");
		
		curTransition = new ArrayList<AssignmentInfo>();
		
		curPath = new ArrayList< List<AssignmentInfo> >();
	}
	
	public void searchStarted(Search search)
	{
		curTransition.clear();	
	}
	
	public void stateAdvanced(Search search)
	{
		List<AssignmentInfo> tr = new ArrayList<AssignmentInfo>();
		tr.addAll(curTransition);
		
		curPath.add(tr);
		
		curTransition.clear();
	}
	
	public void stateBacktracked(Search search)
	{
		curPath.remove(curPath.size()-1);	
	}	
	
	public void instructionExecuted(JVM vm)
	{
		Instruction insn = vm.getLastInstruction();
		ThreadInfo ti = vm.getLastThreadInfo();
		
		if ((insn instanceof PUTFIELD) || (insn instanceof PUTSTATIC))
		{
			FieldInstruction fieldInsn = (FieldInstruction) insn;
			
			MethodInfo mi = insn.getMethodInfo();
			
			// ignore library methods
			if ( ! mi.getBaseName().startsWith(includePrefix) ) return;
			
			AssignmentInfo ai = new AssignmentInfo();
			
			ai.codeLocation = mi.getBaseName() + ":" + insn.getInstructionIndex();
			
			ai.targetVarName = fieldInsn.getVariableId();
			
			ai.newValue = (int) fieldInsn.getLastValue();
			
			ai.varsInfo = extractVariablesInfo(vm);
			
			curTransition.add(ai);
		}
		
		if (insn instanceof ISTORE)
		{
			LocalVariableInstruction locInsn = (LocalVariableInstruction) insn;
			
			MethodInfo mi = insn.getMethodInfo();
			
			// ignore library methods
			if ( ! mi.getBaseName().startsWith(includePrefix) ) return;
			
			int miArgsCount = mi.getNumberOfArguments();
			
			// ignore "String[] args"
			if (mi.getName().equals("main")) miArgsCount--;
			
			int locVarIndex = locInsn.getLocalVariableIndex();
			
			AssignmentInfo ai = new AssignmentInfo();
			
			ai.codeLocation = mi.getBaseName() + ":" + insn.getInstructionIndex();
			
			if (locVarIndex < miArgsCount) ai.targetVarName = "bp" + locVarIndex;
			else ai.targetVarName = "bv" + (locVarIndex - miArgsCount);
			
			ai.newValue = ti.getLocalVariable(locVarIndex);
			
			ai.varsInfo = extractVariablesInfo(vm);
			
			curTransition.add(ai);
		}
	}
	
	public void propertyViolated(Search search) 
	{
		JVM vm = search.getVM();

		
		// print current trace (path)
		printRecordedTrace();

		
		// print snapshot (variable info)		
		
		Map<String, List<VariableInfo>> entity2varsinfo = extractVariablesInfo(vm);		

		for (Map.Entry<String, List<VariableInfo>> me : entity2varsinfo.entrySet())
		{
			String entityName = me.getKey();
			List<VariableInfo> varsInfo = me.getValue();
			
			System.out.println("[SNAPSHOT] entity name = '" + entityName + "'");
			
			for (VariableInfo vi : varsInfo) 
			{
				System.out.println("[SNAPSHOT] \t name = '" + vi.name + "', value = " + vi.val);
			}
		}
	}
	
	private void printRecordedTrace()
	{
		for (List<AssignmentInfo> transition : curPath)
		{
			for (AssignmentInfo ai : transition)
			{
				System.out.println("[TRACE] code location = '" + ai.codeLocation + "'");
				
				System.out.println("[TRACE] \t target variable = '" + ai.targetVarName + "', new value = " + ai.newValue);		
			}			
		}
	}
	
	private Map<String,List<VariableInfo>> extractVariablesInfo(JVM vm)
	{
		Map<String, List<VariableInfo>> entity2varsinfo = new LinkedHashMap<String, List<VariableInfo>>();
		
		
		// static fields
		
		StaticArea sa = vm.getStaticArea();
		
		for (int i = 0; i < sa.size(); i++)
		{
			ElementInfo sei = sa.get(i);
			if (sei == null) continue;
			
			ClassInfo seiCI = sei.getClassInfo();
			if (seiCI == null) continue;
			
			String className = seiCI.getName();

			// ignore library classes
			if ( ( ! className.startsWith(includePrefix) ) && ( ! className.startsWith("[L" + includePrefix) ) ) continue;
		
			List<VariableInfo> varsInfo = entity2varsinfo.get(className);
			
			if (varsInfo == null) varsInfo = new ArrayList<VariableInfo>();
			
			entity2varsinfo.put(className, varsInfo);
						
			for (int k = 0; k < sei.getNumberOfFields(); k++)
			{
				// internal field
				if (sei.getFieldInfo(k).getName().indexOf("$") != -1) continue;
				
				VariableInfo vi = new VariableInfo();
				
				vi.name = sei.getFieldInfo(k).getName();
				vi.val = (sei.getFields().getBooleanValue(k) == true) ? 1 : 0;
				
				varsInfo.add(vi);
			}
		}

		
		// method parameters and local variables
		
		ThreadInfo ti = ThreadInfo.getCurrentThread();
		
		for (StackFrame sf : ti.getStack())
		{
			String mthName = sf.getMethodInfo().getBaseName();
			
			// ignore library methods
			if ( ! mthName.startsWith(includePrefix) ) continue;			
			
			List<VariableInfo> varsInfo = entity2varsinfo.get(mthName);
			
			if (varsInfo == null) varsInfo = new ArrayList<VariableInfo>();
			
			entity2varsinfo.put(mthName, varsInfo);
			
			int argsCount = sf.getMethodInfo().getNumberOfArguments();
			
			// ignore "String[] args"
			if (sf.getMethodInfo().getName().equals("main")) argsCount--;
			
			for (int i = 0; i < sf.getLocalVariableCount(); i++)
			{
				// ignore "String[] args"
				if (sf.getMethodInfo().getName().equals("main") && (i == 0)) continue;

				VariableInfo vi = new VariableInfo();
				
				if (i < argsCount) vi.name = "bp" + i;
				else vi.name = "bv" + (i - argsCount);				
					
				vi.val = sf.getLocalVariable(i);
				
				varsInfo.add(vi);
			}
		}
		
		return entity2varsinfo;
	}

	
	class VariableInfo
	{
		public String name;
		public int val;
	}
	
	class AssignmentInfo
	{
		public String codeLocation;
		public String targetVarName;
		public int newValue;
		public Map<String, List<VariableInfo>> varsInfo;
	}
}
