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
package j2bp;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Stack;
import java.util.Iterator;
import java.io.File;

import com.ibm.wala.util.config.AnalysisScopeReader;
import com.ibm.wala.ipa.callgraph.AnalysisScope;
import com.ibm.wala.ipa.callgraph.AnalysisOptions;
import com.ibm.wala.ipa.callgraph.Entrypoint;
import com.ibm.wala.ipa.callgraph.AnalysisCache;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.CallGraphBuilder;
import com.ibm.wala.ipa.callgraph.CGNode;
import com.ibm.wala.ipa.callgraph.impl.Everywhere;
import com.ibm.wala.ipa.callgraph.impl.Util;
import com.ibm.wala.ipa.callgraph.impl.DefaultEntrypoint;
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis;
import com.ibm.wala.ipa.callgraph.propagation.PointerKey;
import com.ibm.wala.ipa.callgraph.propagation.LocalPointerKey;
import com.ibm.wala.ipa.callgraph.propagation.NormalAllocationInNode;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ipa.cha.ClassHierarchy;
import com.ibm.wala.ssa.SSAOptions;
import com.ibm.wala.ssa.IR;
import com.ibm.wala.types.TypeName;
import com.ibm.wala.types.TypeReference;
import com.ibm.wala.classLoader.IClass;
import com.ibm.wala.classLoader.IMethod;
import com.ibm.wala.classLoader.IField;
import com.ibm.wala.classLoader.CallSiteReference;
import com.ibm.wala.shrikeBT.IInstruction;


public class WALAUtils
{
	private static AnalysisScope scope = null;
	private static IClassHierarchy cha = null;
	private static CallGraph cg = null;
	private static PointerAnalysis pa = null;
	private static AnalysisCache cache = null;


	private static StringBuffer getClassName(TypeName clsType) throws Exception
	{
		StringBuffer cnameStrBuf = new StringBuffer();

		cnameStrBuf.append(clsType.getPackage().toUnicodeString().replace('/', '.'));
		cnameStrBuf.append(".");
		cnameStrBuf.append(clsType.getClassName().toUnicodeString());
	
		return cnameStrBuf;
	}

	public static String getClassName(IClass cls) throws Exception
	{
		if (cls == null) return null;
		
		StringBuffer cnameStrBuf = getClassName(cls.getName());
			
		return cnameStrBuf.toString();
	}
	
	public static String getShortMethodName(IMethod mth) throws Exception
	{
		if (mth == null) return null;
		
		return mth.getName().toUnicodeString();
	}
	
	public static String getFullMethodName(IMethod mth) throws Exception
	{
		if (mth == null) return null;
		
		TypeName clsType = mth.getDeclaringClass().getName();
		
		StringBuffer mnameStrBuf = getClassName(clsType);

		mnameStrBuf.append(".");
		mnameStrBuf.append(mth.getName().toUnicodeString());
		
		return mnameStrBuf.toString();
	}
	
	public static String getClassNameFromMethod(IMethod mth) throws Exception
	{
		if (mth == null) return null;
		
		IClass cls = mth.getDeclaringClass();
		
		return getClassName(cls);
	}
	
	public static String getShortFieldName(IField fld) throws Exception
	{
		if (fld == null) return null;
		
		return fld.getName().toUnicodeString();
	}

	public static void initLibrary(String targetDir, String exclusionFile, String mainClassName) throws Exception
	{
		createAnalysisScope(targetDir, exclusionFile);
		
		makeClassHierarchy();		

		AnalysisOptions options = createAnalysisOptions(mainClassName);		

		createCallGraph(options);
	}

	private static void createAnalysisScope(String targetDirName, String exclusionFileName) throws Exception
	{
		scope = AnalysisScopeReader.readJavaScope("primordial.txt", new File(exclusionFileName), WALAUtils.class.getClassLoader());

		AnalysisScopeReader.processScopeDefLine(scope, WALAUtils.class.getClassLoader(), "Application,Java,binaryDir," + targetDirName);
	}

	private static void makeClassHierarchy() throws Exception
	{	
		cha = ClassHierarchy.make(scope);
	}

	private static AnalysisOptions createAnalysisOptions(String targetClassName) throws Exception
	{
		Iterable<Entrypoint> entryPoints = createEntryPointsFromPublicMethods(cha, targetClassName);

		AnalysisOptions options = new AnalysisOptions(scope, entryPoints);
		
		options.setHandleStaticInit(true);
		
		return options;
	}
	
	private static void createCallGraph(AnalysisOptions options) throws Exception
	{
		cache = new AnalysisCache();	
		
		CallGraphBuilder builder = Util.makeVanillaZeroOneCFABuilder(options, cache, cha, scope);

		cg = builder.makeCallGraph(options, null);

		pa = builder.getPointerAnalysis();
	}

	private static Iterable<Entrypoint> createEntryPointsFromPublicMethods(IClassHierarchy cha, String targetClassName) throws Exception
	{
		IClass cls = findClass(targetClassName);
		if (cls == null) return null;

		List<Entrypoint> epList = new ArrayList<Entrypoint>();
		
		for (IMethod mth : cls.getDeclaredMethods())
		{
			if (mth.isPublic()) epList.add(new DefaultEntrypoint(mth, cha));					
		}				
		
		return epList;
	}
	
	public static void printCallGraph(CallGraph graph)
	{
		printCallGraph(graph, Integer.MAX_VALUE);
	}
		
	public static void printCallGraph(CallGraph graph, int maxLevel)
	{
		System.out.println("CALL GRAPH");
		System.out.println("==========");

		CGNode entryNode = graph.getFakeRootNode(); 
		printCallGraphNode(graph, entryNode, "method: ", maxLevel, 0);

		System.out.println("");
	}

	private static void printCallGraphNode(CallGraph graph, CGNode node, String prefix, int maxLevel, int curLevel)
	{
		if (curLevel > maxLevel) return;
		
		System.out.println(prefix + node.getMethod().getSignature());
		
		Iterator<CallSiteReference> callSitesIt = node.iterateCallSites();
		while (callSitesIt.hasNext())
		{
			CallSiteReference callSite = callSitesIt.next();
			
			Set<CGNode> targetNodes = graph.getPossibleTargets(node, callSite);
			
			for (CGNode tgtNode : targetNodes) printCallGraphNode(graph, tgtNode, "\t"+prefix, maxLevel, curLevel + 1);
		}
	}
	
	public static IClass findClass(String className) throws Exception
	{
		Iterator<IClass> clsIt = cha.iterator();
		while (clsIt.hasNext())
		{
			IClass cls = clsIt.next();
			
			if (className.equals(getClassName(cls))) return cls;
		}
		
		return null;
	}
	
	public static List<String> findSuperclasses(String className, List<String> progClasses) throws Exception
	{
		List<String> superclasses = new ArrayList<String>();
		
		IClass cls = findClass(className);
		
		IClass clsSuper = cls.getSuperclass();
		while (clsSuper != null)
		{
			String clsSuperName = getClassName(clsSuper);
			
			if (progClasses.contains(clsSuperName)) superclasses.add(clsSuperName);
			
			clsSuper = clsSuper.getSuperclass();
		}
		
		for (IClass clsItf : cls.getAllImplementedInterfaces())
		{
			String clsItfName = getClassName(clsItf);
			
			if (progClasses.contains(clsItfName)) superclasses.add(clsItfName);
		}
		
		return superclasses;
	}

	// look for subclasses that override given method
	public static List<String> findSubclassesWithMethod(String rootClassName, String mthName, String mthDescriptor, List<String> progClasses) throws Exception
	{
		List<String> subclasses = new ArrayList<String>();
		
		IClass rootClass = findClass(rootClassName);
		
		String rootClassCanonName = rootClass.getName().toString();

		Iterator<IClass> clsIt = cha.iterator();
		while (clsIt.hasNext())
		{
			IClass cls = clsIt.next();
			
			if (cls.getName().toString().equals(rootClassCanonName)) continue;
		
			if (cha.isSubclassOf(cls, rootClass) || cha.implementsInterface(cls, rootClass))
			{
				for (IMethod mth : cls.getDeclaredMethods())
				{
					if ( getShortMethodName(mth).equals(mthName) && mth.getDescriptor().toString().equals(mthDescriptor) ) 
					{
						String clsName = getClassName(cls);
						
						if (progClasses.contains(clsName)) subclasses.add(clsName);
					}
				}
			}
		}
		
		return subclasses;
	}
	
	public static List<String> findSubclasses(String rootClassName, List<String> progClasses) throws Exception
	{
		List<String> subclasses = new ArrayList<String>();
		
		IClass rootClass = findClass(rootClassName);
		
		String rootClassCanonName = rootClass.getName().toString();

		Iterator<IClass> clsIt = cha.iterator();
		while (clsIt.hasNext())
		{
			IClass cls = clsIt.next();
			
			if (cls.getName().toString().equals(rootClassCanonName)) continue;
		
			if (cha.isSubclassOf(cls, rootClass) || cha.implementsInterface(cls, rootClass))
			{
				String clsName = getClassName(cls);
					
				if (progClasses.contains(clsName)) subclasses.add(clsName);
			}		
		}
		
		return subclasses;
	}

	public static int getMethodParamCount(String className, String methodName) throws Exception
	{
		IClass cls = findClass(className);
		
		int paramCount = -1;

		// we check all methods with given name and select the one with the lowest number of formal parameters
		for (IMethod mth : cls.getAllMethods())
		{
			if (getShortMethodName(mth).equals(methodName)) 
			{
				int newParamCount = 0;
				
				// subtract first parameter representing "this" for instance methods
				if (mth.isStatic()) newParamCount = mth.getNumberOfParameters();
				else newParamCount = mth.getNumberOfParameters() - 1;
				
				if ((newParamCount < paramCount) || (paramCount == -1)) paramCount = newParamCount;
			}
		}
		
		if (paramCount == -1) paramCount = 0;

		return paramCount;
	}
	
	public static boolean hasMethodParamReferenceType(String className, String methodName, int paramIndex) throws Exception
	{
		IClass cls = findClass(className);

		for (IMethod mth : cls.getAllMethods())
		{
			if (getShortMethodName(mth).equals(methodName)) 
			{
				TypeReference paramType = null;
				
				paramType = mth.getParameterType(paramIndex);
				
				return paramType.isReferenceType();
			}
		}

		return false;
	}

	public static boolean hasMethodReturnValue(String className, String methodName) throws Exception
	{
		IClass cls = findClass(className);
		
		if (cls == null) return false;

		for (IMethod mth : cls.getAllMethods())
		{
			if (getShortMethodName(mth).equals(methodName))
			{
				if (mth.getReturnType() != TypeReference.Void) return true;
			}
		}

		return false;
	}
	
	public static String printLocalVariableNames(String className, String methodName, int bcIndex, int monitorVarCount) throws Exception
	{
		IClass cls = findClass(className);
		
		StringBuffer strbuf = new StringBuffer();
		
		int mthCount = 0;

		for (IMethod mth : cls.getDeclaredMethods())
		{
			if (getShortMethodName(mth).equals(methodName)) 
			{
				mthCount++;
				
				if (mthCount > 1) strbuf.append("\n");
				
				int paramCount = 0;
				
				// subtract first parameter representing "this" for instance methods
				if (mth.isStatic()) paramCount = mth.getNumberOfParameters();
				else paramCount = mth.getNumberOfParameters() - 1;
				
				for (int i = 0; i < mth.getMaxLocals(); i++)
				{
					String varName = mth.getLocalVariableName(bcIndex, i);
					
					if (varName == null) break;
					
					String prefix = "";
					
					int startIndex = 1;					
					if (mth.isStatic()) startIndex = 0;
			
					if ((i >= startIndex) && (i < startIndex + paramCount)) 
					{
						prefix = "param" + i;
					}
					else if (i >= startIndex + paramCount) 
					{
						if (i - monitorVarCount >= startIndex + paramCount) prefix = "local" + (i - paramCount - monitorVarCount);
						else prefix = "local" + (i - paramCount);
					}
					else prefix = "this";

					strbuf.append(prefix+"=");
						
					strbuf.append(varName);
					strbuf.append(";");
				}				
			}
		}

		return strbuf.toString();
	}
	
	public static Set<String> findPossibleCallersTransitively(String tgtMethodFullName, List<String> programClasses) throws Exception
	{
		Set<String> possibleCallers = new HashSet<String>();

		Stack<String> curCallStack = new Stack<String>();
		
		CGNode rootNode = cg.getFakeRootNode(); 
		
		// we traverse the call graph recursively
		findPossibleCallers(tgtMethodFullName, rootNode, possibleCallers, curCallStack, programClasses);
		
		return possibleCallers;
	}
	
	private static void findPossibleCallers(String tgtMethodFullName, CGNode curNode, Set<String> possibleCallers, Stack<String> curCallStack, List<String> programClasses) throws Exception
	{
		String curMethodName = getFullMethodName(curNode.getMethod());		
		String curClassName = getClassNameFromMethod(curNode.getMethod());
		
		curCallStack.push(curMethodName);
		
		if (curMethodName.equals(tgtMethodFullName))
		{
			// 	add current stack if we reach the target method
			for (Iterator<String> it = curCallStack.iterator(); it.hasNext(); )
			{
				String calledMthFullName = it.next();
				
				int k = calledMthFullName.lastIndexOf('.');
				String calledMthClassName = calledMthFullName.substring(0, k);
				String calledMthShortName = calledMthFullName.substring(k+1);
				
				if ( ! calledMthFullName.startsWith("com.ibm.wala") ) 
				{
					List<String> subclasses = findSubclasses(calledMthClassName, programClasses);
					
					possibleCallers.add(calledMthFullName);
					
					for (String subClsName : subclasses) possibleCallers.add(subClsName + "." + calledMthShortName);						
				}
			}
			
			// backtrack when reaching a call of the target method -> there is no recursive call 
		}
		else if ( ( ! curClassName.startsWith("com.ibm.wala") ) && ( ! programClasses.contains(curClassName) ) )
		{
			// backtrack when a library call was reached
			// ignore fake root method (do nothing here)
		}
		else
		{			
			Iterator<CallSiteReference> callSitesIt = curNode.iterateCallSites();
			while (callSitesIt.hasNext())
			{
				CallSiteReference callSite = callSitesIt.next();
			
				Set<CGNode> successorNodes = cg.getPossibleTargets(curNode, callSite);
				
				for (CGNode succNode : successorNodes) findPossibleCallers(tgtMethodFullName, succNode, possibleCallers, curCallStack, programClasses);
			}
		}
		
		curCallStack.pop();
	}
	
	public static IR getMethodIR(IMethod mth)
	{
		return cache.getIRFactory().makeIR(mth, Everywhere.EVERYWHERE, SSAOptions.defaultOptions());	
	}
}
