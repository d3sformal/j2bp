#!/bin/sh

CP=lib/wala/dat:lib/wala/com.ibm.wala.core_1.1.3.200805291128.jar:lib/wala/com.ibm.wala.shrike_1.0.0.jar:lib/wala/org.eclipse.core.jobs_3.4.100.v20090429-1800.jar:lib/wala/org.eclipse.core.resources_3.5.2.R35x_v20091203-1235.jar:lib/wala/org.eclipse.core.runtime_3.5.0.v20090525.jar:lib/wala/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar:lib/wala/org.eclipse.osgi_3.5.2.R35x_v20100126.jar:lib/asm-all-3.3.jar:./build/impl

rm -rf output
mkdir output

CP_JPF=lib/jpf/build/jpf.jar:lib/jpf/build/jpf-annotations.jar:build/impl

for progname in Scheduler Image 
do
	# these settings are passed to java runtime
	JAVA_OPTS="-Xmx4G -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main paper.${progname} input/paper-ex/${progname}_classes build/examples 4 input/paper-ex/${progname}_predicates input/paper-ex/${progname}_properties none config/Default_Exclusions.txt output > output/gen_paper_${progname}.txt 2>&1

	# run JPF on the abstract programs
	for suf in `cat output/paper_${progname}_suffixes`
	do
		java -Xmx4g -cp "$CP_JPF" gov.nasa.jpf.JPF +classpath=.,./output +listener=jpf.BooleanErrorTracePrinter +betprinter.include=paper paper.${progname}${suf} > output/jpf_paper_${progname}${suf}.txt 2>&1
	done
done

# with forced backtracking
for progname in DataFlowAnalysis CyclingRace 
do
	# these settings are passed to java runtime
	JAVA_OPTS="-Xmx4G -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main paper.${progname} input/paper-ex/${progname}_classes build/examples 4 input/paper-ex/${progname}_predicates input/paper-ex/${progname}_properties input/paper-ex/${progname}_settings config/Default_Exclusions.txt output > output/gen_paper_${progname}.txt 2>&1

	# run JPF on the abstract programs
	for suf in `cat output/paper_${progname}_suffixes`
	do
		java -Xmx4g -cp "$CP_JPF" gov.nasa.jpf.JPF +classpath=.,./output +listener=jpf.BooleanErrorTracePrinter +betprinter.include=paper paper.${progname}${suf} > output/jpf_paper_${progname}${suf}.txt 2>&1
	done
done

