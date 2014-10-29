#!/bin/sh

CP=lib/wala/dat:lib/wala/com.ibm.wala.core_1.1.3.200805291128.jar:lib/wala/com.ibm.wala.shrike_1.0.0.jar:lib/wala/org.eclipse.core.jobs_3.4.100.v20090429-1800.jar:lib/wala/org.eclipse.core.resources_3.5.2.R35x_v20091203-1235.jar:lib/wala/org.eclipse.core.runtime_3.5.0.v20090525.jar:lib/wala/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar:lib/wala/org.eclipse.osgi_3.5.2.R35x_v20100126.jar:lib/asm-all-3.3.jar:./build/impl

rm -rf output
mkdir output

# these settings are passed to java runtime
JAVA_OPTS="-Xmx4G -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main progtest.ParamTest none build/examples 4 none none none config/Default_Exclusions.txt output > output/gen_progtest_Param.txt 2>&1
JAVA_OPTS="-Xmx4G -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main progtest.ParamRetvalTest none build/examples 4 none none none config/Default_Exclusions.txt output > output/gen_progtest_ParamRetval.txt 2>&1
JAVA_OPTS="-Xmx4G -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main progtest.MyImpl input/MyItfTest_classes build/examples 4 none none none config/Default_Exclusions.txt output > output/gen_progtest_MyImpl.txt 2>&1

# run JPF on the abstract programs
CP_JPF=lib/jpf/build/jpf.jar:lib/jpf/build/jpf-annotations.jar:build/impl
for suf in `cat output/progtest_ParamTest_suffixes`
do
	java -Xmx4g -cp "$CP_JPF" gov.nasa.jpf.JPF +classpath=.,./output +listener=jpf.BooleanErrorTracePrinter +betprinter.include=progtest progtest.ParamTest${suf} > output/jpf_progtest_Param${suf}.txt
done
for suf in `cat output/progtest_ParamRetvalTest_suffixes`
do
	java -Xmx4g -cp "$CP_JPF" gov.nasa.jpf.JPF +classpath=.,./output +listener=jpf.BooleanErrorTracePrinter +betprinter.include=progtest progtest.ParamRetvalTest${suf} > output/jpf_progtest_ParamRetval${suf}.txt
done
for suf in `cat output/progtest_MyImpl_suffixes`
do
	java -Xmx4g -cp "$CP_JPF" gov.nasa.jpf.JPF +classpath=.,./output +listener=jpf.BooleanErrorTracePrinter +betprinter.include=progtest progtest.MyImpl${suf} > output/jpf_progtest_MyImpl${suf}.txt
done

