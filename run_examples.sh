#!/bin/sh

CP=lib/wala/dat:lib/wala/com.ibm.wala.core_1.1.3.200805291128.jar:lib/wala/com.ibm.wala.shrike_1.0.0.jar:lib/wala/org.eclipse.core.jobs_3.4.100.v20090429-1800.jar:lib/wala/org.eclipse.core.resources_3.5.2.R35x_v20091203-1235.jar:lib/wala/org.eclipse.core.runtime_3.5.0.v20090525.jar:lib/wala/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar:lib/wala/org.eclipse.osgi_3.5.2.R35x_v20100126.jar:lib/asm-all-3.3.jar:./build/impl

rm -rf output
mkdir output

# these options are passed to java runtime
JAVA_OPTS="-Xmx4g -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main myjava.ArrayList input/ArrayList_classes build/examples 4 none none none config/Default_Exclusions.txt output > output/vystup_array.txt 2>&1
JAVA_OPTS="-Xmx4g -DWALA_HOME=." TOOL_CLASSPATH="-cp lib/wala/dat" scala -cp "$CP" j2bp.Main myjava.StringBuffer input/StringBuffer_classes build/examples 4 none none none config/Default_Exclusions.txt output > output/vystup_strbuf.txt 2>&1

