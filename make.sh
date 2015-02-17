#!/bin/sh

rm -rf build

mkdir build
mkdir -p build/impl
mkdir -p build/examples

IMPL_CP=lib/wala/com.ibm.wala.core_1.1.3.200805291128.jar:lib/wala/com.ibm.wala.shrike_1.0.0.jar:lib/wala/org.eclipse.core.jobs_3.4.100.v20090429-1800.jar:lib/wala/org.eclipse.core.resources_3.5.2.R35x_v20091203-1235.jar:lib/wala/org.eclipse.core.runtime_3.5.0.v20090525.jar:lib/wala/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar:lib/wala/org.eclipse.osgi_3.5.2.R35x_v20100126.jar:lib/asm-all-3.3.jar:lib/jpf/build/jpf.jar

EXAMPLES_CP=lib/jpf/build/jpf.jar:build/impl

# compile implementation (all tools and tests)
javac -cp "$IMPL_CP" -sourcepath ./src -d ./build/impl `find ./src -name *.java | xargs`
scalac -cp "$IMPL_CP" -deprecation -sourcepath ./src -d ./build/impl `find ./src -name *.scala | xargs`

# compile examples
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/myjava -d ./build/examples `find ./examples/myjava -name *.java | xargs`
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/colltest -d ./build/examples `find ./examples/colltest -name *.java | xargs`
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/progtest -d ./build/examples `find ./examples/progtest -name *.java | xargs`
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/dillig -d ./build/examples `find ./examples/dillig -name *.java | xargs`
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/paper-ex -d ./build/examples `find ./examples/paper-ex -name *.java | xargs`
javac -cp "$EXAMPLES_CP" -g -sourcepath ./examples/vutbr -d ./build/examples `find ./examples/vutbr -name *.java | xargs`

