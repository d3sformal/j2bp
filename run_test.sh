#!/bin/sh

CP=lib/jpf/build/jpf.jar:lib/jpf/build/jpf-annotations.jar:build/impl:./build/examples:./output

java -Xmx4g -cp "$CP" gov.nasa.jpf.JPF +classpath=.,./output myjava.ArrayListTestEnv
java -Xmx4g -cp "$CP" gov.nasa.jpf.JPF +classpath=.,./output myjava.StringBufferTestEnv

