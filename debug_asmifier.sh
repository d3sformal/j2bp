#!/bin/sh
java -cp lib/asm-all-3.3.jar:build/examples org.objectweb.asm.util.ASMifierClassVisitor $1
