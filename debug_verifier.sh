#!/bin/sh
java -cp lib/asm-all-3.3.jar:build/impl:output util.BytecodeChecker $1 $2
