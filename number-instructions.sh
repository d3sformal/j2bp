#!/bin/sh
javap -c $1 | csplit -s - '%public static void main%'+2  /}/ ; nl -v 0 xx00; rm -f xx0?
