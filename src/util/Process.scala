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
package util

import java.io.InputStream
import java.io.OutputStream

import scala.collection.mutable.ArrayBuffer


object Process 
{
	/** Run the external command cmd, taking input from stdin, sending output and error to stdout and stderr. */
	def runCmd(cmd : String, stdin : OutputStream => Unit = none, stdout : InputStream => Unit = ignore, stderr : InputStream => Unit = ignore) : Int = 
	{
		val cmdline = Array("/bin/bash", "-c", cmd)
		
		val process = Runtime.getRuntime.exec(cmdline)
		
		val outs = process.getOutputStream()
		val ins = process.getInputStream()
		val errs = process.getErrorStream()
		
		val tin = new Thread() 
		{
			override def run() = stdin(outs)
		}
		tin.start()
		
		val tout = new Thread() 
		{
			override def run() = stdout(ins)
		}
		tout.start()
		
		val terr = new Thread() 
		{
			override def run() = stderr(errs)
		}
		terr.start()
		
		val res = process.waitFor()
		
		tin.join()
		tout.join()
		terr.join()
		
		outs.close()
		ins.close()
		errs.close()
		
		return res
	}
	
	/** Execute a process, capturing the return code as an Int, and the stdout and stderr as Strings. */
	def runCmdWithStrOutputs(cmd : String, stdin : OutputStream => Unit = none) : (Int, String, String) = 
	{
		val stdoutbuff = new ArrayBuffer[Byte]()
		val stderrbuff = new ArrayBuffer[Byte]()
		
		val res = runCmd(cmd, stdin, stdoutbuff, stderrbuff)
		
		return (res, new String(stdoutbuff.toArray), new String(stderrbuff.toArray))
	}

	private def processBytes(f : Byte => Unit) : InputStream => Unit = 
	{
		is => {
			def again() : Unit = 
			{
				val ret = is.read()
				if (ret >= 0) 
				{
					f(ret.asInstanceOf[Byte])
					again()
				}
			}
			again()
		}
	}

	/** Capture the stdout/stderr of a process into an ArrayBuffer of bytes. */
	implicit def buffer(ab : ArrayBuffer[Byte]) : InputStream => Unit =
	{
		processBytes(ab += _)
	}
  
	/** Allow a String to be passed as stdin to a process. */
	def string2outstream(s : String) : OutputStream => Unit = 
	{
		os => for (c <- s) os.write(c)
		os.flush()
		os.close()
	}
	
	/** Dump the stout/stderr of a process to stdout. */
	val dump : InputStream => Unit = processBytes(System.out.write(_))

	/** Ignore the stdout/stderr of a process. */
	val ignore : InputStream => Unit = {is => while (is.read >= 0) {} }

	/** Pass an empty input as stdin to a process. */
	val none : OutputStream => Unit = string2outstream("")
}

