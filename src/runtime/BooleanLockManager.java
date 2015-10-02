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
package runtime;

import java.util.Map;
import java.util.HashMap;

import gov.nasa.jpf.jvm.Verify;


public class BooleanLockManager
{
	private static Map<String, BooleanLock> varname2lock;
	
	static
	{
		varname2lock = new HashMap<String, BooleanLock>();
	}
	
	
	public static void lock(String varName)
	{
		Verify.beginAtomic();
	
		BooleanLock lockObj = getLockObject(varName);
		
		lockObj.status = true;
		
		Verify.endAtomic();
	}
	
	public static boolean isLocked(String varName)
	{
		boolean status = false;
		
		Verify.beginAtomic();
		
		BooleanLock lockObj = getLockObject(varName);

		status = lockObj.status;
		
		Verify.endAtomic();
		
		return status;
	}
	
	public static void waitForLock(String varName)
	{
		synchronized (varname2lock)
		{
			try
			{
				varname2lock.wait();
			}
			catch (InterruptedException ex) {}
		}
	}
		
	public static void unlock(String varName)
	{
		Verify.beginAtomic();

		BooleanLock lockObj = getLockObject(varName);
		
		lockObj.status = false;
		
		Verify.endAtomic();
		
		synchronized (varname2lock)
		{
			varname2lock.notify();
		}
	}
	
	public static void wait(String varName)
	{
		BooleanLock lockObj = null;
		
		Verify.beginAtomic();
		
		lockObj = getLockObject(varName);
		
		Verify.endAtomic();
		
		synchronized (lockObj)
		{
			try
			{
				lockObj.wait();
			}
			catch (InterruptedException ex) {}
		}
	}
	
	public static void notify(String varName)
	{
		BooleanLock lockObj = null;
		
		Verify.beginAtomic();
		
		lockObj = getLockObject(varName);
		
		Verify.endAtomic();
		
		synchronized (lockObj)
		{
			lockObj.notify();
		}
	}

	
	private static BooleanLock getLockObject(String varName)
	{
		BooleanLock lockObj = varname2lock.get(varName);
		
		if (lockObj == null) 
		{
			lockObj = new BooleanLock(varName);
			
			varname2lock.put(varName, lockObj);
		}
		
		return lockObj;
	}
	
	
	static class BooleanLock
	{
		public String varName;
		public boolean status;
		
		public BooleanLock(String varName)
		{
			this.varName = varName;
			this.status = false;
		}
	}
}
