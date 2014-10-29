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
