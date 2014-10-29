package test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;


public class TestEnvBase
{
	protected static Object createObject(String clsName) throws Exception
	{
		System.out.println("[TEST] creating new object: " + clsName);
		Class cls = Class.forName(clsName);
		Object obj = cls.newInstance();
		printFields(obj, true);
		return obj;
	}

	protected static void callMethod(Object obj, String methodName) throws Exception
	{
		System.out.println("[TEST] calling method: " + methodName);
		Method mth = obj.getClass().getMethod(methodName, new Class[0]);
		Object res = mth.invoke(obj, new Object[0]);
		System.out.println("\t return value = " + res);
		printFields(obj, true);
	}

	protected static void callMethod(Object obj, String methodName, boolean arg1) throws Exception
	{
		System.out.println("[TEST] calling method: " + methodName + "(" + arg1 + ")");
		Method mth = obj.getClass().getMethod(methodName, new Class[]{Boolean.TYPE});
		Object res = mth.invoke(obj, new Object[]{new Boolean(arg1)});
		System.out.println("\t return value = " + res);
		printFields(obj, true);
	}

	protected static void callMethod(Object obj, String methodName, boolean arg1, boolean arg2) throws Exception
	{
		System.out.println("[TEST] calling method: " + methodName + "(" + arg1 + "," + arg2 + ")");
		Method mth = obj.getClass().getMethod(methodName, new Class[]{Boolean.TYPE, Boolean.TYPE});
		Object res = mth.invoke(obj, new Object[]{new Boolean(arg1), new Boolean(arg2)});
		System.out.println("\t return value = " + res);
		printFields(obj, true);
	}

	protected static void callMethod(Object obj, String methodName, boolean arg1, boolean arg2, boolean arg3) throws Exception
	{
		System.out.println("[TEST] calling method: " + methodName + "(" + arg1 + "," + arg2 + "," + arg3 + ")");
		Method mth = obj.getClass().getMethod(methodName, new Class[]{Boolean.TYPE, Boolean.TYPE, Boolean.TYPE});
		Object res = mth.invoke(obj, new Object[]{new Boolean(arg1), new Boolean(arg2), new Boolean(arg3)});
		System.out.println("\t return value = " + res);
		printFields(obj, true);
	}

	protected static void printFields(Object obj, boolean tab) throws Exception
	{
		Field[] objFields = obj.getClass().getDeclaredFields();

		for (Field fld : objFields)
		{
			if (tab) System.out.print("\t ");
			System.out.println(fld.getName() + " = " + fld.getBoolean(obj));
		}		
	}
	
}
