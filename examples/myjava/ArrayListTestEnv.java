package myjava;


public class ArrayListTestEnv extends test.TestEnvBase
{
	public static void main(String[] args)
	{
		try
		{
			Object obj1 = createObject("myjava.ArrayListPA");
			callMethod(obj1, "add");
			callMethod(obj1, "get");
			callMethod(obj1, "size");
			callMethod(obj1, "remove", false, true);
			callMethod(obj1, "get");
			callMethod(obj1, "size");
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
	}

}
