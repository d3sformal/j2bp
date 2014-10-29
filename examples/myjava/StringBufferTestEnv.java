package myjava;


public class StringBufferTestEnv extends test.TestEnvBase
{
	public static void main(String[] args)
	{
		try
		{
			Object obj1 = createObject("myjava.StringBufferPA");
			callMethod(obj1, "append");
			callMethod(obj1, "length");
			callMethod(obj1, "charAt", true, false);
			callMethod(obj1, "append");
			callMethod(obj1, "length");
			callMethod(obj1, "setLength", true, true);
			callMethod(obj1, "insert", false, false);
			callMethod(obj1, "length");
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
	}

}
