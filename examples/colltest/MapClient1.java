package colltest;

import java.util.HashMap;

import gov.nasa.jpf.jvm.Verify;


public class MapClient1
{
	private HashMap<Integer,Integer> data;

	public MapClient1()
	{
		data = new HashMap<Integer,Integer>();
	}

	public void load1()
	{
		data.put(2,10);
		data.put(3,20);
	}

	public void load2()
	{
		data.put(5,30);
		data.put(7,40);
	}

	public void process()
	{
		int i = 0;

		boolean b1 = data.containsKey(3);
		if (b1)
		{
			i = data.get(2);
			if (i != 10) Verify.assertTrue("i != 10", false);
		}

		data.remove(2);

		boolean b2 = data.containsValue(7);
		if (b2)
		{
			i = data.get(5);
			if (i <= 25) Verify.assertTrue("i <= 25", false);
		}

		data.remove(5);
	}

	public void log()
	{
		int sz = data.size();

		//System.out.println(sz);
	}

	public static void main(String[] args)
	{
		MapClient1 mc = new MapClient1();

		mc.load1();
		mc.process();
		mc.load2();
		mc.log();
		mc.process();
		mc.log();
	}
}
