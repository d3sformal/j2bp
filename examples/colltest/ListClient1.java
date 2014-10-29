package colltest;

import java.util.ArrayList;

import gov.nasa.jpf.jvm.Verify;


public class ListClient1
{
	private ArrayList<Integer> data;

	public ListClient1()
	{
		data = new ArrayList<Integer>();
	}

	public void load1()
	{
		data.add(1);
		data.add(2);
	}

	public void load2()
	{
		data.add(3);
		data.add(4);
	}

	public void process()
	{
		int i = 0;

		boolean b = data.contains(3);
		if (b)
		{
			i = data.get(0);
			if (i != 1) Verify.assertTrue("i != 1", false);
			//System.out.println(o);
		}

		data.remove(0);
	}

	public void log()
	{
		int sz = data.size();

		//System.out.println(sz);
	}

	public static void main(String[] args)
	{
		ListClient1 lc = new ListClient1();

		lc.load1();
		lc.process();
		lc.load2();
		lc.process();
		lc.log();
	}
}
