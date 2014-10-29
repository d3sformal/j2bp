package progtest;

import java.util.ArrayList;

import gov.nasa.jpf.jvm.Verify;


public class ParamRetvalTest
{
	private ArrayList<Integer> data;

	public ParamRetvalTest()
	{
		data = new ArrayList<Integer>();
	}

	public void load()
	{
		data.add(1);
		data.add(2);
	}

	public void doSmth1()
	{
		int p = 5;

		int v = data.get(0);

		if (p != v) return;

		int r1 = compute(p);

		int r2 = compute(6);

		if (r1 != 8) Verify.assertTrue("r1 != 8", false);

		if (r2 > r1 + 4) Verify.assertTrue("r2 > r1 + 4", false);
	}

	public int compute(int x)
	{
		if (x < 0) return -1;

		int r = x + 2;

		int sz = data.size();

		if (sz > x) return 0;

		if (r > sz*4) Verify.assertTrue("r > sz*4", false);

		return r;
	}

	public static void main(String[] args)
	{
		ParamRetvalTest prt = new ParamRetvalTest();

		prt.load();

		prt.doSmth1();
	}

}
