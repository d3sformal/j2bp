package progtest;

import java.util.ArrayList;

import gov.nasa.jpf.jvm.Verify;


public class ParamTest
{
	private ArrayList<Integer> data;

	public ParamTest()
	{
		data = new ArrayList<Integer>();
	}

	public void doSmth1()
	{
		int p = 5;

		if (p != 3) return;

		compute(p);

		compute(6);
	}

	public void compute(int x)
	{
		if (x < 0) Verify.assertTrue("x < 0", false);
	}

	public static void main(String[] args)
	{
		ParamTest pt = new ParamTest();

		pt.doSmth1();
	}

}
