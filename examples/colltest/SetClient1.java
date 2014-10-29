package colltest;

import java.util.TreeSet;

import gov.nasa.jpf.jvm.Verify;


public class SetClient1
{
	private TreeSet<Integer> data;

	public SetClient1()
	{
		data = new TreeSet<Integer>();
	}

	public void load1()
	{
		data.add(2);
		data.add(3);
	}

	public void load2()
	{
		data.add(5);
		data.add(7);
	}

	public void process()
	{
		boolean b1 = data.contains(3);

		//if (b1) System.out.println("3:yes");
		//else System.out.println("3:no");

		data.remove(3);

		boolean b2 = data.contains(5);

		if (!b2) Verify.assertTrue("!b2", false);

		//if (b2) System.out.println("5:yes");
		//else System.out.println("5:no");

		data.remove(3);
	}

	public void log()
	{
		int sz = data.size();

		//System.out.println(sz);
	}

	public static void main(String[] args)
	{
		SetClient1 sc = new SetClient1();

		sc.load1();
		sc.process();
		sc.log();
		sc.load2();
		sc.process();
		sc.log();
	}
}
