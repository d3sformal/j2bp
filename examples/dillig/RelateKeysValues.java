package dillig;

import java.util.HashMap;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class RelateKeysValues
{
	public static void bar(int s)
	{
		HashMap<Integer,Integer> m = new HashMap<Integer,Integer>();
		
		m.put(1,3);
		m.put(2,9);
		m.put(3,34);
		m.put(s,56);
		
		m.remove(3);

		int i;
		
		i = m.get(1);
		if (s != 1)
		{
			if (i != 3) Verify.assertTrue("m.get(1) != 3", false);
		}
		else
		{
			if (i != 56) Verify.assertTrue("m.get(1) != 56", false);
		}
	}
	
	public static void main(String[] args)
	{
		bar(8);
		bar(2);
	}		
	
}

