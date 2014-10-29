package dillig;

import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class Multimap
{
	public static void main(String[] args)
	{
		HashMap<Integer, Set<Integer>> m = new HashMap<Integer,Set<Integer>>();
		
		m.put(1, new HashSet<Integer>());
		m.put(2, new HashSet<Integer>());
		m.put(3, new HashSet<Integer>());
		

		Set<Integer> s1 = m.get(1);
		Set<Integer> s2 = m.get(2);
		Set<Integer> s3 = m.get(3);

		s1.add(1);
		s3.add(2);
		s2.add(3);
		s2.add(4);
		s1.add(5);
		s2.add(6);

		
		int sz = s2.size();
		if (sz != 3) Verify.assertTrue("m.get(2).size() != 3", false);

		
		boolean b = s3.contains(2);
		if ( ! b ) Verify.assertTrue("not m.get(3).contains(2)", false);
	}
}

