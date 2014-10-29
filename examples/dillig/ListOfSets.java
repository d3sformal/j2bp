package dillig;

import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class ListOfSets
{
	public static void main(String[] args)
	{
		List<Set<Integer>> v = new ArrayList<Set<Integer>>();
		
		Set<Integer> s1 = new HashSet<Integer>();
		
		s1.add(9);
		s1.add(11);

		Set<Integer> s2 = new HashSet<Integer>();
		
		s2.add(11);
		s2.add(23);

		v.add(s1);
		v.add(s2);
		v.add(s2);

		
		int sz = v.size();
		
		if (sz != 3) Verify.assertTrue("v.size != 3", false);

		Set<Integer> st1 = v.get(0);
		Set<Integer> st2 = v.get(1);
		Set<Integer> st3 = v.get(2);
		
		boolean b;
		
		b = st1.contains(9);		
		if ( ! b ) Verify.assertTrue("not v.get(0).contains(9)", false);

		st3.remove(11);
		
		b = st2.contains(11);		
		if ( b ) Verify.assertTrue("v.get(1).contains(11)", false);
	}
}

