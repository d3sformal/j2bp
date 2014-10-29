package dillig;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class MapOfLists
{
	public static void main(String[] args)
	{
		HashMap<Integer,List<Integer>> m = new HashMap<Integer,List<Integer>>();

		List<Integer> l1 = new LinkedList<Integer>();
		l1.add(3);
		l1.add(0, 7);

		List<Integer> l2 = new LinkedList<Integer>();
		l2.add(34);
		l2.add(0, 23);
		l2.add(0, 2);
		
		List<Integer> l3 = new LinkedList<Integer>();

		m.put(1,l1);
		m.put(2,l2);
		m.put(3,l3);
		
		List<Integer> l;

		l = m.get(2);		
		l.remove(0);
		
		int pos = 0;

		int sz1 = l2.size();
		if (sz1 != 2) Verify.assertTrue("l2.size != 2", false);
		
		int sz2 = 0;

		l = m.get(1);
		Iterator<Integer> it = l.iterator();
		int i1 = it.next();
		if (i1 != 7) Verify.assertTrue("i1 != 7", false);
	}
}

