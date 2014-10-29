package dillig;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class ListOfKeyValuePairs
{
	public static void main(String[] args)
	{
		Map<Integer, Integer> m = new HashMap<Integer, Integer>();
		
		m.put(45,8);
		m.put(23,19);
		
	
		List<Pair> kv_pairs = new ArrayList<Pair>();

		Set<Integer> st = m.keySet();
		Iterator<Integer> it = st.iterator();
		
		int key = 0;
		int val = 0;
		Pair p;
		
		while (it.hasNext())
		{
			key = it.next();
			val = m.get(key);
			
			p = new Pair();
			p.first = key;
			p.second = val;
			
			kv_pairs.add(0, p);
		}


		Iterator<Pair> it2 = kv_pairs.iterator();
		
		int k = 0;
		int v = 0;
		int v2 = 0;
		
		while (it2.hasNext())
		{
			p = it2.next();
			
			k = p.first;
			v = p.second;
			
			v2 = m.get(k);			
			if ( v2 != v ) Verify.assertTrue("m.get(k) != v", false);
		}
	}
	
	public static int unknown()
	{
		return 1;
	}
}

class Pair
{
	public int first;
	public int second;
}
