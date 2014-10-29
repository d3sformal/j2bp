package dillig;

import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class SetOfMapKeys
{
	public static void main(String[] args)
	{
		HashMap<Integer,Integer> m = new HashMap<Integer,Integer>();
		
		m.put(1,8);
		m.put(2,19);
		m.put(3,98);

		
		HashSet<Integer> keys = new HashSet<Integer>();

		Set<Integer> st = m.keySet();
		Iterator<Integer> it = st.iterator();

		int i1 = 0;
		
		boolean b1 = it.hasNext();
		while (b1)
		{
			i1 = it.next();
		
			keys.add(i1);

			b1 = it.hasNext();
		}
		
		
		boolean b2 = keys.contains(1);
		if ( ! b2 ) Verify.assertTrue("not keys.contains(1)", false);
	}
}

