package dillig;

import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class MapCopy
{
	public static void main(String[] args)
	{
		HashMap<Integer,Integer> m = new HashMap<Integer,Integer>();

		m.put(1,10);
		m.put(2,20);

		
		HashMap<Integer,Integer> m2 = new HashMap<Integer,Integer>();

		int i1 = 0;
		int i2 = 0;
		int i3 = 0;

		Set<Integer> st = m.keySet();
		Iterator<Integer> it = st.iterator();

		boolean b1 = it.hasNext();
		while (b1)
		{
			i1 = it.next();
			i2 = m.get(i1);

			m2.put(i1, i2);

			b1 = it.hasNext();
		}

		st = m.keySet();
		it = st.iterator();

		boolean b2 = it.hasNext();
		while (b2)
		{
			i1 = it.next();

			i2 = m.get(i1);
			i3 = m2.get(i1);

			if (i2 != i3) Verify.assertTrue("i2 != i3", false);

			b2 = it.hasNext();
		}
	}
}

