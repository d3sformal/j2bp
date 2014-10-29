package dillig;

import java.util.ArrayList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class ListCopy
{
	public static void main(String[] args)
	{
		ArrayList<Integer> l1 = new ArrayList<Integer>();
		
		l1.add(4);
		l1.add(67);

		
		ArrayList<Integer> l2 = new ArrayList<Integer>();
			
		Iterator<Integer> it = l1.iterator();

		int cur = 0;
		
		boolean b1 = it.hasNext();
		while (b1)
		{
			cur = it.next();
			l2.add(cur);
			
			b1 = it.hasNext();
		}

		
		it = l1.iterator();
  
		Iterator<Integer> it2 = l2.iterator();

		int l1_elem = 0;
		int l2_elem = 0;
		
		b1 = it.hasNext();
		while (b1)
		{
			l1_elem = it.next();
			l2_elem = it2.next();
			
			if (l1_elem != l2_elem) Verify.assertTrue("l1_elem != l2_elem", false);

			b1 = it.hasNext();
		}
	}
}

