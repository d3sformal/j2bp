package dillig;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class ListElementsNotAliased
{
	public static void main(String[] args)
	{
		int size = 2;
		
		List<Point2> v = new ArrayList<Point2>();
		
		int i = 0;
		for (i = 0; i < size; i++) v.add(new Point2());
		
		
		Point2 p1;
		Point2 p2;
		
		int j = 0;
		int k = 0;
		
		boolean b;
		
		for (j = 0; j < v.size(); j++)
		{
			p1 = v.get(j);
			
			k = j + 1;
			
			while (k < v.size())
			{
				p2 = v.get(k);
				
				b = false;
				if (p1 == p2) b = true;
				if (b) Verify.assertTrue("p1 == p2", false);
				
				k++;
			}
		}
	}
}


class Point2
{
	private int x;
	private int y;
	

	public Point2()
	{
		x = 0;
		y = 0;
	}
}

