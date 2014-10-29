package dillig;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Collection;

import gov.nasa.jpf.jvm.Verify;


public class MapValuesNonNull
{
	public static void main(String[] args)
	{
		Map<Integer, Point3> m = new HashMap<Integer, Point3>();
		
		m.put(55, new Point3(55, 0));
		m.put(23, new Point3(34, 23));
		m.put(4, new Point3(8, 9));
		m.put(12, new Point3(45, 5));
				
		
		Collection<Point3> coll = m.values();
		
		Iterator<Point3> it = coll.iterator();
	
		Point3 p;
		
		boolean b;
		
		while (it.hasNext())
		{
			p = it.next();
			
			b = false;
			if (p == null) b = true;			
			if (b) Verify.assertTrue("p == null", false);
		}
	}	
}


class Point3
{
	private int x;
	private int y;
	

	public Point3(int _x, int _y)
	{
		this.x = _x;
		this.y = _y;
	}	
}

